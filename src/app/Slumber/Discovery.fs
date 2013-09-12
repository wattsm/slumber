namespace Slumber

open System
open Slumber.Common.Attempt
open Slumber.Execution
open Slumber.Framework
open Slumber.Framework.Helpers

///Contains functions and types for identifying the binding, content types and user of a request
module Discovery = 

    ///Represents the arguments used to execute the discovery phase
    type DiscoveryArgs = {
        Request : Request;
        Container : Container;
    }

    ///Represents the result of matching an HTTP request against a binding
    type MatchingResult = {
        Binding : Binding;
        EndpointName : String;
        Parameters : (String * String) list;
    }
    with

        ///The empty matching result
        static member Empty = 
            {
                Binding = Binding.Empty;
                EndpointName = String.Empty;
                Parameters = [];
            }

    ///Contains functions for matching a request URI and verb to an operation
    module Matching = 
        
        open System.IO
        open Slumber.Common.AsyncAttempt
        open Slumber.Framework.Core.Endpoints

        ///Normalises a URL for template matching, ensuring folder-level URLs end with a trailing slash.
        ///This is important for cases when the base URL and candidate URL are essentially equal - e.g.
        //http://localhost/api/ and http://localhost/api.
        let normaliseUrl (url : Uri) = 
            if (url.AbsolutePath.EndsWith "/") then
                url
            else
                Uri (
                    String.Format (
                        "{0}{1}/{2}",
                        url.GetLeftPart (UriPartial.Authority),
                        url.AbsolutePath,
                        url.Query
                    ),
                    UriKind.Absolute
                )

        ///Attempts to match a request URI to the given endpoint and returns the matching URI variables
        let private getTemplateVariables args (endpoint : Endpoint) = 
        
            let url = normaliseUrl args.Request.Url.Raw
            let template = UriTemplate (endpoint.Template, true)            
            let results = template.Match (args.Container.BaseUrl, url)

            if results = null then
                None
            else
                results.BoundVariables
                |> NameValueCollection.toList
                |> Some

        ///Finds the endpoint bound to the request URI
        let asyncMatchEndpoint () = 
            fun args ->
                async { 

                    logInfo "[%A] Resolving endpoint for %A" args.Request.Id args.Request.Url.Path

                    return
                        args.Container.Endpoints
                        |> List.tryPick (fun endpoint ->
                                match (getTemplateVariables args endpoint) with
                                | Some parameters -> Some (endpoint, parameters)
                                | _ -> None
                            )    
                        |> successOr StatusCodes.NotFound  
                }

        ///Find the binding for the request verb
        let asyncMatchBinding (endpoint, parameters) = 
            fun args -> 
                async {

                    logInfo "[%A] Resolving binding for %A" args.Request.Id args.Request.Verb

                    let createBindingInfo binding = 
                        match binding with
                        | Some binding' ->                             
                            {
                                MatchingResult.Empty 
                                with
                                    Binding = binding';
                                    EndpointName = endpoint.Name;
                                    Parameters = parameters;
                            }
                            |> Some
                        | _ -> None

                    return
                        endpoint
                        |> tryGetBinding args.Request.Verb
                        |> createBindingInfo
                        |> successOr StatusCodes.MethodNotAllowed
                }

        ///Asynchronously gets the result of matching the current request to an endpoint
        let asyncGetMatchingResult =             
            asyncAttempt {

                let! endpoint, parameters = asyncMatchEndpoint ()
                let! result = asyncMatchBinding (endpoint, parameters)

                return result
            }

    ///Contains functions for applying securtiy to requests
    module Security = 

        ///Attempts to authenticate the current user, if applicable for the selected binding
        let asyncAuthenticateRequest (result : MatchingResult) (args : DiscoveryArgs) =
            async {
                
                logInfo "[%A] Authenticating request" args.Request.Id

                return
                    if result.Binding.IsPublic then

                        logInfo "[%A] Binding is public" args.Request.Id

                        Success (None)
                    else
                        match args.Container.SecurityMode with
                        | Public ->

                            logWarn "[%A] Binding is private but container is public" args.Request.Id

                            Success (None)
                        | Private auth ->
                            match (auth args.Request) with
                            | Allow userData ->

                                logInfo "[%A] Request was successfully authenticated" args.Request.Id
                            
                                Success (userData)
                            | _ ->

                                logInfo "[%A] Authentication failed for request" args.Request.Id

                                Failure StatusCodes.Unauthorised
            }
            
    ///Contains functions for negotiating content types based on the Content-Type and Accept header
    module Negotiation = 

        open Slumber.Framework.Core.Containers

        ///The default content type to be used if the Content-Type of Accept headers are omitted
        let [<Literal>] DefaultMediaType = MediaTypes.Text.Xml

        ///The 'any' content type
        let [<Literal>] AnyContentType = "*/*"

        ///Gets the reader to be used to deserialise any request body
        let asyncGetReader args = 
            async {

                logInfo "[%A] Negotiating request content type" args.Request.Id

                let requestedContentType = 
                    match (Headers.getContentType args.Request.Payload.Headers) with
                    | Some contentType -> contentType
                    | _ -> DefaultMediaType

                let targetContentType = 
                    args.Container
                    |> applyForwarding requestedContentType

                if (targetContentType <> requestedContentType) then
                    logInfo "[%A] Request content type forwarding from %A to %A" args.Request.Id requestedContentType targetContentType

                let reader = 
                    args.Container
                    |> getReader targetContentType

                if (Option.isSome reader) then
                    logInfo "[%A] Selected request content type of %A" args.Request.Id targetContentType

                return
                    match reader with
                    | Some reader' -> Some (targetContentType, reader')
                    | _ -> None
            }

        ///Gets the writer to be used when serialising any response body
        let asyncGetWriter args =
            async {

                logInfo "[%A] Negotiating response content type" args.Request.Id

                let requestedContentType = 
                    match (Headers.getAccept args.Request.Payload.Headers) with
                    | Some contentType when (contentType <> AnyContentType) -> contentType
                    | _ -> DefaultMediaType

                let writer = 
                    requestedContentType
                    |> String.split ","
                    |> List.map String.trim
                    |> List.tryPick (fun contentType ->

                            let targetContentType = 
                                args.Container
                                |> applyForwarding contentType

                            if (targetContentType <> contentType) then
                                logInfo "[%A] Response content type forwarding from %A to %A" args.Request.Id contentType targetContentType

                            match (getWriter targetContentType args.Container) with
                            | Some writer -> Some (targetContentType, writer)
                            | _ -> None
                        )

                match writer with
                | Some (contentType, _) ->
                    logInfo "[%A] Selected response content type of %A" args.Request.Id contentType
                | _ -> ()

                return  writer
            }

    ///Runs the discovery phase asynchronously
    let asyncRun args = 

        let getWriterInfo result =
            match result with
            | None -> None
            | Some (contentType, writer) -> 
                {
                    Writer = writer;
                    ContentType = contentType;
                }
                |> Some

        let getReaderInfo (contentType, reader) messageType = 
            match messageType with
            | None -> None
            | Some type' ->
                {   
                    Reader = reader;
                    ContentType = contentType;
                    MessageType = type';
                }
                |> Some

        let onAuthenticationSuccess (result : MatchingResult) userData = 
            async {

                let! reader = Negotiation.asyncGetReader args
                let! writer = Negotiation.asyncGetWriter args

                match (result.Binding.MessageType, reader) with
                | (_, None) -> 
                    return stopWithStatus StatusCodes.ContentTypeNotSupported
                | (messageType, Some reader') ->                   
                    return
                        {
                            Request = args.Request;
                            Reader = (getReaderInfo reader' messageType);
                            Writer = (getWriterInfo writer);
                            Target = 
                                {
                                    EndpointName = result.EndpointName;
                                    Operation = result.Binding.Operation;
                                    Parameters = result.Parameters;
                                }
                            User = userData;
                        }
                        |> continue'
            }
            
        let onMatchingSuccess (result : MatchingResult) args =
            async {

                let! authResult = Security.asyncAuthenticateRequest result args

                match authResult with
                | Failure statusCode -> return stopWithStatus statusCode
                | Success userData -> return! onAuthenticationSuccess result userData
            }

        async {

            logInfo "[%A] Beginning discovery phase for %A %A" args.Request.Id args.Request.Verb args.Request.Url.Path
            
            let! matchingResult = Matching.asyncGetMatchingResult args            

            match matchingResult with
            | Failure statusCode -> return stopWithStatus statusCode
            | Success info -> return! onMatchingSuccess info args
        }
        
    ///Runs the discovery phase synchronously
    let run = 
        asyncRun >> Async.RunSynchronously
        