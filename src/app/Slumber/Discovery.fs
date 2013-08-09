namespace Slumber

open System
open Slumber.Common.Http
open Slumber.Common.Attempt
open Slumber.Configuration
open Slumber.Execution
open Slumber.Framework.Helpers

///Contains functions and types for identifying the binding, content types and user of a request
module Discovery = 

    ///Represents the arguments used to execute the discovery phase
    type DiscoveryArgs = {
        Request : Request;
        Container : Container;
    }

    ///Contains functions for matching a request URI and verb to an operation
    module Matching = 
        
        open Slumber.Common.AsyncAttempt
        open Slumber.Configuration.Endpoints

        ///Represents basic information about a matched endpoint
        type EndpointInfo = {
            Endpoint : Endpoint;
            Parameters : (String * String) list;
        }

        ///Represents basic information about a matched binding
        type BindingInfo = {
            Binding : Binding;
            Parameters : (String * String) list;
        }     

        ///Attempts to match a request URI to the given endpoint and returns the matching URI variables
        let private getTemplateVariables args (endpoint : Endpoint) = 

            let template = UriTemplate (endpoint.Template, true)            
            let results = template.Match (args.Container.BaseUrl, args.Request.Url.Raw)

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
                                | Some parameters -> 
                                    {
                                        Endpoint = endpoint;
                                        Parameters = parameters;
                                    }
                                    |> Some

                                | _ -> 
                                    None
                            )    
                        |> successOr StatusCodes.NotFound  
                }

        ///Find the binding for the request verb
        let asyncMatchBinding (info : EndpointInfo) = 
            fun args -> 
                async {

                    logInfo "[%A] Resolving binding for %A" args.Request.Id args.Request.Verb

                    let createBindingInfo binding = 
                        match binding with
                        | Some binding' -> 
                            Some {
                                Binding = binding';
                                Parameters = info.Parameters;
                            }
                        | _ -> None

                    return
                        info.Endpoint
                        |> tryGetBinding args.Request.Verb
                        |> createBindingInfo
                        |> successOr StatusCodes.MethodNotAllowed
                }

        ///Asynchronously gets the binding matching the args, or an error flag
        let asyncGetBinding =             
            asyncAttempt {

                let! endpoint = asyncMatchEndpoint ()
                let! binding = asyncMatchBinding endpoint

                return binding
            }

    ///Contains functions for applying securtiy to requests
    module Security = 

        ///Attempts to authenticate the current user, if applicable for the selected binding
        let asyncAuthenticateRequest (binding : Matching.BindingInfo) (args : DiscoveryArgs) =
            async {
                
                logInfo "[%A] Authenticating request" args.Request.Id

                return
                    if binding.Binding.IsPublic then

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

        open Slumber.Common.Http.Requests
        open Slumber.Configuration.Containers

        ///The default content type to be used if the Content-Type of Accept headers are omitted
        let [<Literal>] DefaultMediaType = MediaTypes.Text.Xml

        ///The 'any' content type
        let [<Literal>] AnyContentType = "*/*"

        ///Gets the reader to be used to deserialise any request body
        let asyncGetReader args = 
            async {

                logInfo "[%A] Negotiating request content type" args.Request.Id

                let requestedContentType = 
                    match (Headers.getContentType args.Request.Payload) with
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
                    match (Headers.getAccept args.Request.Payload) with
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

        let onAuthenticationSuccess (info : Matching.BindingInfo) userData = 
            async {

                let! reader = Negotiation.asyncGetReader args
                let! writer = Negotiation.asyncGetWriter args

                match (info.Binding.MessageType, reader) with
                | (_, None) -> 
                    return stopWithStatus StatusCodes.ContentTypeNotSupported
                | (messageType, Some reader') ->                   
                    return
                        {
                            Request = args.Request;
                            Reader = (getReaderInfo reader' messageType);
                            Writer = (getWriterInfo writer);
                            Parameters = info.Parameters;
                            Operation = info.Binding.Operation;
                            User = userData;
                        }
                        |> continue'
            }
            
        let onBindingSuccess (info : Matching.BindingInfo) args =
            async {

                let! authResult = Security.asyncAuthenticateRequest info args

                match authResult with
                | Failure statusCode -> return stopWithStatus statusCode
                | Success userData -> return! onAuthenticationSuccess info userData
            }

        async {

            logInfo "[%A] Beginning discovery phase for %A %A" args.Request.Id args.Request.Verb args.Request.Url.Path
            
            let! bindingResult = Matching.asyncGetBinding args            

            match bindingResult with
            | Failure statusCode -> return stopWithStatus statusCode
            | Success info -> return! onBindingSuccess info args
        }
        
    ///Runs the discovery phase synchronously
    let run = 
        asyncRun >> Async.RunSynchronously
        