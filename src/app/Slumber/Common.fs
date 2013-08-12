namespace Slumber

open System

///Contains common modules, types and functions used by Slumber
[<AutoOpen>]
module Common =

    ///Gets the ID of the executing thread
    let getThreadId () = 
        System.Threading.Thread.CurrentThread.ManagedThreadId

    ///Describes possible outcomes of an operation
    type Outcome<'TSuccess, 'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure

    ///Converts an optional value to a Success or Failure value with the appropriate error
    let successOr error result = 
        match result with 
        | Some data -> Success data
        | _ -> Failure error   

    ///Contains modules for working with HTTP
    module Http = 

        open System.IO
        open System.Web
        open System.Collections.Specialized

        ///Contains constants for HTTP status codes
        module StatusCodes = 
        
            let [<Literal>] Ok = 200
            let [<Literal>] BadRequest = 400
            let [<Literal>] Unauthorised = 401
            let [<Literal>] NotFound = 404
            let [<Literal>] MethodNotAllowed = 405
            let [<Literal>] NotAcceptable = 406
            let [<Literal>] ContentTypeNotSupported = 415
            let [<Literal>] InternalServerError = 500            
            let [<Literal>] NotImplemented = 501            

        ///Contains constans for HTTP verbs
        [<RequireQualifiedAccess>]
        module Verbs = 

            let [<Literal>] Get = "GET"
            let [<Literal>] Post = "POST"
            let [<Literal>] Put = "PUT"
            let [<Literal>] Delete = "DELETE"
            let [<Literal>] Options = "OPTIONS"
            let [<Literal>] Head = "HEAD"
            let [<Literal>] Patch = "PATCH"

        ///Contains constants for common media types
        module MediaTypes = 

            [<RequireQualifiedAccess>]
            module Text = 
                let [<Literal>] Xml = "text/xml"
                let [<Literal>] Plain = "text/plain"
                let [<Literal>] Html = "text/html"

            [<RequireQualifiedAccess>]
            module Application = 
                let [<Literal>] Json = "application/json"
            

        ///Represents various forms of a request URL
        type Urls = {
            Raw : Uri;
            Path : String;
            Query : (String * String) list;
            BaseUrl : Uri;
        } 
        with

            ///The empty URL collection
            static member Empty = 

                let uri = Uri ("http://localhost/", UriKind.Absolute)

                {
                    Raw = uri;
                    Path = "/";
                    Query = [];
                    BaseUrl = uri;
                }

        ///Represents an HTTP request or response payload
        type Payload = {
            Headers : (String * String) list;
            Body : Stream option;
        }
        with

            ///The empty HTTP payload
            static member Empty = 
                {
                    Headers = [];
                    Body = None;
                }


        ///Represents a basic HTTP request
        type Request = {
            Id : Guid;
            Url : Urls;
            Verb : String;
            Payload : Payload;
        }
        with

            ///The empty HTTP request
            static member Empty = 
                {
                    Id = Guid.Empty;
                    Url = Urls.Empty;
                    Verb = String.Empty;
                    Payload = Payload.Empty;
                }

        ///Contains functions for working with requests
        module Requests = 

            ///True if a request contains a body
            let hasBody request = 
                match request.Payload.Body with
                | None -> false
                | _ -> true

        ///Describes possible response types
        type ResponseType =
            | StatusCode of Int32
            | Resource of (Int32 * Byte list)

        ///Describes an HTTP response
        type Response = {
            ResponseType : ResponseType;
            ContentType : String option;
            CustomHeaders : (String * String) list;
        }
        with

            ///The empty response
            static member Empty =
                {
                    ResponseType = (StatusCode StatusCodes.NotImplemented);
                    ContentType = None;
                    CustomHeaders = [];
                }

        ///Parses URL information from an HTTP request
        let parseUrls (raw : HttpRequestBase) = 

            let root = 
                Uri (raw.Url.GetLeftPart (UriPartial.Authority), UriKind.Absolute)

            {
                Raw = raw.Url;                
                Path = raw.Url.AbsolutePath.Substring (raw.ApplicationPath.Length);
                Query = raw.QueryString |> NameValueCollection.toList;
                BaseUrl = Uri (root, raw.ApplicationPath);
            }

        ///Parses the payload of an HTTP request
        let parsePayload (raw : HttpRequestBase) = 

            let body = 
                if (raw.InputStream.Length > 0L) then
                    Some raw.InputStream
                else    
                    None

            {
                Headers = raw.Headers |> NameValueCollection.toList;
                Body = body;
            }

        ///Parses an HTTP request
        let parseRequest (raw : HttpRequestBase) requestId = 
            {
                Id = requestId;
                Url = (parseUrls raw);
                Verb = raw.HttpMethod;
                Payload = (parsePayload raw);
            }

        ///Creates an absolute URL from a base URL and a relative URL
        let createAbsoluteUri (baseUrl : Uri) (relativeUrl : String) = 

            //NOTE That this function adds/removes slashes in the base and relative URLs. This is to 
            //maintain expected behaviour when working with extensionless URLs. By default, for example
            //http://localhost/app + api will result in http://localhost/api whereas the expected result is
            //likely http://localhost/app/api.

            let relativeUrl' = 
                if (relativeUrl.StartsWith "/") then
                    relativeUrl.Substring 1
                else
                    relativeUrl

            let baseUrl' = 
                if not (baseUrl.AbsoluteUri.EndsWith "/") then
                    Uri (baseUrl.AbsoluteUri + "/", UriKind.Absolute)
                else
                    baseUrl

            Uri (baseUrl', relativeUrl')

        ///Contains functions for working with HTTP headers
        module Headers = 

            let [<Literal>] ContentType = "Content-Type"
            let [<Literal>] Accept = "Accept"
            let [<Literal>] ContentLength = "Content-Length"

            //TODO Consider changing naming convention to getX and getPayloadX

            ///Picks the value of a header with a given key from a key/value list
            let pickHeaderValue key = 
                List.tryPick (fun (key', value) ->
                    if (String.same key key') then
                        Some value
                    else
                        None
                )

            ///Gets the value of a header from an HTTP payload
            let getHeaderValue key (payload : Payload) = 
                payload.Headers
                |> pickHeaderValue key

            ///Picks a non-empty header value from a key/value list
            let pickNonEmptyHeaderValue key headers = 
                match (pickHeaderValue key headers) with
                | Some value when not (String.IsNullOrWhiteSpace value) -> Some value
                | _ -> None

            ///Gets a header value, counting empty values as not being present
            let getNonEmptyHeaderValue key payload = 
                payload.Headers
                |> pickNonEmptyHeaderValue key

            ///Gets the value of the Content-Type header from a key/value list
            let pickContentType =
                pickNonEmptyHeaderValue ContentType

            ///Gets the value of the Content-Type header
            let getContentType = 
                getNonEmptyHeaderValue ContentType

            ///Gets the value of the Accept header from a key/value list
            let pickAccept = 
                getNonEmptyHeaderValue Accept

            ///Gets the value of the Accept header
            let getAccept =
                getNonEmptyHeaderValue Accept

    ///Record describing basic user properties
    type UserData = {
        Id : String;
        Properties : (String * String) list;
    }

    ///Contains functions for working with operations
    [<AutoOpen>]
    module Operations = 

        open Http

        ///Represents metadata about an operation request
        type OperationMetadata = {
            RequestId : Guid;
            Url : Urls;
            Parameters : (String * String) list;
            User : UserData option;
        }
        with

            ///Empty operation metadata
            static member Empty = 
                {
                    RequestId = Guid.Empty;
                    Url = Urls.Empty;
                    Parameters = [];
                    User = None;
                }

        ///Useful functions for working with metadata
        module Metadata = 

            ///Union describing the result of trying to get some metadata
            type TryGetResult<'TFound> = 
                | Found of 'TFound
                | Missing
                | Malformed

            let private pickValue key =
                fun (key', value) ->
                    if (String.same key key') then
                        Some value
                    else
                        None

            ///Gets the parameters from metadata
            let getParameters meta = 
                meta.Parameters

            ///Gets the value of a parameter
            let getParameter key = 
                getParameters
                >> List.pick (pickValue key)

            ///Tries to get the value of a parameter
            let tryGetParameter key = 
                getParameters
                >> List.tryPick (pickValue key)

            ///Gets a parameter as the given type
            let getParameterAs<'TResult> key meta = 
                
                let value = 
                    getParameter key meta

                Convert.ChangeType (value, typeof<'TResult>) :?> 'TResult

            ///Tries to get a parameter as a given type
            let tryGetParameterAs<'TResult> key meta = 
                match (tryGetParameter key meta) with
                | None -> Missing
                | Some value ->
                    try
                        Found (Convert.ChangeType (value, typeof<'TResult>) :?> 'TResult)
                    with
                    | _ -> Malformed

            ///Gets a parameter using the given conversion
            let getParameterUsing key (conversion : String -> 'TResult) =                 
                getParameter key
                >> conversion

            ///Tries to gets a parameter using a given conversion
            let tryGetParameterUsing key (conversion : String -> 'TResult) meta = 
                match (tryGetParameter key meta) with
                | None -> Missing
                | Some value ->
                    try
                        value
                        |> conversion
                        |> Found
                    with
                    | _ -> Malformed

        ///Represents the context in which an operation is executed
        type OperationContext = {
            Metadata : OperationMetadata;
            Message : obj option;
        }        

        ///Record describing the result of an operation
        type OperationResult = {
            StatusCode : Int32 option;
            Resource : obj option;
            Headers : (String * String) list;
        }
        with

            ///The empty result
            static member Empty = 
                {
                    StatusCode = None;
                    Resource = None;
                    Headers = [];
                }

            ///Creates a result with only a status code
            static member StatusOnly (statusCode, headers) = 
                {
                    StatusCode = (Some statusCode);
                    Resource = None;
                    Headers = headers;
                }

            ///Creates a result with only a status code
            static member StatusOnly statusCode = 
                OperationResult.StatusOnly (statusCode, [])

            ///Creates a result with only a resource
            static member ResourceOnly (resource, headers) = 
                {
                    StatusCode = None;
                    Resource = (Some resource);
                    Headers = headers;
                }

            ///Creates a result with only a resource
            static member ResourceOnly resource = 
                OperationResult.ResourceOnly (resource, [])

            ///Creates a result with body a status code and a resource
            static member Both (statusCode, resource, headers) = 
                {
                    StatusCode = (Some statusCode);
                    Resource = (Some resource);
                    Headers = headers;
                }

            ///Creates a result with body a status code and a resource
            static member Both (statusCode, resource) = 
                OperationResult.Both (statusCode, resource, [])

        ///Type alias describing the signature for operations
        type Operation = OperationContext -> OperationResult

    ///Contains a monad similar to maybe which can be used to escape function chains on exceptions
    module Attempt = 

        ///Contains the main monad functions for attempt
        module Monad = 

            ///Binds two synchronous functions together, calling g if f succeeds. 
            let bind f g = 
                fun state ->
                    match (f state) with
                    | Success value -> g value state
                    | Failure data -> Failure data

            ///Lifts a value to the success state
            let return' value =             
                fun _ ->
                    Success value

        ///Workflow for the attempt monad
        type AttemptBuilder () = 
            
            member this.Bind (expr, rest) = 
                Monad.bind expr rest

            member this.Return expr = 
                Monad.return' expr

        //Syntactic sugar for the attempt workflow
        let attempt = 
            AttemptBuilder ()       

    ///Asynchronous version of the Attempt monad and workflow
    module AsyncAttempt = 

        ///Contains the monad functions for the aysnc attempt workflow
        module Monad = 

            open Attempt

            ///Binds two asynchrounous functions together, calling g if f succeeds
            let bind f g = 
                fun state ->
                    async {

                        let! result = f state

                        match result with
                        | Success value -> 
                            return! g value state
                        | Failure data -> 
                            return (Failure data)
                    }

            ///Lifts a value to an asynchronous success state
            let return' value = 
                fun _ -> 
                    async {
                        return (Success value)
                    }

        ///Workflow for the async attempt monad
        type AsyncAttemptBuilder () = 

            member this.Bind (expr, rest) = 
                Monad.bind expr rest

            member this.Return expr = 
                Monad.return' expr

        ///Gets the state of the workflow
        let getState () =
            fun state ->
                async {
                    return (Success state)
                }

        //Syntactic sugar for the asycn attempt workflow
        let asyncAttempt = 
            AsyncAttemptBuilder ()

    ///Contains functions and types for serialising and deserialising request and response bodies
    module MessageIO = 

        open System.IO

        ///Type alias for the signature of deserialisers
        type Reader = Stream -> Type -> obj option

        ///Type alias for the signature of serialisers
        type Writer = obj -> byte list

        
            
