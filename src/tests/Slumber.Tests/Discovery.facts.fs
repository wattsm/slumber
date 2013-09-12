namespace Slumber.Tests

open FsUnit
open Xunit
open Xunit.Extensions
open System
open System.IO
open Slumber

module ``Discovery facts`` =

    open Discovery
    open Http
    open Framework
    open Setup.Bindings

    let teapot (_ : obj option) = 
        OperationResult.StatusOnly 418 //Teapot

    let endpoint = 
        {
            Name = "Dummy";
            Template = "/people/{personCode}";
            Bindings = [ get teapot ]; 
        }

    let [<Literal>] ModuleName = "Discovery"

    [<AutoOpen>] 
    module Helpers = 
        
        let getArgs (relativeUrl : String) =     
        
            let reader _ _ = 
                None

            let writer _ = 
                []

            let relativeUrl' = 
                if (relativeUrl.StartsWith "/") then
                    relativeUrl.Substring (1)
                else
                    relativeUrl

            let baseUrl = 
                Uri ("http://localhost:8080/api/", UriKind.Absolute)

            {
                Request = 
                    {
                        Request.Empty 
                        with
                            Url = 
                                {
                                    Urls.Empty
                                    with
                                        Raw = Uri (baseUrl, relativeUrl');
                                        Path = relativeUrl;
                                        BaseUrl = baseUrl;
                                };
                            Verb = "GET";
                            Payload = 
                                {
                                    Payload.Empty
                                    with
                                        Body = (Some (new MemoryStream () :> Stream));
                                }
                    } ;
                Container =
                    {
                        Container.Empty
                        with
                            Endpoints = [ endpoint; ];
                            IO = 
                                {
                                    IOConfig.Empty
                                    with
                                        Readers = [ (MediaTypes.Text.Xml, reader); (MediaTypes.Application.Json, reader) ];
                                        Writers = [ (MediaTypes.Text.Xml, writer); (MediaTypes.Application.Json, writer) ];
                                }
                            BaseUrl = baseUrl;
                    }
            }

        let addHeader name value (args : DiscoveryArgs) = 

            let payload'  = 
                { args.Request.Payload with Headers = (name, value) :: args.Request.Payload.Headers; }

            { args with Request = { args.Request with Payload = payload'; }; }

        let addForwardedType from to' args = 

            let container' =
                {
                    args.Container
                    with
                        IO = 
                            {
                                args.Container.IO
                                with
                                    ForwardedTypes = [ (from, to'); ];
                            }
                }

            { args with Container = container'; }
      
        let makePrivate auth args = 
            let security = { args.Container.Security with DefaultMode = Private; Authenticate = (Some auth); }
            in  { args with Container = { args.Container with Security = security; }; }

    module ``Matching facts`` = 

        open Discovery.Matching
        open Attempt

        let [<Literal>] ModuleName = "Discovery.Matching"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``normaliseUrl function`` =

            let normaliseUrl' url = 
                Uri (url, UriKind.Absolute)
                |> normaliseUrl                

            [<Theory>]
            [<InlineData ("http://localhost/")>]
            [<InlineData ("http://localhost/api/")>]
            let ``URIs with a trailing slash are not changed`` url =
                normaliseUrl' url
                |> string
                |> should equal url

            [<Theory>]
            [<InlineData ("http://localhost", "http://localhost/")>]
            [<InlineData ("http://localhost?key=value", "http://localhost/?key=value")>]
            [<InlineData ("http://localhost/api", "http://localhost/api/")>]
            [<InlineData ("http://localhost/api?key=value", "http://localhost/api/?key=value")>]
            let ``A trailing slash is added after the path if not present`` url expected =
                normaliseUrl' url
                |> string
                |> should equal expected

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncMatchEndpoint function`` = 

            let matchEndpoint = 
                asyncMatchEndpoint ()
                >> Async.RunSynchronously

            let [<Fact>] ``Unregistered URL returns HTTP 404`` () =

                let isNotFound outcome = 
                    match outcome with
                    | Failure statusCode -> statusCode = StatusCodes.NotFound
                    | _ -> false

                "/addresses"
                |> getArgs
                |> matchEndpoint
                |> isNotFound
                |> should be True

            let [<Fact>] ``Matching URL returns success`` () = 

                let isSuccess outcome = 
                    match outcome with
                    | Success _ -> true
                    | _ -> false

                "/people/12345"
                |> getArgs
                |> matchEndpoint
                |> isSuccess
                |> should be True

            let [<Fact>] ``Trailing slashes are ignored`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Success _ -> true
                    | _ -> false

                "/people/12345/"
                |> getArgs
                |> matchEndpoint
                |> isSuccess
                |> should be True

            let [<Fact>] ``Successful match includes correct endpoint`` () =

                let getTemplate outcome = 
                    match outcome with
                    | Success (endpoint, _) -> endpoint.Template
                    | _ -> String.Empty

                "/people/12345"
                |> getArgs
                |> matchEndpoint
                |> getTemplate
                |> should equal "/people/{personCode}"

            let [<Fact>] ``Successful match includes matched URL parameters`` () =
                
                let getParameters outcome = 
                    match outcome with
                    | Success (_, parameters) -> parameters
                    | _ -> []

                "/people/12345"
                |> getArgs
                |> matchEndpoint
                |> getParameters
                |> List.same [ ("PERSONCODE", "12345") ] //Note UriTemplate converts placeholders to upper case. Potentially annoying.
                |> should be True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncMatchBinding function`` =

            let matchBinding endpoint = 
                "/people/12345"
                |> getArgs
                |> asyncMatchBinding endpoint 
                |> Async.RunSynchronously

            let [<Fact>] ``No registered binding returns HTTP 405`` () = 

                let isNotSupported outcome = 
                    match outcome with
                    | Failure statusCode -> statusCode = StatusCodes.MethodNotAllowed
                    | _ -> false

                let endpoint' = 
                    { endpoint with Bindings = [ post teapot ] }

                (endpoint', [])
                |> matchBinding
                |> isNotSupported
                |> should be True
                
            let [<Fact>] ``Registered binding returns success`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Success _ -> true
                    | _ -> false
                
                (endpoint, [])
                |> matchBinding
                |> isSuccess
                |> should be True

            let [<Fact>] ``Success includes URL parameters`` () =
                
                let getParameters outcome = 
                    match outcome with
                    | Success (result : MatchingResult) -> result.Parameters
                    | _ -> []

                let parameters = 
                    [ ("PERSONCODE", "12345"); ]

                (endpoint, parameters)
                |> matchBinding
                |> getParameters
                |> List.same parameters
                |> should be True

            let [<Fact>] ``Success includes operation`` () =
                
                let executeOp outcome = 
                    
                    let (context : OperationContext) = 

                        let baseUrl = 
                            Uri ("http://localhost/api", UriKind.Absolute)

                        {
                            Metadata = 
                                {
                                    OperationMetadata.Empty
                                    with
                                        Request = 
                                            {
                                                Request.Empty
                                                with
                                                    Url = 
                                                        {
                                                            Raw = Uri (baseUrl, "people/12345");
                                                            Path = "/people/12345";
                                                            Query = [];
                                                            BaseUrl = baseUrl;
                                                        };                                                    
                                            };
                                };
                            Message = None;
                        }

                    match outcome with
                    | Success result -> Some (result.Binding.Operation context)
                    | _ -> None

                (endpoint, [])
                |> matchBinding
                |> executeOp
                |> should be (Some' ({ StatusCode = Some 418; Resource = None; Headers = []; }))

            let [<Fact>] ``Success includes message type`` () =

                let isStringMessage result = 
                    match result with
                    | Success result -> result.Binding.MessageType = (Some typedefof<obj>)
                    | _ -> false

                (endpoint, [])
                |> matchBinding
                |> isStringMessage
                |> should be True

            let [<Fact>] ``Success includes endpoint name`` () =

                let hasCorrectName result = 
                    match result with
                    | Success (result : MatchingResult) -> result.EndpointName = endpoint.Name
                    | _ -> false

                (endpoint, [])
                |> matchBinding
                |> hasCorrectName
                |> should be True                

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetMatchingResult function`` =

            let getMatchingResult = 
                asyncGetMatchingResult
                >> Async.RunSynchronously

            let [<Fact>] ``No registered URL returns HTTP 404`` () =
                
                let isNotFound outcome = 
                    match outcome with
                    | Failure statusCode -> statusCode = 404
                    | _ -> false

                "/addresses"
                |> getArgs
                |> getMatchingResult
                |> isNotFound
                |> should be True

            let [<Fact>] ``No registered operation returns HTTP 405`` () =
                
                let isNotSupported outcome = 
                    match outcome with
                    | Failure statusCode -> statusCode = 405
                    | _ -> false

                let changeVerb (args : DiscoveryArgs) = 
                    { args with Request = { args.Request with Verb = "POST"; }; }

                "/people/12345"
                |> getArgs
                |> changeVerb
                |> getMatchingResult
                |> isNotSupported
                |> should be True

            let [<Fact>] ``Registered URL and binding returns success`` () =

                let isSuccess outcome = 
                    match outcome with
                    | Success _ -> true
                    | _ -> false

                "/people/12345"
                |> getArgs
                |> getMatchingResult
                |> isSuccess
                |> should be True

    module ``Security facts`` =

        open Security

        let [<Literal>] ModuleName = "Discovery.Security"
            
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncAuthenticateRequest function`` =
            
            open Matching

            [<AutoOpen>]
            module Helpers = 

                let authenticateRequest binding args = 
                    asyncAuthenticateRequest binding args
                    |> Async.RunSynchronously

                let setAuth f args = 
                    let security = { args.Container.Security with Authenticate = (Some f); }
                    in { args with Container = { args.Container with Security = security; }; }

                let setMode mode args = 
                    let security = { args.Container.Security with DefaultMode = mode; } 
                    in { args with Container = { args.Container with Security = security; }; }

                let getResult mode = 
                    {
                        EndpointName = "";
                        Parameters = [];
                        Binding = 
                            {
                                Verb = "VERB";
                                SecurityMode = mode;
                                Operation = (fun _ -> OperationResult.Empty);
                                MessageType = None;
                            };
                    }

                let assertCalled isPublic defaultMode expected = 

                    let _called = ref false

                    let result = 
                        getResult isPublic

                    let auth _ = 
                        _called := true
                        Deny

                    getArgs "/12345"
                    |> setAuth auth
                    |> setMode defaultMode
                    |> authenticateRequest result
                    |> ignore

                    Assert.Equal (expected, _called.Value)

                let getUserData outcome = 
                    match outcome with
                    | Success user -> user
                    | _ -> invalidOp "Unexpected outcome"

                let getStatusCode outcome = 
                    match outcome with
                    | Failure statusCode -> statusCode
                    | _ -> invalidOp "Unexpected outcome"

            let [<Fact>] ``Authentication function called for private bindings`` () =
                assertCalled
                <| (Some SecurityMode.Private)
                <| SecurityMode.Private
                <| true

            let [<Fact>] ``Authentication function not called for public bindings`` () =
                assertCalled
                <| (Some SecurityMode.Public)
                <| SecurityMode.Private
                <| false

            let [<Fact>] ``Authenticaiton function called for inherited bindings if mode is private`` () =
                assertCalled
                <| None
                <| SecurityMode.Private
                <| true

            let [<Fact>] ``Authentication function not called for inherited bindings if mode is public`` () =
                assertCalled
                <| None
                <| SecurityMode.Public
                <| false

            let [<Fact>] ``Correct user data is returned when authentication is successful`` () =
                
                let result = 
                    getResult (Some SecurityMode.Private)

                let auth _ =
                    Allow (Some { Id = "admin"; Properties = []; })

                let isCorrect (data : UserData option) = 
                    match data with 
                    | Some user -> user.Id = "admin"
                    | _ -> false

                getArgs "/1235"
                |> setAuth auth
                |> authenticateRequest result
                |> getUserData
                |> isCorrect
                |> should be True

            let [<Fact>] ``Nothing is returned for private binding when no authentication function is set`` () =
                
                let result = getResult (Some SecurityMode.Private)

                getArgs "/12345"
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> should be True

            let [<Fact>] ``Nothing is returned for public bindings`` () =
                
                let result = getResult (Some SecurityMode.Public)
                let auth _ = Allow (Some { Id = "admin"; Properties = []; })

                getArgs "/1235"
                |> setAuth auth
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> should be True

            let [<Fact>] ``Nothing is returned for inherited bindings when mode is public`` () =
                
                let result = getResult None
                let auth _ = Allow (Some { Id = "admin"; Properties = []; })

                getArgs "/12345"
                |> setAuth auth
                |> setMode SecurityMode.Public
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> should be True

            let [<Fact>] ``Nothing is returned for inherited bindings when mode is private but no authentication function is set`` () = 
                
                let result = getResult None

                getArgs "/12345"
                |> setMode SecurityMode.Private
                |> authenticateRequest result
                |> getUserData
                |> Option.isNone
                |> should be True

            let [<Fact>] ``HTTP 401 is returend when authentication is not successful`` () =
                
                let result = getResult (Some SecurityMode.Private)
                let auth _ = Deny

                getArgs "/12345"
                |> setAuth auth
                |> authenticateRequest result
                |> getStatusCode
                |> should equal StatusCodes.Unauthorised


    module ``Negotiation facts`` =

        open Discovery.Negotiation

        let [<Literal>] ModuleName = "Discovery.Negotiation"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetReader function`` =

            let getRequestType = 

                let getContentType result = 
                    match result with
                    | Some (contentType, _) -> Some contentType
                    | _ -> None

                asyncGetReader
                >> Async.RunSynchronously
                >> getContentType

            let [<Fact>] ``Specified content type is selected if supported`` () =
                getArgs "/people/12345"
                |> addHeader "Content-Type" MediaTypes.Text.Xml
                |> getRequestType
                |> should be (Some' MediaTypes.Text.Xml)

            let [<Fact>] ``Nothing is selected if content type is not supported`` () =
                getArgs "/people/12345"
                |> addHeader "Content-Type" "application/vendor"
                |> getRequestType
                |> should be None'<String>

            let [<Fact>] ``Default content type is selected if none specified`` () =
                getArgs "/people/12345"
                |> getRequestType
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Target content type is used if specified content type is forwarded`` () =
                getArgs "/people/12345"
                |> addHeader "Content-Type" MediaTypes.Text.Html
                |> addForwardedType MediaTypes.Text.Html MediaTypes.Text.Xml
                |> getRequestType
                |> should be (Some' MediaTypes.Text.Xml)

            let [<Fact>] ``Target content type is used in preference to specified content type even if supported`` () =
                getArgs "/people/12345"
                |> addHeader "Content-Type" MediaTypes.Text.Xml
                |> addForwardedType MediaTypes.Text.Xml MediaTypes.Application.Json
                |> getRequestType
                |> should be (Some' MediaTypes.Application.Json)
                
        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncGetWriter function`` =

            let getResponseType =

                let getContentType result = 
                    match result with
                    | Some (contentType, _) -> Some contentType
                    | _ -> None

                asyncGetWriter
                >> Async.RunSynchronously
                >> getContentType

            let [<Fact>] ``Specified content type is selected if supported`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" MediaTypes.Text.Xml
                |> getResponseType
                |> should be (Some' MediaTypes.Text.Xml)

            let [<Fact>] ``First supported content type is selected if multiple types are specified`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" "text/html, text/xml, application/json"
                |> getResponseType
                |> should be (Some' MediaTypes.Text.Xml)

            let [<Fact>] ``Nothing is selected if content type is not supported`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" MediaTypes.Text.Html
                |> getResponseType
                |> should be None'<String>

            let [<Fact>] ``Default content type is selected if none specified`` () =
                getArgs "/people/12345"
                |> getResponseType
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Default content type is selected if */* is specified`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" "*/*"
                |> getResponseType
                |> should be (Some' DefaultMediaType)

            let [<Fact>] ``Target content type is used if specified content type is forwarded`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" MediaTypes.Text.Html
                |> addForwardedType MediaTypes.Text.Html MediaTypes.Text.Xml
                |> getResponseType
                |> should be (Some' MediaTypes.Text.Xml)

            let [<Fact>] ``Target content type is used in preference to specified content type even if supported`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" MediaTypes.Text.Xml
                |> addForwardedType MediaTypes.Text.Xml MediaTypes.Application.Json
                |> getResponseType
                |> should be (Some' MediaTypes.Application.Json)

            let [<Fact>] ``Content type forwarding is applied when multiple content types are specified`` () =
                getArgs "/people/12345"
                |> addHeader "Accept" "text/html, text/plain, application/json"
                |> addForwardedType MediaTypes.Text.Html MediaTypes.Text.Xml
                |> getResponseType
                |> should be (Some' MediaTypes.Text.Xml)

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``run function`` = 

        open Framework
        open Discovery.Negotiation

        let [<Fact>] ``Unsupported content type returns HTTP 415`` () =

            let isNotSupported state = 
                match state with
                | Stopped stopType -> 
                    match stopType with
                    | Completed response ->
                        match response.ResponseType with
                        | StatusCode statusCode -> statusCode = StatusCodes.ContentTypeNotSupported
                        | _ -> false
                    | _ -> false
                | _ -> false

            getArgs "/people/12345"
            |> addHeader "Content-Type" MediaTypes.Text.Html
            |> run
            |> isNotSupported
            |> should be True

        let [<Fact>] ``Bindings without messages return no reader information`` () =

            let hasNoReader state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> Option.isNone args.Reader
                | _ -> false

            let setBinding args = 

                let binding = 
                    get (fun () -> "Hello, World")

                let container = 
                    { args.Container with Endpoints = [ { endpoint with Bindings = [ binding; ]; } ]; }

                { args with Container = container; }

            getArgs "/people/12345"
            |> setBinding
            |> run
            |> hasNoReader
            |> should be True

        let [<Fact>] ``Bindings with messages return correct reader information`` () =
            
            let isReaderCorrect state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> 
                    match args.Reader with
                    | Some reader -> 
                        reader.ContentType = DefaultMediaType
                            && reader.MessageType = typedefof<obj>
                    | _ -> false
                | _ -> false

            getArgs "/people/12345"
            |> run
            |> isReaderCorrect
            |> should be True

        let [<Fact>] ``Correct writer information is returned`` () =
            
            let isWriterCorrect state = 
                match state with
                | Running (args : Execution.ExecutionArgs) ->
                    match args.Writer with
                    | Some writer -> writer.ContentType = DefaultMediaType
                    | _ -> false
                | _ -> false

            getArgs "/people/12345"
            |> run
            |> isWriterCorrect
            |> should be True

        let [<Fact>] ``Unauthorised request returns HTTP 401`` () =

            let isUnauthorised' state = 
                match state with
                | Stopped stopType ->
                    match stopType with
                    | Completed response ->
                        match response.ResponseType with
                        | StatusCode statusCode -> statusCode = StatusCodes.Unauthorised
                        | _ -> false
                    | _ -> false
                | _ -> false

            getArgs "/people/12345"
            |> makePrivate (fun _ -> Deny)
            |> run
            |> isUnauthorised'
            |> should be True

        let [<Fact>] ``User details are returned if provided by authentication function`` () =
            
            let hasUserDetails state = 
                match state with
                | Running (args : Execution.ExecutionArgs) -> 
                    match args.User with
                    | Some data -> data.Id = "user.name"
                    | _ -> false
                | _ -> false

            getArgs "/people/12345"
            |> makePrivate (fun _ -> Allow (Some { Id = "user.name"; Properties = []; }))
            |> run
            |> hasUserDetails
            |> should be True

