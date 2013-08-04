namespace Slumber.Tests

open System
open System.Text
open System.IO
open FsUnit
open Xunit
open Slumber

module ``Execution facts`` =
    
    open Execution
    open Http

    let [<Literal>] ModuleName = "Execution"

    [<AutoOpen>]
    module Helpers = 

        let getRequest body = 
            
            let stream = 
                if (String.IsNullOrWhiteSpace body) then
                    None
                else
                    
                    let bytes = 
                        body |> Encoding.UTF8.GetBytes

                    Some (new MemoryStream (bytes) :> Stream)

            let baseUrl = 
                Uri ("http://localhost:8080/api", UriKind.Absolute)

            {
                Request.Empty
                with
                    Url = 
                        {
                            Urls.Empty
                            with
                                Raw = Uri (baseUrl, "people");
                                Path = "/people";
                                BaseUrl = baseUrl;
                        };
                    Verb = "POST";
                    Payload = 
                        {
                            Payload.Empty
                            with
                                Body = stream;
                        }
            }

        let getReader (value : String) = 
            fun _ _ ->
                value
                |> box
                |> Some

        let getArgs (hasBody, hasReader) = 

            let body = 
                if hasBody then
                    "Hello, World"
                else
                    String.Empty

            let reader = 
                if hasReader then
                    Some {
                        ContentType = "text/xml";
                        Reader = (getReader body);
                        MessageType = typedefof<String>;
                    }
                else
                    None

            let writer = 
                {
                    ContentType = "text/xml";
                    Writer = (fun _ -> []);
                }

            {
                Request = (getRequest body);
                Reader = reader;
                Writer = Some writer;
                Parameters = [];
                Operation = fun _ -> OperationResult.Both (200, "Hello, World", [ ("key", "value"); ])
                User = None;
            }

        let getArgs' () = 
            getArgs (true, true)

        let setOperation op args = 
            { args with Operation = op; }

        let setWriter writer (args : ExecutionArgs) = 
            { args with Writer = writer; }
    
    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetMessage function`` = 

        let getMessage =    
            asyncGetMessage ()
            >> Async.RunSynchronously

        let [<Fact>] ``No body returns nothing`` () =

            //NOTE This should never happen when executed as a part of the default pipeline but is possible from the arguments
                
            let isNothing result = 
                match result with
                | Success None -> true
                | _ -> false

            getArgs (false, true)
            |> getMessage
            |> isNothing
            |> should be True

        let [<Fact>] ``No reader returns nothing`` () =
                
            let isNothing result = 
                match result with
                | Success None -> true
                | _ -> false
                    
            getArgs (true, false)
            |> getMessage
            |> isNothing
            |> should be True

        let [<Fact>] ``The message is returned correctly`` () =
                
            let isCorrect result = 
                match result with
                | Success (Some value) -> ((string value) = "Hello, World")
                | _ -> false

            getArgs (true, true)
            |> getMessage
            |> isCorrect
            |> should be True

        let [<Fact>] ``Exception from reader returns HTTP 400`` () =
                
            let isBadRequest result = 
                match result with
                | Failure statusCode -> statusCode = StatusCodes.BadRequest
                | _ -> false

            let setFailingReader args = 
                    
                let reader = 
                    match args.Reader with
                    | Some reader' -> 
                        Some {
                            reader'
                            with
                                Reader = (fun _ _ -> raise (InvalidOperationException ()));
                        }
                    | _ -> None

                { args with Reader = reader; }

            getArgs (true, true)
            |> setFailingReader
            |> getMessage
            |> isBadRequest
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetContext function`` = 

        let getContext args = 
            args
            |> asyncGetContext ()
            |> Async.RunSynchronously

        let [<Fact>] ``Request ID is set correctly on OperationMetadata`` () =

            let requestId = Guid.NewGuid ()
            
            let isCorrect result = 
                match result with
                | Success context -> context.Metadata.RequestId = requestId
                | _ -> false

            let setRequestId args = 
                { args with Request = { args.Request with Id = requestId; } } 

            getArgs' ()
            |> setRequestId
            |> getContext
            |> isCorrect
            |> should be True

        let [<Fact>] ``User is set correctly on OperationMetadata when provided`` () =

            let setUser (args : ExecutionArgs) = 
                { args with User = (Some { Id = "user.name"; Properties = []; }); }

            let isCorrect result = 
                match result with
                | Success context -> 
                    match context.Metadata.User with
                    | Some data -> data.Id = "user.name"
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> setUser
            |> getContext
            |> isCorrect
            |> should be True

        let [<Fact>] ``No user is set on OperationMetadata when not provided`` () =
            
            let isCorrect result = 
                match result with
                | Success context -> Option.isNone context.Metadata.User
                | _ -> false

            getArgs' ()
            |> getContext
            |> isCorrect
            |> should be True
            

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncInvokeOperation function`` = 
            
        let invokeOperation context args = 
            asyncInvokeOperation context args
            |> Async.RunSynchronously

        let createContext () = 

            let baseUrl = 
                Uri ("http://localhost:8080/api", UriKind.Absolute)

            {
                Metadata = 
                    {
                        OperationMetadata.Empty
                        with
                            Url = 
                                {
                                    Raw = Uri (baseUrl, "people/12345");
                                    Path = "/people/12345";
                                    Query = [];
                                    BaseUrl = baseUrl;
                                };
                            Parameters = [];
                    };
                Message = None;
            }

        let [<Fact>] ``Successful returns correct value`` () =

            let isCorrectValue result = 
                match result with
                | Success data ->
                    data.StatusCode = (Some 200)
                        && data.Resource = (Some (box "Hello, World"))
                | _ -> false

            getArgs' ()
            |> invokeOperation (createContext ())
            |> isCorrectValue
            |> should be True

        let [<Fact>] ``Exception returns HTTP 500`` () =
            
            let isServerError result = 
                match result with
                | Failure statusCode -> statusCode = 500
                | _ -> false

            getArgs' ()
            |> setOperation (fun _ -> raise (InvalidOperationException ()))
            |> invokeOperation (createContext ())
            |> isServerError
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetResponse function`` =

        open System.Text

        let getResponse result args = 
            asyncGetResponse result args
            |> Async.RunSynchronously

        let [<Fact>] ``Empty result returns default status code and no resource`` () =

            let isDefaultStatusCode result = 
                match result with
                | Success (StatusCode statusCode, _) -> statusCode = DefaultStatusCode
                | _ -> false

            getArgs' ()
            |> getResponse OperationResult.Empty
            |> isDefaultStatusCode
            |> should be True

        let [<Fact>] ``Status code with no resource returns specified status code`` () =

            let isStatusCode result = 
                match result with
                | Success (StatusCode 418, _) -> true
                | _ -> false

            getArgs' ()
            |> getResponse (OperationResult.StatusOnly 418)
            |> isStatusCode
            |> should be True

        let [<Fact>] ``Default status code used when none specified`` () =

            let isDefaultStatusCode result = 
                match result with
                | Success (Resource (DefaultStatusCode, _), _) -> true
                | _ -> false

            getArgs' ()
            |> getResponse ({ StatusCode = None; Resource = (Some (box  "Hello, World")); Headers = []; })
            |> isDefaultStatusCode
            |> should be True

        let [<Fact>] ``Resource with no writer returns HTTP 406`` () =

            let isNotAcceptable result = 
                match result with
                | Failure statusCode -> statusCode = StatusCodes.NotAcceptable
                | _ -> false

            getArgs' ()
            |> setWriter None
            |> getResponse (OperationResult.ResourceOnly "Hello, World")
            |> isNotAcceptable
            |> should be True

        let [<Fact>] ``Writer exception returns HTTP 406`` () =

            let isNotAcceptable result = 
                match result with
                | Failure statusCode -> statusCode = StatusCodes.NotAcceptable
                | _ -> false

            let writer = 
                {
                    ContentType = "text/plain";
                    Writer = (fun _ -> raise (InvalidOperationException ()));
                }

            getArgs' ()
            |> setWriter (Some writer)
            |> getResponse (OperationResult.ResourceOnly "Hello, World")
            |> isNotAcceptable
            |> should be True

        let [<Fact>] ``Writer output is returned correctly`` () =

            let bytes = 
                Encoding.UTF8.GetBytes "Hello, World"
                |> Array.toList

            let writer =
                {
                    ContentType = "text/plain";
                    Writer = (fun _ -> bytes);
                }

            let isCorrectValue result = 
                match result with
                | Success (Resource (_, bytes'), _) -> bytes' = bytes
                | _ -> false

            getArgs' ()
            |> setWriter (Some writer)
            |> getResponse (OperationResult.ResourceOnly "Hello, World")
            |> isCorrectValue
            |> should be True

        let [<Fact>] ``Headers are returned with resource`` () =

            let headers = 
                [ ("key", "value"); ]
            
            let headersAreCorrect result = 
                match result with
                | Success (_, headers') -> headers' = headers
                | _ -> false

            getArgs' ()
            |> getResponse (OperationResult.ResourceOnly ("Hello, World", headers))
            |> headersAreCorrect
            |> should be True

        let [<Fact>] ``Headers are returned with status code`` () =

            let headers =
                [ ("key", "value"); ]
            
            let headersAreCorrect result = 
                match result with
                | Success (_, headers') -> headers' = headers
                | _ -> false

            getArgs' ()
            |> getResponse (OperationResult.StatusOnly (200, headers))
            |> headersAreCorrect
            |> should be True

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``run function`` = 

        open Framework

        let [<Fact>] ``Stops due to completion`` () =
            
            let isStopped result = 
                match result with
                | Stopped (Completed _) -> true
                | _ -> false

            getArgs' () 
            |> run
            |> isStopped
            |> should be True

        let [<Fact>] ``Returns the correct response type`` () =
            
            let isCorrectResponse result = 
                match result with
                | Stopped (Completed resp) ->
                    match resp.ResponseType with
                    | Resource (200, []) -> true
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> run
            |> isCorrectResponse
            |> should be True

        let [<Fact>] ``Returns the correct headers`` () =
            
            let areHeadersCorrect result = 
                match result with
                | Stopped (Completed resp) -> resp.CustomHeaders = [ ("key", "value"); ]
                | _ -> false

            getArgs' ()
            |> run
            |> areHeadersCorrect
            |> should be True

        let [<Fact>] ``Returns the content type of the writer if present`` () =
            
            let isContentTypeCorrect result = 
                match result with
                | Stopped (Completed resp) ->
                    match resp.ContentType with
                    | Some contentType -> contentType = "text/xml"
                    | _ -> false
                | _ -> false

            getArgs' ()
            |> run
            |> isContentTypeCorrect
            |> should be True

        let [<Fact>] ``Returns no content type if no writer present`` () =
            
            let isContentTypeCorrect result = 
                match result with
                | Stopped (Completed resp) -> Option.isNone resp.ContentType
                | _ -> false

            getArgs' ()
            |> setOperation (fun _ -> OperationResult.Empty)
            |> setWriter None
            |> run
            |> isContentTypeCorrect
            |> should be True


