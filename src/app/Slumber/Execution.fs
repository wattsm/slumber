namespace Slumber

open System
open HandyFS.Option
open Slumber.Common.AsyncAttempt
open Slumber.Framework
open Slumber.Framework.Helpers
open Slumber.Render

///Contains functions used to invoke operations
module Execution =

    ///The default status code used for successful execution
    let [<Literal>] DefaultStatusCode = StatusCodes.Ok  

    ///Represents the selected message deserialiser and associated information
    type ReaderInfo = {
        Reader : MessageIO.Reader;
        ContentType : String;
        MessageType : Type;
    }

    ///Represents the selected message serialiser and associated information
    type WriterInfo = {
        Writer : MessageIO.Writer;
        ContentType : String;
    }

    ///Represents the selected operation and associated information
    type TargetInfo = {
        EndpointName : String;
        Operation : Operation;
        Parameters : (String * String) list;
    }
    with

        ///Empty target information
        static member Empty = 
            {
                EndpointName = String.Empty;
                Operation = (fun _ -> OperationResult.Empty);
                Parameters = [];
            }

    ///Represents the arguments used to run the execution phase
    type ExecutionArgs = {
        Request : Http.Request;
        Reader : ReaderInfo option;
        Writer : WriterInfo option;
        Target : TargetInfo;
        User : UserData option;
    }
    with

        ///Empty execution args
        static member Empty = 
            {
                Request = Http.Request.Empty;
                Reader = None;
                Writer = None;
                Target = TargetInfo.Empty;
                User = None;
            }

    ///Reads a message from the input stream
    let asyncGetMessage () =
        fun args ->
            async {

                try 
                    let message = 
                        match (args.Request.Payload.Body, args.Reader) with
                        | (Some _, None) ->

                            logWarn "[%A] Request message received but no reader selected" args.Request.Id

                            None

                        | (None, _) | (_, None) -> None
                        | (Some stream, Some reader) ->

                            logInfo "[%A] Deserialising request message" args.Request.Id

                            reader.Reader
                            <| stream
                            <| reader.MessageType

                    return Success message
                with
                | e -> 

                    logException e "[%A] Exception encountered reading request body: %A" args.Request.Id e.Message

                    ///TODO Standard error message for this error?

                    return Failure StatusCodes.BadRequest                
            }

    ///Gets the operation context for a set of arguments
    let asyncGetContext () = 
        asyncAttempt {

            let! message = asyncGetMessage ()
            let! args = getState ()

            let context = 
                {
                    Metadata = 
                        {
                            EndpointName = args.Target.EndpointName;
                            Request = args.Request;
                            Parameters = args.Target.Parameters;
                            User = args.User;
                        };
                    Message = message;
                } 
                              
            return context
        }

    ///Invokes the operation and returns the result
    let asyncInvokeOperation (context : OperationContext) =
        fun (args : ExecutionArgs) ->
            async {

                logInfo "[%A] Invoking operation" args.Request.Id

                try 
                    return (Success (args.Target.Operation context))
                with
                | e ->

                    logException e "[%A] An exception was encountered invoking the operation: %A" args.Request.Id e.Message

                    //TODO Error message to display?

                    return (Failure StatusCodes.InternalServerError)
            }

    ///Serialises the resource returned by the operation, if any
    let asyncGetResponse (result : OperationResult) =
        fun args -> 
            async {

                logInfo "[%A] Constructing operation response" args.Request.Id

                let statusCode =
                    result.StatusCode
                    |> someOr DefaultStatusCode

                try 
                    return 
                        match (result.Resource, args.Writer) with 
                        | (Some _, None) -> 

                            logInfo "[%A] Response resource returned but no writer selected" args.Request.Id

                            Failure StatusCodes.NotAcceptable

                        | (None, _) -> 

                            logInfo "[%A] No response resource returned, status code is %A" args.Request.Id statusCode

                            Success (StatusCode statusCode, result.Headers)

                        | (Some resource, Some writer) ->

                            logInfo "[%A] Response resource returned, status code is %A" args.Request.Id statusCode

                            let bytes = 
                                writer.Writer resource

                            Success (Resource (statusCode, bytes), result.Headers)
                with
                | e ->

                    
                    logException e "[%A] Exception encountered writing response body: %A" args.Request.Id e.Message

                    ///TODO Error message to display

                    return Failure StatusCodes.NotAcceptable
            }

    ///Asynchronously runs the execution phase
    let asyncRun (args : ExecutionArgs) =

        let (-->) = AsyncAttempt.Monad.bind
        let start = AsyncAttempt.Monad.return' ()

        async {

            logInfo "[%A] Beginning operation execution phase" args.Request.Id

            let! result =             
                args 
                |> start
                --> asyncGetContext
                --> asyncInvokeOperation
                --> asyncGetResponse

            let responseType, headers = 
                match result with
                | Success (responseType, headers) -> (responseType, headers)
                | Failure statusCode -> (StatusCode statusCode, [])

            let contentType = 
                match args.Writer with
                | Some writer -> Some writer.ContentType
                | _ -> None

            let response = 
                { 
                    ResponseType = responseType;
                    ContentType = contentType;
                    CustomHeaders = headers;
                }

            return (stopWithResponse response)
        }

    ///Runs the execution phase
    let run<'TState> : ExecutionArgs -> State<'TState> = 
        asyncRun
        >> Async.RunSynchronously

