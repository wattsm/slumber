namespace Slumber

open System
open System.Text
open System.Web
open System.IO
open System.Collections.Specialized
open Slumber.Framework

///Contains functions used to render the HTTP response 
module Render =

    ///Content type used for textual message
    let [<Literal>] MessageContentType = MediaTypes.Text.Plain

    ///Functions for writing to the HTTP response object
    module Writing = 

        ///Writes an optional body to the given output
        let asyncWriteBody (output : IOutput) args = 
            async {
                match args.ResponseType with
                | StatusCode _ -> ()
                | Resource (_, []) -> ()
                | Resource (_, bytes) -> do! output.WriteBody (bytes)                    
            }     
        
        ///Writes the response headers to the given output
        let asyncWriteHeaders (output : IOutput) args = 

            let addContentLength headers = 

                let value = 
                    match args.ResponseType with
                    | StatusCode _ -> 0
                    | Resource (_, bytes) -> List.length bytes
                    
                (Headers.ContentLength, (string value)) :: headers

            let addContentType headers = 

                let currentValue = headers |> Headers.getContentType

                match (currentValue, args.ContentType) with
                | (None, Some value) -> (Headers.ContentType, value) :: headers
                | _ -> headers

            async {
                args.CustomHeaders
                |> addContentLength
                |> addContentType
                |> List.iter (fun (key, value) ->
                        output.WriteHeader key value
                        |> Async.RunSynchronously
                    )
            }

    ///Writes the output asynchronously
    let asyncWrite (requestId : Guid) (output : IOutput) response = 
        async {

            let statusCode = 
                match response.ResponseType with
                | StatusCode code -> code
                | Resource (code, _) -> code

            do!
                Writing.asyncWriteBody 
                <| output
                <| response

            do!
                Writing.asyncWriteHeaders
                <| output
                <| response

            do! output.SetStatusCode statusCode

            logInfo "[%A] Response rendered, status code is %A" requestId statusCode
        }    

    ///Gets the response to be rendered from the given state
    let asyncGetResponse (requestId : Guid) state = 
    
        let getMessageResponse (message : String) = 

            let bytes = 
                Encoding.UTF8.GetBytes (message)
                |> Array.toList

            {
                Response.Empty
                with
                    ResponseType = Resource (StatusCodes.InternalServerError, bytes);
                    ContentType = Some MessageContentType;
            }

        async {
            return 
                match state with 
                | Running _ -> 

                    logInfo "[%A] Pipeline has failed to stop," requestId
                
                    getMessageResponse "Pipeline failed to stop"

                | Stopped type' ->
                    match type' with
                    | Exception e -> 
                    
                        logInfo "[%A] Pipeline has encountered an exception" requestId
                        
                        getMessageResponse e.Message

                    | Completed resp -> 
                    
                        logInfo "[%A] Pipeline has completed successfully" requestId
                        
                        resp
        }

    ///Runs the render phase asynchronously
    let asyncRun requestId context = 
        fun state ->
            async {

                logInfo "[%A] Beginning render phase" requestId

                let! response = asyncGetResponse requestId state
                do! asyncWrite requestId context response

            }

    ///Runs the render phase synchronously
    let run requestId context = 
        asyncRun requestId context 
        >> Async.RunSynchronously

