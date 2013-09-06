namespace Slumber

open System
open System.Text
open System.Web
open System.IO
open System.Collections.Specialized
open Slumber.Common.Http
open Slumber.Common.Http.Headers
open Slumber.Framework

///Contains functions used to render the HTTP response 
module Render =

    ///Content type used for textual message
    let [<Literal>] MessageContentType = MediaTypes.Text.Plain

    ///Functions for writing to the HTTP response object
    module Writing = 

        ///Writes an optional body to the given output stream
        let asyncWriteBody (stream : Stream) bytes = 
            async {
                match bytes with
                | None | Some [] -> ()
                | Some bytes' ->

                    //TODO Find a way to make the code below look less manky; Async.AwaitTask doesn't seem to work with non-generic tasks

                    do! 
                        Async.FromBeginEnd(
                            List.toArray bytes',
                            0,
                            List.length bytes',
                            stream.BeginWrite,
                            stream.EndWrite
                        )                   
            }     
        
        ///Writes the response headers
        let asyncWriteHeaders (headers : NameValueCollection) args = 

            let setHeader key value = 
                headers.[key] <- value

            async {

                let contentLength = 
                    match args.ResponseType with
                    | StatusCode _ -> 0
                    | Resource (_, bytes) -> List.length bytes

                setHeader ContentLength (string contentLength)

                args.CustomHeaders
                |> List.filter (fst >> String.same Headers.ContentType >> not) //Special header
                |> List.iter (fun (key, value) -> setHeader key value)
            }

        ///Writes "special" headers that need to be set against the response
        let asyncWriteSpecialHeaders (response : HttpResponseBase) args = 
            async {

                let customContentType = 
                    args.CustomHeaders
                    |> pickContentType

                match (customContentType, args.ContentType) with
                | (Some contentType, _) -> response.ContentType <- contentType
                | (None, Some contentType) -> response.ContentType <- contentType
                | _ -> ()
            }

    ///Writes the output asynchronously
    let asyncWrite (requestId : Guid) (context : HttpContextBase) response = 
        async {

            let statusCode, bytes = 
                match response.ResponseType with
                | StatusCode code -> code, None
                | Resource (code, bytes) -> code, Some bytes

            do!
                Writing.asyncWriteBody 
                <| context.Response.OutputStream 
                <| bytes

            do!
                Writing.asyncWriteHeaders
                <| context.Response.Headers
                <| response

            do!
                Writing.asyncWriteSpecialHeaders
                <| context.Response
                <| response

            context.Response.StatusCode <- statusCode

            logInfo "[%A] Response rendered, status code is %A" requestId context.Response.StatusCode
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

