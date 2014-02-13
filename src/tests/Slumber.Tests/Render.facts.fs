namespace Slumber.Tests

open System
open System.IO
open System.Text
open System.Collections.Specialized
open FsUnit
open Xunit
open Xunit.Extensions
open Foq
open Slumber

module ``Render facts`` =
    
    open Render
    open Http

    let [<Literal>] ModuleName = "Render"    

    module ``Writing facts`` = 

        open Writing

        let [<Literal>] ModuleName = "Render.Writing"

        [<AutoOpen>]
        module Helpers = 

            let getOutput () = 
                Mock<IOutput>()
                    .Setup(fun o -> <@ o.WriteBody (any ()) @>).Returns(async { return () })    
                    .Setup(fun o -> <@ o.WriteHeader (any ()) (any ()) @>).Returns(async { return () })
                    .Setup(fun o -> <@ o.SetStatusCode (any ()) @>).Returns(async { return () })
                    .Create()

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWriteBody function`` = 

            [<AutoOpen>]
            module Helpers = 

                let getResponse body = 

                    let responseType = 
                        match body with 
                        | Some bytes -> Resource (200, bytes)
                        | _ -> StatusCode 200

                    {
                        ResponseType = responseType;
                        ContentType = None;
                        CustomHeaders = [];
                    }

            let [<Fact>] ``Nothing written to stream when no body present`` () =
                
                let output = getOutput ()
                let response = getResponse None

                asyncWriteBody output response
                |> Async.RunSynchronously

                verify <@ output.WriteBody (any ()) @> never

            let [<Fact>] ``Nothing written to stream when body is zero length`` () =
                
                let output = getOutput ()
                let response = getResponse (Some [])

                asyncWriteBody output response
                |> Async.RunSynchronously

                verify <@ output.WriteBody (any ()) @> never

            let [<Fact>] ``Body is written correctly to stream`` () =
                
                let output = getOutput ()
                let body = "Hello, World" |> Encoding.UTF8.GetBytes |> Array.toList
                let response = getResponse (Some body)

                asyncWriteBody output response
                |> Async.RunSynchronously

                verify <@ output.WriteBody body @> once

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWriteHeaders function`` = 

            [<AutoOpen>]
            module Helpers = 

                let getResponse body contentType headers = 
                    
                    let responseType =
                        match body with
                        | Some bytes -> Resource (200, bytes)
                        | _ -> StatusCode (200)

                    {
                        ResponseType = responseType;
                        ContentType = contentType;
                        CustomHeaders = headers;
                    }

            [<Theory>]
            [<InlineData (0)>]
            [<InlineData (30)>]
            let ``Content-Length header is set correctly for resource response types`` length =
                
                let body = List.init length (fun _ -> byte 0)
                let response = getResponse (Some body) None []
                let output = getOutput ()
                let length' = string length

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader Headers.ContentLength length' @> once

            let [<Fact>] ``Content-Length header is set correctly for status response types`` () = 
                
                let response = getResponse None None []
                let output = getOutput ()

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader Headers.ContentLength "0" @> once

            let [<Fact>] ``Content-Type header is set correctly when explicitly specified`` () =
                
                let response = getResponse None (Some "text/xml") [ (Headers.ContentType, "application/json") ]
                let output = getOutput ()

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader Headers.ContentType "application/json" @> once

            let [<Fact>] ``Content-Type header is set correcrtly when not explicitly specified`` () =
                
                let response = getResponse None (Some "text/xml") []
                let output = getOutput ()

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader Headers.ContentType "text/xml" @> once

            let [<Fact>] ``Content-Type header is ommitted when not explicitly set and not specified in response`` () =
                
                let response = getResponse None None []
                let output = getOutput ()

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader Headers.ContentType (any ()) @> never

            let [<Fact>] ``Custom headers are set`` () =
                
                let response = getResponse None None [ ("Custom", "Value") ]
                let output = getOutput ()

                asyncWriteHeaders output response
                |> Async.RunSynchronously

                verify <@ output.WriteHeader "Custom" "Value" @> once

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWrite function`` =

            [<AutoOpen>]
            module Helpers = 
                
                let getResponse isStatusOnly = 
                    
                    let responseType =
                        if isStatusOnly then
                            StatusCode 418
                        else
                            Resource (418, [])

                    {
                        ResponseType = responseType;
                        CustomHeaders = [];
                        ContentType = None;
                    }

            let [<Fact>] ``Status code is set correctly for resource responses`` () = 
                
                let response = getResponse false
                let output = getOutput ()
                let id = Guid.NewGuid ()

                asyncWrite id output response
                |> Async.RunSynchronously

                verify <@ output.SetStatusCode 418 @> once

            let [<Fact>] ``Status code is set correctly for status code responses`` () = 
                
                let response = getResponse true
                let output = getOutput ()
                let id = Guid.NewGuid ()

                asyncWrite id output response
                |> Async.RunSynchronously

                verify <@ output.SetStatusCode 418 @> once

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``asyncGetResponse facts`` =

        open Framework

        let getResponse state = 
            asyncGetResponse Guid.Empty state
            |> Async.RunSynchronously

        let getStatusCode response = 
            match response.ResponseType with
            | StatusCode statusCode -> statusCode
            | Resource (statusCode, _) -> statusCode

        let [<Fact>] ``Running state returns HTTP 500`` () =
            Running (10) |> getResponse |> getStatusCode |> should equal StatusCodes.InternalServerError

        let [<Fact>] ``Stopped (exception) state returns HTTP 500`` () = 
            Stopped (Exception (new NotSupportedException ())) |> getResponse |> getStatusCode |> should equal StatusCodes.InternalServerError

        let [<Fact>] ``Stopped (completed) state returns response`` () = 

            let response = 
                { Response.Empty with ResponseType = StatusCode (418); }

            Stopped (Completed response) |> getResponse |> getStatusCode |> should equal 418