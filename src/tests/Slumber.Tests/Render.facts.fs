namespace Slumber.Tests

open System
open System.IO
open System.Text
open System.Collections.Specialized
open FsUnit
open Xunit
open Xunit.Extensions
open Slumber

module ``Render facts`` =
    
    open Render
    open Http

    let [<Literal>] ModuleName = "Render"

    module ``Writing facts`` = 

        open Writing

        let [<Literal>] ModuleName = "Render.Writing"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWriteBody function`` = 

            [<AutoOpen>]
            module Helpers =  

                let writeBody bytes stream = 
                    asyncWriteBody stream bytes
                    |> Async.RunSynchronously

                    stream

                let getStream () = 
                    new MemoryStream () :> Stream

                let getBytes (message : String) = 
                    message
                    |> Encoding.UTF8.GetBytes
                    |> Array.toList
                    |> Some

                let streamContains message (stream : Stream) = 

                    stream.Position <- 0L

                    use reader = 
                        new StreamReader (stream)

                    reader.ReadToEnd () = message

                let getLength (stream : Stream) = 
                    stream.Length

            let [<Fact>] ``Nothing written to stream when no body present`` () =
                getStream ()
                |> writeBody None
                |> getLength
                |> should equal 0L

            let [<Fact>] ``Nothing written to stream when body is zero length`` () =
                getStream ()
                |> writeBody (Some [])
                |> getLength
                |> should equal 0L

            let [<Fact>] ``Body is written correctly to stream`` () =
                let message = "Hello, World"
                in
                    getStream ()
                    |> writeBody (getBytes message)
                    |> streamContains message
                    |> should be True

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWriteHeaders function`` = 

            [<AutoOpen>]
            module Helpers = 

                let writeHeaders args headers = 
                    asyncWriteHeaders headers args
                    |> Async.RunSynchronously

                    headers

                let emptyHeaders () = 
                    NameValueCollection ()

                let getResponse length contentType = 
            
                    let bytes = 
                        List.init length byte

                    {
                        ResponseType = Resource (StatusCodes.Ok, bytes);
                        ContentType = contentType;
                        CustomHeaders = [];
                    }

                let withHeader key value response = 
                    { response with CustomHeaders = (key, value) :: response.CustomHeaders; }

                let getHeader (key : String) (headers : NameValueCollection) = 
                    match (headers.[key]) with
                    | null -> None
                    | value -> Some value

            [<Theory>]
            [<InlineData (0)>]
            [<InlineData (30)>]
            let ``Content-Length header is set correctly`` length =
                emptyHeaders ()
                |> writeHeaders (getResponse length None)
                |> getHeader Headers.ContentLength
                |> should be (Some' (string length))

            let [<Fact>] ``Content-Type header is omitted set when specified`` () =
                let contentType = MediaTypes.Text.Xml
                in
                    emptyHeaders ()
                    |> writeHeaders (getResponse 0 (Some contentType))
                    |> getHeader Headers.ContentType
                    |> should be None'<String>

            let [<Fact>] ``Content-Type header is omitted when not specified`` () =
                emptyHeaders () 
                |> writeHeaders (getResponse 0 None)
                |> getHeader Headers.ContentType
                |> should be None'<String>

            let [<Fact>] ``Custom headers are set`` () =
                emptyHeaders ()
                |> writeHeaders (
                        getResponse 0 None
                        |> withHeader "custom" "value"
                    )
                |> getHeader "custom"
                |> should be (Some' "value")

            let [<Fact>] ``Custom Content-Type header is omitted`` () =
                emptyHeaders ()
                |> writeHeaders (
                        getResponse 0 None
                        |> withHeader Headers.ContentType MediaTypes.Text.Xml
                    )
                |> getHeader Headers.ContentType
                |> should be None'<String>

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``asyncWriteSpecialHeaders function`` = 

            [<AutoOpen>]
            module Helpers = 

                let writeSpecialHeaders resp args = 
                    asyncWriteSpecialHeaders resp args
                    |> Async.RunSynchronously

                type FakeResponse () = 
                    inherit System.Web.HttpResponseBase ()

                        override val ContentType = String.Empty with get, set

                let getArgs customCT responseCT = 
                    {
                        ResponseType = (StatusCode 200);
                        ContentType = responseCT;
                        CustomHeaders = 
                            match customCT with
                            | Some ct -> [ (Headers.ContentType, ct); ]
                            | _ -> []
                    }

            let [<Fact>] ``Content-Type specified in custom headers is set if present`` () =
            
                let response = 
                    FakeResponse ()

                let isCorrect () = 
                    response.ContentType = MediaTypes.Text.Xml

                getArgs (Some MediaTypes.Text.Xml) None
                |> writeSpecialHeaders response
                |> isCorrect
                |> should be True

            let [<Fact>] ``Content-Type specified by response is set if present`` () =
            
                let response = 
                    FakeResponse ()

                let isCorrect () = 
                    response.ContentType = MediaTypes.Text.Xml

                getArgs None (Some MediaTypes.Text.Xml)
                |> writeSpecialHeaders response
                |> isCorrect
                |> should be True


            let [<Fact>] ``Content-Type specified in custom headers takes precedence`` () =
            
                let response = 
                    FakeResponse ()

                let isCorrect () = 
                    response.ContentType = "text/custom"

                getArgs (Some "text/custom") (Some "text/writer")
                |> writeSpecialHeaders response
                |> isCorrect
                |> should be True

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