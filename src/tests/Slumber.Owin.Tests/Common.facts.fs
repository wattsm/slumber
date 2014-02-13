namespace Slumber.Owin.Tests

open System
open System.Text
open System.IO
open System.Linq
open System.Collections.Generic
open FsUnit.Xunit
open Xunit
open Foq
open Owin
open Microsoft.Owin
open Slumber.Owin

module ``Common facts`` = 

    let [<Literal>] ModuleName = "Owin.Common"

    [<AutoOpen>]
    module Helpers = 

        type FakeQueryString () = 
            inherit Dictionary<String, String array> ()            

            static member Create items = 
                
                let queryString = FakeQueryString ()

                for (key, value) in items do
                    queryString.Add (key, [| value |])

                queryString :> IReadableStringCollection

            interface IReadableStringCollection with

                member this.Get key = 
                    match this.[key] with
                    | null | [||] -> null
                    | values -> values.[0]

                member this.GetValues key = 
                    this.[key].ToList () :> IList<String>

                member this.Item
                    with get key = 
                        match this.[key] with
                        | null | [||] -> null
                        | values -> values.[0]

        let getContext url (appPath : String) body =

            let stream = 
                
                let bytes = 
                    if (String.IsNullOrWhiteSpace body) then
                        Array.empty<byte>
                    else
                        Encoding.UTF8.GetBytes body

                new MemoryStream (bytes) :> Stream

            let uri = Uri (url, UriKind.Absolute)

            let queryString = 
                [ ("page", "5") ]
                |> FakeQueryString.Create

            let headers = 
                [ 
                    ("Content-Type", [| "text/xml" |]);
                    ("Accept", [| "application/json" |]);
                ]
                |> dict

            let environment = 
                [ (AppPathKey, box appPath) ]
                |> dict

            let request = 
                Mock<IOwinRequest>()
                    .Setup(fun r -> <@ r.Uri @>).Returns(uri)
                    .Setup(fun r -> <@ r.Query @>).Returns(queryString)
                    .Setup(fun r -> <@ r.Environment @>).Returns(environment)
                    .Setup(fun r -> <@ r.Body @>).Returns(stream)
                    .Setup(fun r -> <@ r.Headers @>).Returns(HeaderDictionary (headers))
                    .Create()

            Mock<IOwinContext>()
                .Setup(fun c -> <@ c.Request @>).Returns(request)
                .Setup(fun c -> <@ c.Get<String> AppPathKey @>).Returns(appPath)
                .Create()

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``parseUrls function`` = 

        let context = 
            getContext "http://localhost:8080/api/people" "/api" "Hello, World"

        let [<Fact>] ``Raw URL is copied across`` () = 
            
            let urls = parseUrls context

            urls.Raw |> should equal context.Request.Uri

        let [<Fact>] ``Path is set correctly`` () =
            
            let urls = parseUrls context

            urls.Path |> should equal "/people"

        let [<Fact>] ``Forward slash is set for root path`` () = 
            
            let context' = getContext "http://localhost:8080/api" "/api" "Hello, World"
            let urls = parseUrls context'
            
            urls.Path |> should equal "/"

        let [<Fact>] ``Query is set correctly`` () = 
            
            let urls = parseUrls context

            urls.Query |> List.same [ ("page", "5") ] |> should be True

        let [<Fact>] ``BaseUrl is set correctly`` () = 
            
            let urls = parseUrls context
            let baseUrl = Uri ("http://localhost:8080/api", UriKind.Absolute)

            urls.BaseUrl |> should equal baseUrl

    [<Trait (Traits.Names.Module, ModuleName)>]
    module ``parsePayload function`` = 

        let [<Fact>] ``Headers are copied correctly`` () = 
        
            let context = getContext "http://localhost:8080/api/people" "/api" "Hello, World"    
            let payload = parsePayload context
            let expected = [ ("Content-Type", "text/xml"); ("Accept", "application/json"); ]

            payload.Headers |> List.same expected |> should be True

        let [<Fact>] ``Body is copied when not empty`` () = 
            
            let context = getContext "http://localhost:8080/api/people" "/api" String.Empty
            let payload = parsePayload context

            payload.Body |> Option.isNone |> should be True

        let [<Fact>] ``Body is not copied when empty`` () = 
            
            let context = getContext "http://localhost:8080/api/people" "/api" "Hello, World"
            let payload = parsePayload context

            payload.Body |> Option.isSome |> should be True

            let stream = payload.Body |> Option.get

            use reader = new StreamReader (stream)
            let body = reader.ReadToEnd ()

            body |> should equal "Hello, World"

    module ``OwinContextOutput facts`` = 

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``SetStatusCode function`` = 

            let [<Fact>] ``Sets response status code correctly`` () = 
                
                let response = Mock.Of<IOwinResponse> ()

                let context = 
                    Mock<IOwinContext>()
                        .Setup(fun c -> <@ c.Response @>).Returns(response)
                        .Create()

                let wrapped = 
                    OwinContextOutput.Create context

                wrapped.SetStatusCode 418
                |> Async.RunSynchronously

                verify <@ response.StatusCode <- 418 @> once