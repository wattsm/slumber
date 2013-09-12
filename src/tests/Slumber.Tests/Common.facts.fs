namespace Slumber.Tests

open FsUnit
open Xunit
open Xunit.Extensions
open System
open System.IO
open System.Web
open System.Text
open Slumber

module ``Common facts`` =

    module ``Http facts`` = 

        open Http

        let [<Literal>] ModuleName = "Common.Http"

        [<AutoOpen>]
        module Helpers = 

            let getRequestTo (url : String) (app : String) (body : string) = 
                {
                    new HttpRequestBase () with

                        member this.Url = Uri (url, UriKind.Absolute)
                        member this.ApplicationPath = app
                        member this.HttpMethod = "POST"
                    
                        member this.QueryString = 
                            [
                                ("page", "5");
                            ]
                            |> List.toNvc

                        member this.Headers = 
                            [
                                ("Content-Type", MediaTypes.Text.Xml);
                                ("Accept", MediaTypes.Application.Json);
                            ]
                            |> List.toNvc

                        member this.InputStream = 

                            let bytes = 
                                if (String.IsNullOrWhiteSpace body) then
                                    Array.empty<Byte>
                                else
                                    Encoding.UTF8.GetBytes body

                            new MemoryStream (bytes) :> Stream


                }

            let getRequest (body : string) = 
                getRequestTo "http://localhost:8080/api/people" "/api" body

            let request = 
                getRequest "Hello, World"

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parseUrls function`` =

            let [<Fact>] ``Raw URL is copied across`` () =
            
                let urls = 
                    parseUrls request

                urls.Raw |> should equal request.Url

            let [<Fact>] ``Path is set correctly`` () =

                let urls = 
                    parseUrls request

                urls.Path |> should equal "/people"

            let [<Fact>] ``Forward slash is set for root path`` () =

                let urls = 
                    getRequestTo "http://localhost:8080/api" "/api" "Hello, World"
                    |> parseUrls

                urls.Path |> should equal "/"

            let [<Fact>] ``Query is set correctly`` () =

                let urls = 
                    parseUrls request

                urls.Query |> List.same [ ("page", "5"); ] |> should be True

            let [<Fact>] ``BaseUrl is set correctly`` () =
                
                let urls = 
                    parseUrls request

                urls.BaseUrl |> should equal (Uri ("http://localhost:8080/api", UriKind.Absolute))

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parsePayload function`` = 

            let [<Fact>] ``Headers are set correctly`` () =

                let payload = 
                    parsePayload request

                payload.Headers |> List.same [ ("Content-Type", MediaTypes.Text.Xml); ("Accept", MediaTypes.Application.Json); ] |> should be True

            let [<Fact>] ``Body input stream is copied when not empty`` () =

                let payload = 
                    parsePayload request

                use reader = 
                    new StreamReader (Option.get payload.Body)

                let body = 
                    reader.ReadToEnd ()

                body |> should equal "Hello, World"

            let [<Fact>] ``Body input stream is not copied when empty`` () =

                let payload = 
                    getRequest String.Empty
                    |> parsePayload

                payload.Body |> should be None'<Stream>                    

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``parseRequest function`` = 

            let [<Fact>] ``Verb is set correctly`` () =

                let request' = 
                    parseRequest request Guid.Empty

                request'.Verb |> should equal "POST"

            let [<Fact>] ``Request ID is set correctly`` () =

                let id = Guid.NewGuid ()
                let request' = parseRequest request id

                request'.Id |> should equal id

        [<Trait (Traits.Names.Module, ModuleName)>]
        module ``createAbsoluteUri function`` = 

            [<Theory>]
            [<InlineData ("http://localhost", "api", "http://localhost/api")>]
            [<InlineData ("http://localhost", "/api", "http://localhost/api")>]
            [<InlineData ("http://localhost/", "api", "http://localhost/api")>]
            [<InlineData ("http://localhost/", "/api", "http://localhost/api")>]
            [<InlineData ("http://localhost/app", "api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app", "/api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app/", "api", "http://localhost/app/api")>]
            [<InlineData ("http://localhost/app/", "/api", "http://localhost/app/api")>]
            let ``Returns correct URL`` baseUrl relativeUrl expectedUrl = 

                let baseUrl' = Uri (baseUrl, UriKind.Absolute)
                let actualUrl = createAbsoluteUri baseUrl' relativeUrl

                actualUrl.AbsoluteUri |> should equal expectedUrl

        module ``Headers facts`` =

            let [<Literal>] ModuleName = "Common.Http.Headers"

            let headers =
                [
                    ("Content-Type", MediaTypes.Text.Xml);
                    ("Authorization", String.Empty);
                ]

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getValue function`` = 

                let [<Fact>] ``Correct value is returned when header is present`` () =
                    headers
                    |> Headers.getValue "Content-Type"
                    |> should be (Some' MediaTypes.Text.Xml)

                let [<Fact>] ``None is returned when header is not present`` () = 
                    headers
                    |> Headers.getValue "Accept"
                    |> should be None'<String>

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getNonEmptyValue function`` =

                let [<Fact>] ``Correct value is returned when the header is present and non-empty`` () =
                    headers
                    |> Headers.getNonEmptyValue "Content-Type"
                    |> should be (Some' MediaTypes.Text.Xml)

                let [<Fact>] ``None is returned when the header is present and empty`` () =
                    headers
                    |> Headers.getNonEmptyValue "Authorization"
                    |> should be None'<String>

                let [<Fact>] ``None is returned when the header is not present`` () =
                    headers
                    |> Headers.getNonEmptyValue "Accept"
                    |> should be None'<String>