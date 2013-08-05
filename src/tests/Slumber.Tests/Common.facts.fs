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

            let getRequest (body : string) = 
                {
                    new HttpRequestBase () with

                        member this.Url = Uri ("http://localhost:8080/api/people", UriKind.Absolute)
                        member this.ApplicationPath = "/api"
                        member this.HttpMethod = "POST"
                    
                        member this.QueryString = 
                            [
                                ("page", "5");
                            ]
                            |> List.toNvc

                        member this.Headers = 
                            [
                                ("Content-Type", "text/xml");
                                ("Accept", "application/json");
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

                payload.Headers |> List.same [ ("Content-Type", "text/xml"); ("Accept", "application/json"); ] |> should be True

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
        module ``createAbsoluteUriFrom function`` = 

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
                let actualUrl = createAbsoluteUriFrom baseUrl' relativeUrl

                actualUrl.AbsoluteUri |> should equal expectedUrl

        module ``Headers facts`` =

            open Http.Headers

            let [<Literal>] ModuleName = "Common.Http.Headers"

            let payload = 
                {
                    Headers = 
                        [
                            ("Content-Type", "text/xml");
                            ("Authorization", String.Empty);
                        ];
                    Body = None;
                }

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getHeaderValue function`` = 

                let [<Fact>] ``Correct value is returned when header is present`` () =
                    payload
                    |> getHeaderValue "Content-Type"
                    |> should be (Some' "text/xml")

                let [<Fact>] ``None is returned when header is not present`` () = 
                    payload
                    |> getHeaderValue "Accept"
                    |> should be None'<String>

            [<Trait (Traits.Names.Module, ModuleName)>]
            module ``getNonEmptyHeaderValue function`` =

                let [<Fact>] ``Correct value is returned when the header is present and non-empty`` () =
                    payload
                    |> getNonEmptyHeaderValue "Content-Type"
                    |> should be (Some' "text/xml")

                let [<Fact>] ``None is returned when the header is present and empty`` () =
                    payload
                    |> getNonEmptyHeaderValue "Authorization"
                    |> should be None'<String>

                let [<Fact>] ``None is returned when the header is not present`` () =
                    payload
                    |> getNonEmptyHeaderValue "Accept"
                    |> should be None'<String>