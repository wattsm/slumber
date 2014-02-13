namespace Slumber.Owin

open System
open Slumber
open Owin
open Microsoft.Owin

///Contains common functions and types
[<AutoOpen>]
module Common =

    ///Contains functions and types relating to Owin HTTP
    [<AutoOpen>]
    module Http = 

        let [<Literal>] AppPathKey = "Owin.RequestPathBase"

        ///Contains helper functions
        [<AutoOpen>]
        module private Helpers = 

            ///TODO How do you handle multiple items per key? How are they delimited? By comma? Semicolon? 

            ///Parse a collection of key/values - e.g. query string, headers
            let parseKeyValues collection = 
                Seq.toList (seq {
                    for KeyValue (key, items) in collection do
                        if (Array.length items) = 1 then
                            yield (key, Array.head items)
                })

        ///Parse request URLs
        let parseUrls (context : IOwinContext) = 

            let url = context.Request.Uri

            let applicationPath =                 
                match (context.Get<String> AppPathKey) with
                | null -> String.Empty
                | value -> string value

            let root = 
                Uri (url.GetLeftPart (UriPartial.Authority), UriKind.Absolute)

            let path = 
                match (url.AbsolutePath.Substring (applicationPath.Length)) with
                | "" -> "/"
                | p -> p

            {
                Raw = url;                
                Path = path;
                Query = context.Request.Query |> parseKeyValues;
                BaseUrl = Uri (root, applicationPath);
            }

        ///Parse request payload
        let parsePayload (context : IOwinContext) = 

            let body = 
                if (context.Request.Body.Length > 0L) then
                    Some context.Request.Body
                else
                    None

            {
                Headers = context.Request.Headers |> parseKeyValues;
                Body = body;
            }

        ///Parses a request from an Owin context
        let parseRequest context requestId = 
            {
                Id = requestId;
                Url = (parseUrls context);
                Payload = (parsePayload context);
                Verb = context.Request.Method;
            }

        ///An IOutput wrapper for IOwinContext
        type OwinContextOutput (raw : IOwinContext) = 

            static member Create raw = 
                OwinContextOutput (raw) :> IOutput

            interface IOutput with

                member this.WriteBody bytes = 
                    async {                       
                        do! raw.Response.Body.AsyncWrite (List.toArray bytes)
                    }

                member this.WriteHeader key value = 
                    async {
                        if (String.same key Headers.ContentType) then
                            raw.Response.ContentType <- value
                        else                        
                            raw.Response.Headers.[key] <- value
                    }

                member this.SetStatusCode statusCode = 
                    async { 
                        raw.Response.StatusCode <- statusCode
                    }

        ///Contains Owin extension methods
        [<AutoOpen>]
        module Extensions = 

            ///Extensions to the IOwinContext interface
            type IOwinContext with
                member this.GetOutput () = OwinContextOutput.Create this
                member this.GetInput requestId = parseRequest this requestId
