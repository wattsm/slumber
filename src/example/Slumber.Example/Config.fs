namespace Slumber.Example

open System
open Slumber
open Slumber.Configuration
open Slumber.Common.Http
open Slumber.Common.Http.Headers
open Slumber.Setup
open Slumber.IO.DataContract

module Config = 

    type Description () = 

        let authenticate (request : Request) = 

            let username = 
                request.Payload
                |> getHeaderValue "Authorization"

            match username with
            | Some username' -> Allow (Some { Id = username'; Properties = []; })
            | _ -> Deny

        interface IContainerDescription with
        
            member this.Describe baseUrl =
                containerAt (relativeUri baseUrl "/")
                |> authenticatedBy authenticate
                |> with' (
                        endpointAt "/"
                        |> supporting (public' get Startup.getCatalog)
                    )            
                |> with' (
                        endpointAt "/people"
                        |> supporting (public' get People.getPeople)
                        |> supporting (post People.addPerson)
                    )
                |> with' (
                        endpointAt "/people/{id}"
                        |> supporting (public' get People.getPerson)
                        |> supporting (delete People.deletePerson)
                        |> supporting (put People.updatePerson)
                    )
                |> reading MediaTypes.Application.Json Json.read
                |> writing MediaTypes.Application.Json Json.write
                |> reading MediaTypes.Text.Xml Xml.read
                |> writing MediaTypes.Text.Xml Xml.write
                |> forwarding MediaTypes.Text.Html MediaTypes.Text.Xml