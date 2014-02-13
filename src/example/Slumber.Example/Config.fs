namespace Slumber.Example

open System
open Slumber
open Slumber.Framework
open Slumber.Setup
open Slumber.IO.DataContract

module Config = 

    type Description () = 

        let authenticate (request : Request) = 

            let username = 
                request.Payload.Headers
                |> Headers.getValue "Authorization"

            match username with
            | Some username' -> Allow (Some { Id = username'; Properties = []; })
            | _ -> Deny

        let createRepository () =

            //NOTE You can uncomment the SQL CE line below to use a very simple, very
            //fragile SQL CE repository.

            //let repository = Repository.SqlCe.Repository.Create "Default"
            let repository = Repository.InMemory.Repository.Create ()

            repository.Setup ()

            repository

        interface IContainerDescription with

            member this.Describe baseUrl =

                (**
                    NOTE You may be tempted looking at the code below to create a function to partially apply the operation functions, e.g.
                    let get' f = get (f repository). Be wary when doing this, however, as the partially applied function's argumenst will not have the
                    same names as the original function's - so Slumber will no longer populate values from URI segments.
                    **)

                let repository = createRepository ()

                containerAt (relativeUri baseUrl "/")
                |> authenticatedBy authenticate true
                |> with' (
                        endpointAt "/"
                        |> named "service-catalog"
                        |> supporting (public' get Startup.getCatalog)
                    )            
                |> with' (
                        endpointAt "/people"
                        |> named "people"
                        |> supporting (public' get (People.getPeople repository))
                        |> supporting (post (People.addPerson repository))
                    )
                |> with' (
                        endpointAt "/people/{id}"
                        |> named "person"
                        |> supporting (public' get (People.getPerson repository))
                        |> supporting (delete (People.deletePerson repository))
                        |> supporting (put (People.updatePerson repository))
                    )
                |> all (public' options Common.options)
                |> reading MediaTypes.Application.Json Json.read
                |> writing MediaTypes.Application.Json Json.write
                |> reading MediaTypes.Text.Xml Xml.read
                |> writing MediaTypes.Text.Xml Xml.write
                |> forwarding MediaTypes.Text.Html MediaTypes.Text.Xml