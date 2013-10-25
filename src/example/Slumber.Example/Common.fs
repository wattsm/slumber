namespace Slumber.Example

open System
open Slumber
open Slumber.Framework
open Slumber.Framework.Core.Containers

module Common = 

    let options (meta : OperationMetadata) = 

        let endpoint = 
            meta.ContainerUrl
            |> ImplicitConfiguration.get
            |> tryGetEndpointByName meta.EndpointName
            |> Option.get

        let verbs = 
            endpoint.Bindings
            |> List.map (fun binding -> binding.Verb)
            
        let accept = 
            String.Join (",", verbs)

        OperationResult.StatusOnly (
            StatusCodes.Ok,
            [ (Headers.Accept, accept) ]
        )