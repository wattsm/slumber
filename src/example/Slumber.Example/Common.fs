namespace Slumber.Example

open System
open Slumber
open Slumber.Common.Http
open Slumber.Framework
open Slumber.Framework.Core.Containers
open HandyFS

module Common = 

    let options (meta : OperationMetadata) = 

        let container = 
            ImplicitConfiguration.get meta.Request.Url.BaseUrl

        let endpoint = 
            container
            |> tryGetEndpointByName meta.EndpointName
            |> Option.get

        let verbs = 
            endpoint.Bindings
            |> List.map (fun binding -> binding.Verb)
            |> List.toArray
            
        let accept = 
            String.Join (",", verbs)

        { 
            OperationResult.Empty 
            with
                StatusCode = (Some StatusCodes.Ok);
                Headers = [ (Headers.Accept, accept); ];
        }