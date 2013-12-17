namespace Slumber.Example

open System
open System.Text.RegularExpressions
open System.Runtime.Serialization

module Startup = 

    open Slumber    
    open Slumber.Framework
    open Slumber.Common.Http

    [<AutoOpen>]
    module Model = 

        [<DataContract (Name = "service", Namespace = "")>] 
        type Service = {
            [<field: DataMember (Name = "name")>] Name : String;
            [<field: DataMember (Name = "url")>] Url : String;
        }

        [<DataContract (Name = "service-catalog", Namespace = "")>]
        type ServiceCatalog = {
            [<field: DataMember (Name = "self")>] Self : String;
            [<field: DataMember (Name = "services")>] Services : Service seq;
        }
    
    let getCatalog (meta : OperationMetadata) = 

        let baseUrl = meta.ContainerUrl
        let container = ImplicitConfiguration.get baseUrl

        let services = 
            container.Endpoints
            |> List.filter (fun endpoint -> not (Regex.IsMatch (endpoint.Template, "{.+?}"))) //Display only top level, non-parameterised endpoints
            |> List.map (fun endpoint ->

                    let url = 
                        match endpoint.Template with
                        | "/" -> string baseUrl
                        | _ -> string (createAbsoluteUri baseUrl endpoint.Template)

                    {
                        Name = endpoint.Name;
                        Url = url;
                    }
                )

        {
            Self = baseUrl.AbsoluteUri;
            Services = services;
        }

