namespace Slumber.Example

open System
open System.Runtime.Serialization

module Startup = 

    open Slumber    
    open Slumber.Common.Http

    [<AutoOpen>]
    module Model = 

        [<DataContract (Name = "service")>] 
        type Service = {

            [<field: DataMember (Name = "name")>]
            Name : String;

            [<field: DataMember (Name = "url")>]
            Url : String;
        }

        [<DataContract (Name = "service-catalog")>]
        type ServiceCatalog = {

            [<field: DataMember (Name = "self")>]
            Self : String;

            [<field: DataMember (Name = "services")>]
            Services : Service seq;
        }
    
    let getCatalog (meta : OperationMetadata) = 
        let baseUrl = meta.Request.Url.BaseUrl
        in
            {
                Self = baseUrl.AbsoluteUri;
                Services = 
                    [
                        {
                            Name = "get-catalog";
                            Url = baseUrl.AbsoluteUri;
                        };
                        {
                            Name = "get-people";
                            Url = string (createAbsoluteUri baseUrl "people");
                        };
                    ];
            }

