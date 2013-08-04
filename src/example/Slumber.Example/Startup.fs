namespace Slumber.Example

open System
open System.Runtime.Serialization

module Startup = 

    open Slumber    

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

    let getCatalog (ctx : OperationContext) = 
        {
            Self = ctx.Metadata.Url.BaseUrl.ToString ();
            Services = 
                [
                    {
                        Name = "get-catalog";
                        Url = ctx.Metadata.Url.BaseUrl.ToString ();
                    };
                    {
                        Name = "get-people";
                        Url = (Uri (ctx.Metadata.Url.BaseUrl, "people")).ToString ();
                    };
                ];
        }

