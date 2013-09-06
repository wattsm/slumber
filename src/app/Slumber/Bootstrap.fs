namespace Slumber

open System
open System.Web
open Slumber.Framework
open Slumber.Discovery
open Slumber.Common.Http

///Contains functions used to parse the raw HTTP request and initialise Slumber's configuration
module Bootstrap = 

    ///Runs the bootstrapping phase asynchronously
    let asyncRun requestId mode (request : HttpRequestBase) =
        async {

            let mode' = 
                match mode with
                | Explicit _ -> "explicit"
                | Implicit -> "implicit"
                | Mixed _ -> "mixed"

            logInfo "[%A] Bootstrapping using %A" requestId mode'

            let request' = 
                parseRequest request requestId

            let container = 
                match mode with
                | Explicit container' -> container'
                | Implicit -> ImplicitConfiguration.get request'.Url.BaseUrl
                | Mixed modifier -> 
                    request'.Url.BaseUrl
                    |> ImplicitConfiguration.get
                    |> modifier

            return 
                {
                    Request = request';
                    Container = container;
                }
                |> Running
        }

    ///Runs the bootstrapping phase synchronously
    let run requestId mode request = 
        asyncRun requestId mode request
        |> Async.RunSynchronously
    
        
