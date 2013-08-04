namespace Slumber

open System
open System.Web
open Slumber.Configuration

///Default implementation of an HTTP handler for Slumber
type SlumberHandler () =
    interface IHttpHandler with

        member this.ProcessRequest (context : HttpContext) = 
            Pipeline.run
            <| Implicit
            <| HttpContextWrapper (context)

        member this.IsReusable = 
            false
            

