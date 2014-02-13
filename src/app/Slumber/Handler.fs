namespace Slumber

open System
open System.Web
open Slumber.Framework

///Default implementation of an HTTP handler for Slumber
type SlumberHandler () =
    interface IHttpHandler with

        member this.ProcessRequest (context : HttpContext) = 

            let id = Guid.NewGuid ()
            let wrapped = HttpContextWrapper (context)
            let input = wrapped.GetInput id
            let output = wrapped.GetOutput ()

            Pipeline.run
            <| Implicit
            <| input
            <| output

        member this.IsReusable = 
            false
            

