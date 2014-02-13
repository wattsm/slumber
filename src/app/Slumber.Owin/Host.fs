namespace Slumber.Owin

open System
open System.Threading.Tasks
open Slumber.Framework
open Microsoft.Owin
open Owin

///An Owin host that runs the standard Slumber pipeline
type Host () = 

    member this.Configuration (app : IAppBuilder) = 
        app.Run (fun (context : IOwinContext) ->
            Task.Run (fun () ->

                let id = Guid.NewGuid ()
                let input = context.GetInput id
                let output = context.GetOutput ()

                Slumber.Pipeline.run
                <| Implicit
                <| input
                <| output
            )
        )