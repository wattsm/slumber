namespace Slumber

open System
open System.Web
open Slumber.Framework

///Contains functions for running the default Slumber pipeline
module Pipeline = 

    ///Runs the standard Slumber pipeline asynchronously
    let asyncRun mode (context : HttpContextBase) = 

        let requestId = Guid.NewGuid ()

        logInfo "[%A] Pipeline begins for request to %A" requestId context.Request.Url.AbsoluteUri

        let execute = 
            start
            --> Bootstrap.asyncRun requestId mode
            --> Discovery.asyncRun
            --> Execution.asyncRun
            --| Render.asyncRun requestId context
    
        execute context.Request

    ///Runs the standard Slumber pipeline synchronously
    let run mode context = 
        asyncRun mode context
        |> Async.RunSynchronously