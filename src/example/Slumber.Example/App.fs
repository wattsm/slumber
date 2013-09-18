namespace Slumber.Example

open System
open System.Web
open System.IO
open log4net
open log4net.Config
open Slumber.Logging

type App () = 
    inherit HttpApplication ()

    let [<Literal>] LogName = "Slumber.log"

    static let mutable _initialised = false

    member this.Application_BeginRequest (_ : obj, _ : EventArgs) =
        if (not _initialised) then

            _initialised <- true

            XmlConfigurator.Configure (
                new FileInfo ( 
                    Path.Combine (
                        AppDomain.CurrentDomain.BaseDirectory,
                        "log4net.config"
                    )
                )
            )
            |> ignore

            let log = 
                LogManager.GetLogger LogName

            setLogWriter (fun entry ->
                match entry with
                | Debug msg -> 
                    if log.IsDebugEnabled then
                        log.Debug (msg)
                | Info msg ->
                    if log.IsInfoEnabled then
                        log.Info (msg)
                | Warning msg ->
                    if log.IsWarnEnabled then
                        log.Warn (msg)
                | Error (msg, err) ->
                    if log.IsErrorEnabled then
                        match err with 
                        | Some ex ->  log.Error (msg, ex)
                        | _ -> log.Error (msg)
            )