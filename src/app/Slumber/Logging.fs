namespace Slumber

open System
open System.Diagnostics
open Printf

///Functions and types used for logging information
[<AutoOpen>]
module Logging = 

    ///Union describing a log entry
    type LogEntry = 
        | Debug of String
        | Warning of String
        | Info of String
        | Error of (String * Exception option)

    ///Writes to the log output. Defaults to debug.
    let mutable private _write = 
        fun entry -> 

            let message, level = 
                match entry with
                | Debug msg -> msg, "Debug"
                | Warning msg -> msg, "Warn"
                | Info msg -> msg, "Info"
                | Error (msg, Some ex) -> String.Format ("{0} : {1}", msg, ex.Message), "Error"
                | Error (msg, _) -> msg, "Error"

            Debug.WriteLine (
                "{0} {1} {2} {3}", 
                (getThreadId ()), 
                DateTime.Now.ToString ("yyyy-MM-dd HH:mm:ss"),
                level,
                message
            )

    ///Sets the writer used to record log entries
    let setLogWriter writer = 
        _write <- writer

    ///Writes a log entry 
    let private log builder format =
        kprintf (builder >> _write) format

    ///Writes a debug entry in the log
    let logDebug format = 
        log (fun msg -> Debug (msg)) format

    ///Writes a warning entry in the log
    let logWarn format = 
        log (fun msg -> Warning (msg)) format

    ///Writes a info entry in the log
    let logInfo format= 
        log (fun msg -> Info (msg)) format

    ///Writes an error entry in the log
    let logError format = 
        log (fun msg -> Error (msg, None)) format

    ///Writes an exception entry in the log
    let logException ex format = 
        log (fun msg -> Error (msg, Some ex)) format



