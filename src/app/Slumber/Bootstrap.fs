namespace Slumber

open System
open System.Web
open System.Reflection
open System.IO
open HandyFS.Types
open Slumber.Common.Http
open Slumber.Configuration
open Slumber.Discovery
open Slumber.Framework
open Slumber.Setup

///Contains functions used to parse the raw HTTP request and initialise Slumber's configuration
module Bootstrap = 

    ///Instantiates and queries the first container description that can be found in the /bin/ folder 
    let private implicitConfiguration = 
        lazy (

            ///TODO Tidy up / abstract this function

            let tryLoadAssembly path = 
                try 
                    path
                    |> File.ReadAllBytes
                    |> Assembly.Load
                    |> Some
                with
                | _ -> None

            let tryGetTypes (assembly : Assembly) = 
                try
                    assembly.GetTypes ()
                with
                | :? ReflectionTypeLoadException -> Array.empty

            let tryCreateDescription (type' : Type) = 
                try
                    Some ((Activator.CreateInstance type') :?> IContainerDescription)
                with
                | _ -> None

            let binPath = 
                Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "bin")

            let types = 
                Directory.GetFiles (binPath, "*.dll", SearchOption.TopDirectoryOnly)
                |> Array.Parallel.choose tryLoadAssembly
                |> Array.Parallel.collect tryGetTypes
                |> Array.filter (implements typeof<IContainerDescription>)

            if (Array.isEmpty types) then
                invalidOp "Implicit configuration requires a type that implements IContainerDescription but none could be found."

            else
                match (Array.tryPick tryCreateDescription types) with
                | Some description -> description.Describe ()
                | _ -> invalidOp "No type implementing IContainerDescription could be instantiated." //TODO More appropriate exception type?

        )

    ///Runs the bootstrapping phase asynchronously
    let asyncRun requestId mode (request : HttpRequestBase) =
        async {

            let mode' = 
                match mode with
                | Explicit _ -> "explicit"
                | Implicit -> "implicit"
                | Mixed _ -> "mixed"

            logInfo "[%A] Bootstrapping using %A" requestId mode'

            let container = 
                match mode with
                | Explicit container' -> container'
                | Implicit -> implicitConfiguration.Force ()
                | Mixed modifier -> implicitConfiguration.Force () |> modifier

            return 
                {
                    Request = (parseRequest request requestId);
                    Container = container;
                }
                |> Running
        }

    ///Runs the bootstrapping phase synchronously
    let run requestId mode request = 
        asyncRun requestId mode request
        |> Async.RunSynchronously
    
        
