module AssemblyInfo.Owin

open System.Reflection
open Microsoft.Owin

[<assembly: OwinStartup(typeof<Slumber.Owin.Host>)>]

do ()