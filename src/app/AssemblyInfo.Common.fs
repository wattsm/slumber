module AssemblyInfo.Common

open System.Reflection

let [<Literal>] private Version = "0.9.0.2"

[<assembly: AssemblyProductAttribute ("Slumber")>]
[<assembly: AssemblyVersion (Version)>]
[<assembly: AssemblyFileVersion (Version)>]

do ()