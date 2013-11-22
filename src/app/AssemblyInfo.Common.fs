module AssemblyInfo

open System.Reflection

let [<Literal>] private Version = "0.9.0.0"

[<assembly: AssemblyProductAttribute ("Slumber")>]
[<assembly: AssemblyVersion (Version)>]
[<assembly: AssemblyFileVersion (Version)>]

do ()