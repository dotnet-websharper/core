[<AutoOpen>]
module internal WebSharper.Compiler.CSharp.Utilities

module Option =
    let ofObj o = 
        if obj.ReferenceEquals(o, null) then None else Some o