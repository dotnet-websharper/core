module IntelliFactory.WebSharper.JavaScript.Definition

open IntelliFactory.WebSharper.InterfaceGenerator
open IntelliFactory.WebSharper.JavaScript

let Assembly =
    Assembly [
        yield! Ecma.Definition.Namespaces
        yield! Dom.Definition.Namespaces
        yield! Html5.Definition.Namespaces
    ]

[<Sealed>]
type DomExtension() =
    interface IExtension with
        member x.Assembly = Assembly

[<assembly: Extension(typeof<DomExtension>)>]
do ()
