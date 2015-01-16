namespace IntelliFactory.WebSharper.InterfaceGenerator.Tests

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let WIGtest =
        Class "WIGtest"
        |+> Static [
            "TupledFuncIn" => (T<int> * T<int> ^-> T<int>)?add ^-> T<int>    
            "TupledFuncOut" => T<unit> ^-> (T<int> * T<int> ^-> T<int>)
        ]

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.InterfaceGenerator.Tests" [
                 WIGtest
                 Resource "WIGTestJs" "WIGtest.js" |> AssemblyWide
            ]
        ]

open IntelliFactory.WebSharper.InterfaceGenerator

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
