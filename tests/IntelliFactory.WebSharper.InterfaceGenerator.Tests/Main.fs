namespace IntelliFactory.WebSharper.InterfaceGenerator.Tests

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let JustX =
        Pattern.Config "JustX" {
            Required = [ "x", T<int> ]
            Optional = []
        }

    let WIGtestInstance =
        Class "WIGtestInstance"
        |+> Instance [
            "x" =@ T<int>
            "adderFunc" =@ T<int> * T<int> ^-> T<int>
            "adderFuncWithThis" =@ TSelf -* T<int> * T<int> ^-> T<int>
            "stringOrInt" =@ T<string> + T<int>
            "optionalInt" =@ !? T<int>
            "optionalStringOrFunction" =@ !? (T<string> + ( T<int> * T<int> ^-> T<int>))
        ]

    let WIGtest =
        Class "WIGtest"
        |+> Static [
            "ArgsFuncIn" => (T<int> * T<int> ^-> T<int>)?add ^-> T<int>    
            "ArgsFuncOut" => T<unit> ^-> (T<int> * T<int> ^-> T<int>)
            Generic - fun a -> "GetGetThis" => T<unit> ^-> (a -* T<unit> ^-> a)            
            "FuncInWithThis" => (JustX -* T<unit> ^-> T<string>) ^-> T<string>
            "ArgFuncInWithThis" => (JustX -* T<int> ^-> T<string>) ^-> T<string>
            "ArgsFuncInWithThis" => (JustX -* T<int> * T<int> ^-> T<string>) ^-> T<string>
            "TupledFuncInWithThis" => (JustX -* (T<int> * T<int>).Parameter ^-> T<string>) ^-> T<string>
            "Instance" =@ WIGtestInstance
        ]

    let WIGtestGeneric =
        Generic - fun a b ->
            Class "WIGtestGeneric"
            |+> Instance [
                "NonGenericMethod" => a * b ^-> T<unit>
                Generic - fun c d -> "GenericMethod" => a * b * c * d ^-> T<unit>
                Constructor T<unit>
            ]

    let ConfigObj =
        Pattern.Config "ConfigObj" {
            Required = 
                [
                    "firstReq", T<int>
                    "secondReq", T<int * int -> string>
                ]
            Optional = 
                [
                    "firstOpt", T<int>
                    "secondOpt", T<int * int -> string>
                ]
        }

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.InterfaceGenerator.Tests" [
                 JustX
                 WIGtestInstance
                 WIGtest
                 WIGtestGeneric
                 ConfigObj
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
