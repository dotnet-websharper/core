namespace IntelliFactory.WebSharper.InterfaceGenerator.Tests

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let Int = T<int>
    let String = T<string>

    let O = Type.Unit

    let JustX =
        Pattern.Config "JustX" {
            Required = [ "x", Int ]
            Optional = []
        }

    let WIGtestInstance =
        Class "WIGtestInstance"
        |+> Instance [
            "x" =@ Int
            "adderFunc" =@ Int * Int ^-> Int
            "adderFuncWithThis" =@ TSelf -* Int * Int ^-> Int
            "stringOrInt" =@ String + Int
            "optionalInt" =@ !? Int
            "optionalStringOrFunction" =@ !? (String + ( Int * Int ^-> Int))
        ]

    let WIGtest =
        Class "WIGtest"
        |+> Static [
            "ArgsFuncIn" => (Int * Int ^-> Int)?add ^-> Int    
            "ArgsFuncIn2" => 
                (Int * Int ^-> Int)?add ^-> Int   
                |> WithInteropInline (fun tr -> "WIGtest.ArgsFuncIn(" + tr "add" + ")") 
            "ArgsFuncOut" => O ^-> (Int * Int ^-> Int)
            Generic - fun a -> "GetGetThis" => O ^-> (a -* O ^-> a)            
            "FuncInWithThis" => (JustX -* O ^-> String) ^-> String
            "ArgFuncInWithThis" => (JustX -* Int ^-> String) ^-> String
            "ArgsFuncInWithThis" => (JustX -* Int * Int ^-> String) ^-> String
            "TupledFuncInWithThis" => (JustX -* (Int * Int).Parameter ^-> String) ^-> String
            "Sum" => !+ Int ^-> Int
            "SumBy" => T<int -> int>?mapping *+ Int ^-> Int
            "SumByThenMap" => T<int -> int>?mapping * T<int -> int>?resultMapping *+ Int ^-> Int
            "GetSum" => O ^-> (!+ Int ^-> Int)
            "GetSumBy" => O ^-> (T<int -> int>?mapping *+ Int ^-> Int)
            "GetSumByThenMap" => O ^-> (T<int -> int>?mapping * T<int -> int>?resultMapping *+ Int ^-> Int)
            "GetSum" => O ^-> (Int * Int * Int * Int * Int * Int * Int *+ Int ^-> Int) |> WithSourceName "GetSum7AndRest"
            
            "CallWith1" => (Int ^-> Int) ^-> Int
            "CallWith2" => (Int * Int ^-> Int) ^-> Int
            "CallWith10" => (Int * Int * Int * Int * Int * Int * Int * Int * Int * Int ^-> Int) ^-> Int
            "CallWith10" => (!+ Int ^-> Int) ^-> Int |> WithSourceName "CallWithRest"
            "CallWith10" => (Int *+ Int ^-> Int) ^-> Int |> WithSourceName "CallWith1AndRest"
            "CallWith10" => (Int * Int *+ Int ^-> Int) ^-> Int |> WithSourceName "CallWith2AndRest"
            "CallWith10" => (Int * Int * Int * Int * Int * Int * Int *+ Int ^-> Int) ^-> Int |> WithSourceName "CallWith7AndRest"

            "Instance" =@ WIGtestInstance

            "TestCurriedSig" => Int ^-> String ^-> T<obj>
            "TestIntOrStringReturned" => O ^-> (Int + String)
            "TestWithNoInterop" => ((Int * Int ^-> Int) ^-> (Int + String) |> WithNoInterop)
        ]

    let WIGtestGeneric =
        Generic - fun a b ->
            Class "WIGtestGeneric"
            |+> Instance [
                "NonGenericMethod" => a * b ^-> O
                Generic - fun c d -> "GenericMethod" => a * b * c * d ^-> O
                Constructor O
            ]

    let ConfigObj =
        Pattern.Config "ConfigObj" {
            Required = 
                [
                    "firstReq", Int
                    "secondReq", T<int * int -> string>
                ]
            Optional = 
                [
                    "firstOpt", Int
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
