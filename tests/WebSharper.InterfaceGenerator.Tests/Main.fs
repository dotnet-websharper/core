namespace WebSharper.InterfaceGenerator.Tests

open WebSharper

module Definition =
    open WebSharper.InterfaceGenerator

    let Int = T<int>
    let String = T<string>

    let O = Type.Unit

    let JustX =
        Pattern.Config "JustX" {
            Required = [ "x", Int ]
            Optional = []
        }

    let IWIGTest =
        Interface "IWIGTest"
        |+> [
            "add" => Int * Int ^-> Int
        ]

    let WIGtestInstance =
        Class "WIGtestInstance"
        |=> Implements [ IWIGTest ]
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
            "ArgsFuncInStrings" => (String * String ^-> O)?f ^-> O
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

    let WIGtest2 =
        Class "WIGtest2"
        |+> Static [
            "SumTest" =? Int
        ]

    let WIGtestGeneric =
        Generic + [ "T"; "U" ] - fun a b ->
            Class "WIGtestGeneric"
            |+> Instance [
                "NonGenericMethod" => a * b ^-> O
                Generic + [ "T"; "U" ] - fun c d -> "GenericMethod" => a * b * c * d ^-> O
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

    let Index =
        T<int> |> WithInterop {
            In = fun s -> s + " - 1" 
            Out = fun s -> s + " + 1"
        }
    
    let OneBasedArr =
        Class "OneBasedArr"
        |+> Static [
            Constructor T<int> |> WithInline "Array($0)"
        ]
        |+> Instance [
            "" =@ T<string> |> Indexed Index
        ] 

    let Lowercase =
        T<string> |> WithInterop {
            In = fun s -> s + "[0].toLowerCase() + " + s + ".slice(1)"
            Out = fun s -> s + "[0].toUpperCase() + " + s + ".slice(1)"
        }

    let ObjWithOptionalFields =
        Class "ObjWithOptionalFields"
        |+> Static [
            Constructor T<unit> |> WithInline "{}"
        ]
        |+> Instance [
            "" =@ !? T<string> |> Indexed T<string>
            "asLowerCase" =@ !? T<string> |> Indexed Lowercase
                |> WithInteropGetterInline (fun tr -> sprintf "$this[%s]" (tr "index"))
                |> WithInteropSetterInline (fun tr -> sprintf "$wsruntime.SetOptional($this, %s, %s)" (tr "index") (tr "value"))
        ] 

    let Assembly =
        let res1 = Resource "WIGTestJs" "WIGtest.js"
        Assembly [
            Namespace "WebSharper.InterfaceGenerator.Tests" [
                 JustX
                 IWIGTest
                 WIGtestInstance
                 WIGtest
                 WIGtest2
                 WIGtestGeneric
                 ConfigObj
                 OneBasedArr
                 ObjWithOptionalFields
                 res1 |> AssemblyWide
                 Resource "WIGTestJs2" "WIGtest2.js" |> Requires [ res1 ] |> AssemblyWide
            ]
        ]

open WebSharper.InterfaceGenerator

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
