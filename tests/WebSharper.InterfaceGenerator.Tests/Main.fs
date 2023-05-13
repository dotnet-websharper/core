// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
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

    let IMixinTest =
        Interface "IMixinTest"
        |+> [
            "callMixin" => O ^-> String |> WithInline "$this.callMixin('called through mixin')"
            "x" =@ Int
        ]

    let WIGtestInstance =
        Class "WIGtestInstance"
        |=> Implements [ IWIGTest; IMixinTest ]
        |+> Instance [
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
            Generic - fun a -> "GetGetThis" => O ^-> (a -* O ^-> a) |> WithWarning "Testing WIG WithWarning, you should see this"           
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

    let res3 = Resource "WIGTestJs3" "WIGtest3.js"

    let WIGtest3 =
        Class "WIGtest3"
        |+> Static [
            "Ok" =? String |> WithGetterInline "WIGtest3" |> Requires [ res3 ]
        ]

    let WIGtest4 =
        Class "WIGtest4"
        |> ImportDefault "./WIGtest4.js"
        |+> Instance [
            "sayHiInst" => String ^-> String
        ]
        |+> Static [
            Constructor O
            "sayHiStatic" => String ^-> String
            "sayHiFunc" => String ^-> String |> Import "sayHi" "./WIGtest4.js"
        ]

    let NPMTest =
        Class "NPMTest"
        |> ImportDefault "NPMTest"
        |+> Static [
            "sayHiStatic" => String ^-> String
        ]

    let WIGtestGeneric =
        Generic + [ "T"; "U" ] -- fun a b ->
            Class "WIGtestGeneric"
            |+> Instance [
                "NonGenericMethod" => a * b ^-> O
                Generic + [ "T"; "U" ] -- fun c d -> "GenericMethod" => a * b * c * d ^-> O
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
                |> WithInteropSetterInline (fun tr -> sprintf "$wsruntime.SetOrDelete($this, %s, %s)" (tr "index") (tr "value"))
        ]

    let AbsCls =
        AbstractClass "AbsCls"
        |+> Static [Constructor T<string>]
        |+> Instance [
            "absMeth" => T<unit> ^-> T<string>
            |> Abstract
            "virtMeth" => T<unit> ^-> T<string>
            |> Virtual
            "concMeth" => T<unit> ^-> T<string>
        ]

    let OverridingCls =
        Class "OverridingCls"
        |=> Inherits AbsCls
        |+> Static [Constructor T<unit>]
        |+> Instance [
            "absMeth" => T<unit> ^-> T<string>
            |> Override
            "virtMeth" => T<unit> ^-> T<string>
            |> Override
        ]

    let NonOverridingCls =
        Class "NonOverridingCls"
        |=> Inherits AbsCls
        |+> Static [Constructor T<unit>]
        |+> Instance [
            "absMeth" => T<unit> ^-> T<string>
            |> Override
        ]

    let ConcCls =
        Class "ConcCls"
        |+> Static [Constructor T<string>]
        |+> Instance [
            "virtMeth" => T<unit> ^-> T<string>
            |> Virtual
            "concMeth" => T<unit> ^-> T<string>
        ]

    module Regression1010 =
        let A =
            Class "Regression1010.A"

        let B =
            Class "Regression1010.B"
            |=> Inherits A
            |+> Static [Constructor T<unit>]
            |+> Instance ["m" => T<unit> ^-> T<int>]

    let Assembly =
        let res1 = Resource "WIGTestJs" "WIGtest.js"
        Assembly [
            Namespace "WebSharper.InterfaceGenerator.Tests" [
                 JustX
                 IWIGTest
                 IMixinTest
                 WIGtestInstance
                 WIGtest
                 WIGtest2
                 WIGtest3
                 WIGtest4
                 NPMTest
                 WIGtestGeneric
                 ConfigObj
                 OneBasedArr
                 ObjWithOptionalFields
                 AbsCls
                 OverridingCls
                 NonOverridingCls
                 ConcCls
                 res1 |> AssemblyWide
                 Resource "WIGTestJs2" "WIGtest2.js" |> Requires [ res1 ] |> AssemblyWide
                 res3
            ]
            Namespace "WebSharper.InterfaceGenerator.Tests.Regression1010" [
                Regression1010.A
                Regression1010.B
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
