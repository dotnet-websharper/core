// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Tests.WIG

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
open WebSharper.InterfaceGenerator.Tests

[<JavaScript>]
let f (x, y) = Console.Log((x:obj), (y:obj))

[<JavaScript>]
type InheritWIG() =
    interface IWIGTest with
        member this.Add(x, y) = x + y    

[<Stub>]
type StubTestBaseClass internal () =
    member this.GetY() = X<int>

[<Name "StubTest.Class"; Stub>]
type StubTestClass =
    inherit StubTestBaseClass
    new () = {}
    member this.GetX() = X<int>
    static member Static() = X<int>

[<JavaScript>]
let Tests =
    TestCategory "Interface generator" {

        Test "Functions" {
            let doNotRun() = 
                let g = WIGtestGeneric<int, string>()
                g.NonGenericMethod(1, "")
                g.GenericMethod<bool, obj>(1, "", true, obj())

            equal (WIGtest.ArgsFuncIn(fun (a, b) -> a + b)) 3
            equal (WIGtest.ArgsFuncIn2(fun (a, b) -> a + b)) 3
            equal (WIGtest.ArgsFuncOut().Invoke(1, 2)) 3
            let x = JustX(5)       
            equal (WIGtest.GetGetThis().Call(x)) x
            equal (WIGtest.FuncInWithThis(fun (t: JustX) -> string t.X)) "0"
            equal (WIGtest.ArgFuncInWithThis(fun (t: JustX) a -> string t.X + string a)) "01"
            equal (WIGtest.ArgsFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) "012"
            equal (WIGtest.TupledFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) "012"

            equal (WIGtest.CallWith1 (fun x -> x)) 1
            equal (WIGtest.CallWith1 (fun x -> x + 2)) 3
            equal (WIGtest.CallWith2 (fun (a, b) -> a + b)) 3
            equal (WIGtest.CallWith10 (fun (a, b, c, d, e, f, g, h, i, j) -> a + b + c + d + e + f + g + h + i + j)) 55

            equal (FuncWithArgs(fun (a, b) -> a + b).Length) 2
        }

        Test "Named tuple" {
            equal (WIGtest.ArgsFuncIn(fun ((a, b) as ab) -> a + b + fst ab + snd ab)) 6
        }

        Test "Functions with ParamArray" {
            let doNotRun() = 
                (WIGtest.TestCurriedSig(0).Invoke("") : obj) |> ignore
                (WIGtest.TestIntOrStringReturned() : Union<int, string>) |> ignore
                (WIGtest.TestWithNoInterop : FuncWithArgs<int * int, int> -> obj) |> ignore

            equal (WIGtest.Sum(1)) 1
            equal (WIGtest.Sum(1, 2)) 3
            equal (WIGtest.Sum(1, 2, 3)) 6
            equal (WIGtest.SumBy((+) 1, 1)) 2
            equal (WIGtest.SumBy((fun x -> x + 1), 1, 2)) 5
            equal (WIGtest.SumBy((fun x -> x + 1), 1, 2, 3)) 9
            equal (WIGtest.SumByThenMap((fun x -> x + 1), (+) 2, 1)) 4
            equal (WIGtest.SumByThenMap((fun x -> x + 1), (fun x -> x + 2), 1, 2)) 7
            equal (WIGtest.SumByThenMap((fun x -> x + 1), (fun x -> x + 2), 1, 2, 3)) 11

            let add1Func = System.Func<_,_>((+) 1)
            let add2Func = System.Func<_,_>((+) 2)
            equal (WIGtest.GetSum().Call [| 1 |]) 1
            equal (WIGtest.GetSum().Call [| 1; 2 |]) 3
            equal (WIGtest.GetSum().Call [| 1; 2; 3 |]) 6
            equal (WIGtest.GetSumBy().Call (add1Func, [| 1 |])) 2
            equal (WIGtest.GetSumBy().Call (add1Func, [| 1; 2 |])) 5
            equal (WIGtest.GetSumBy().Call (add1Func, [| 1; 2; 3 |])) 9
            equal (WIGtest.GetSumByThenMap().Call (add1Func, add2Func, [| 1 |])) 4
            equal (WIGtest.GetSumByThenMap().Call (add1Func, add2Func, [| 1; 2 |])) 7
            equal (WIGtest.GetSumByThenMap().Call (add1Func, add2Func, [| 1; 2; 3 |])) 11

            equal (WIGtest.GetSum7AndRest().CallUnsafe (null, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) (box 55)

            equal (WIGtest.CallWithRest(fun r -> Array.sum r)) 55
            equal (WIGtest.CallWith1AndRest(fun (a, r) -> a + Array.sum r)) 55
            equal (WIGtest.CallWith2AndRest(fun (a, b, r) -> a + b + Array.sum r)) 55
            equal (WIGtest.CallWith7AndRest(fun (a, b, c, d, e, f, g, r) -> a + b + c + d + e + f + g + Array.sum r)) 55
        }

        Test "Function property" {
            let x = WIGtest.Instance 
            equal (x.AdderFunc.Invoke(1, 2)) 3
            x.AdderFunc <- fun a b -> a + b + 1
            equal (x.AdderFunc.Invoke(1, 2)) 4 
            equal (x.AdderFuncWithThis.Call(x, 1, 2)) 3
            x.X <- 1
            equal (x.AdderFuncWithThis.Call(x, 1, 2)) 4
            x.AdderFuncWithThis <- ThisFunc<WIGtestInstance,_,_,_>(fun t a b -> t.X + a + b + 1)
            equal (x.AdderFuncWithThis.Call(x, 1, 2)) 5
        }

        Test "Choice property" {
            let x = WIGtest.Instance 
            equal (x.StringOrInt) (Union1Of2 0)
            equal (Union.toChoice2 x.StringOrInt) (Choice1Of2 0)
            equal (x.StringOrInt.Value1) 0
//            raises (x.StringOrInt.Value2)
            x.StringOrInt <- Union2Of2 "hi"
            equal (x.StringOrInt) (Union2Of2 "hi")
            equal (Union.toChoice2 x.StringOrInt) (Choice2Of2 "hi")
            equal (x.StringOrInt.Value2) "hi"
//            raises (x.StringOrInt.Value1)
            x.StringOrInt <- Union1Of2 1
            equal (x.StringOrInt) (Union1Of2 1)
        }

        Test "Option property" {
            let x = WIGtest.Instance 
            equal (x.OptionalInt) Undefined
            equal (Optional.toOption x.OptionalInt) None
//            raises (x.OptionalInt.Value)
            x.OptionalInt <- Defined 1
            equal (x.OptionalInt) (Defined 1)
            equal (Optional.toOption x.OptionalInt) (Some 1)
            equal (x.OptionalInt.Value) 1
            x.OptionalInt <- Undefined
            equal (x.OptionalInt) Undefined       
        }

        Test "Optional choice property" {
            let x = WIGtest.Instance 
            equal (x.OptionalStringOrFunction) Undefined
            x.OptionalStringOrFunction <- Defined (Union2Of2 "hi")
            equal (x.OptionalStringOrFunction) (Defined (Union2Of2 "hi"))
            x.OptionalStringOrFunction <- Defined (Union1Of2 (System.Func<_,_,_>(fun a b -> a + b)))
            equal (
                match x.OptionalStringOrFunction with
                | Defined (Union1Of2 f) -> f.Invoke(1, 2)
                | _ -> 0
            ) 3
            x.OptionalStringOrFunction <- Undefined
            jsEqual (x.OptionalStringOrFunction) JS.Undefined
        }

        Test "Pattern.Config" {
            let c = 
                ConfigObj(1, (fun (a, b) -> string a + string b), 
                    FirstOpt = 2, SecondOpt = (fun (a, b) -> string a + "," + string b))
            equal (c.FirstReq) 1
            equal (c.SecondReq.Invoke(1, 2)) "12"
            equal (c.FirstOpt) 2
            equal (c.SecondOpt.Invoke(1, 2)) "1,2"
        }

        Test "Passing variadic" {
            WIGtest.ArgsFuncInStrings(f)
            WIGtest.ArgsFuncInStrings(fun (x, y) -> f (x, y))
            WIGtest.ArgsFuncInStrings(fun (x, y) -> Console.Log(x, y))
            WIGtest.ArgsFuncInStrings(Console.Log)
            equal () ()
        }

        Test "Indexed property" {
            let a = OneBasedArr(1)
            a.[1] <- "hi"
            equal (a) (New [| "0" => "hi" |])
            equal (a.[1]) "hi"

            let b = ObjWithOptionalFields()
            b.["x"] <- Defined "hi"
            equal (b) (New [| "x" => "hi" |])
            equal (b.["x"]) (Defined "hi")
            b.["x"] <- Undefined
            equal (b) (New [| |])

            b.AsLowerCase("X") <- Defined "hi"
            equal (b) (New [| "x" => "hi" |] )
            equal (b.AsLowerCase("X")) (Defined "hi")
            b.AsLowerCase("X") <- Undefined
            equal (b) (New [| |])
        }

        Test "Resource ordering" {
            equal WIGtest2.SumTest 6    
        }

        Test "Type test" {
            let el = WebSharper.JavaScript.JS.Document.CreateElement("div") |> box
            isTrue (el :? Dom.Element)
        }

        Test "Interface" {
            let x = WIGtest.Instance 
            equal (x.Add(1, 2)) 3
            equal ((x :> IWIGTest).Add(1, 2)) 3 
        }

        Test "Mixin interface" {
            let x = WIGtest.Instance 
            equal (x.CallMixin()) "ok"
            equal ((x :> IMixinTest).CallMixin()) "ok"
        }

        Test "Inheriting from WIG interface" {
            let x = InheritWIG()
            equal ((x :> IWIGTest).Add(1, 2)) 3
        }

        Test "Stub class" { 
            let s = StubTestClass()
            equal (s.GetX()) 3
            equal (s.GetY()) 3
            equal (StubTestClass.Static()) 4
        }
    }
