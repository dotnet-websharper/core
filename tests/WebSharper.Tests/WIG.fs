// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
let Tests =
    Section "Interface generator"

    Test "Functions" {
        let doNotRun() = 
            let g = WIGtestGeneric<int, string>()
            g.NonGenericMethod(1, "")
            g.GenericMethod<bool, obj>(1, "", true, obj())

        Equal (WIGtest.ArgsFuncIn(fun (a, b) -> a + b)) 3
        Equal (WIGtest.ArgsFuncIn2(fun (a, b) -> a + b)) 3
        Equal (WIGtest.ArgsFuncOut()(1, 2)) 3
        let x = JustX(5)       
        Equal (WIGtest.GetGetThis()(x)) x
        Equal (WIGtest.FuncInWithThis(fun (t: JustX) -> string t.X)) "0"
        Equal (WIGtest.ArgFuncInWithThis(fun (t: JustX) a -> string t.X + string a)) "01"
        Equal (WIGtest.ArgsFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) "012"
        Equal (WIGtest.TupledFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) "012"

        Equal (WIGtest.CallWith1 id) 1
        Equal (WIGtest.CallWith1 ((+) 2)) 3
        Equal (WIGtest.CallWith2 (fun (a, b) -> a + b)) 3
        Equal (WIGtest.CallWith10 (fun (a, b, c, d, e, f, g, h, i, j) -> a + b + c + d + e + f + g + h + i + j)) 55

        Equal (FuncWithArgs(fun (a, b) -> a + b).Length) 2
    }

    Test "Named tuple" {
        Equal (WIGtest.ArgsFuncIn(fun ((a, b) as ab) -> a + b + fst ab + snd ab)) 6
    }

    Test "Functions with ParamArray" {
        let doNotRun() = 
            (WIGtest.TestCurriedSig 0 "" : obj) |> ignore
            (WIGtest.TestIntOrStringReturned() : Choice<int, string>) |> ignore
            (WIGtest.TestWithNoInterop : FuncWithArgs<int * int, int> -> obj) |> ignore

        Equal (WIGtest.Sum(1)) 1
        Equal (WIGtest.Sum(1, 2)) 3
        Equal (WIGtest.Sum(1, 2, 3)) 6
        Equal (WIGtest.SumBy((+) 1, 1)) 2
        Equal (WIGtest.SumBy((+) 1, 1, 2)) 5
        Equal (WIGtest.SumBy((+) 1, 1, 2, 3)) 9
        Equal (WIGtest.SumByThenMap((+) 1, (+) 2, 1)) 4
        Equal (WIGtest.SumByThenMap((+) 1, (+) 2, 1, 2)) 7
        Equal (WIGtest.SumByThenMap((+) 1, (+) 2, 1, 2, 3)) 11

        Equal (WIGtest.GetSum() [| 1 |]) 1
        Equal (WIGtest.GetSum() [| 1; 2 |]) 3
        Equal (WIGtest.GetSum() [| 1; 2; 3 |]) 6
        Equal (WIGtest.GetSumBy() ((+) 1, [| 1 |])) 2
        Equal (WIGtest.GetSumBy() ((+) 1, [| 1; 2 |])) 5
        Equal (WIGtest.GetSumBy() ((+) 1, [| 1; 2; 3 |])) 9
        Equal (WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1 |])) 4
        Equal (WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1; 2 |])) 7
        Equal (WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1; 2; 3 |])) 11
        
        Equal (WIGtest.GetSum7AndRest() (1, 2, 3, 4, 5, 6, 7, [| 8; 9; 10 |])) 55

        Equal (WIGtest.CallWithRest(fun r -> Array.sum r)) 55
        Equal (WIGtest.CallWith1AndRest(fun (a, r) -> a + Array.sum r)) 55
        Equal (WIGtest.CallWith2AndRest(fun (a, b, r) -> a + b + Array.sum r)) 55
        Equal (WIGtest.CallWith7AndRest(fun (a, b, c, d, e, f, g, r) -> a + b + c + d + e + f + g + Array.sum r)) 55
    }

    Test "Function property" {
        let x = WIGtest.Instance 
        Equal (x.AdderFunc(1, 2)) 3
        x.AdderFunc <- fun (a, b) -> a + b + 1
        Equal (x.AdderFunc(1, 2)) 4 
        Equal (x.AdderFuncWithThis(x)(1, 2)) 3
        x.X <- 1
        Equal (x.AdderFuncWithThis(x)(1, 2)) 4
        x.AdderFuncWithThis <- fun t (a, b) -> t.X + a + b + 1
        Equal (x.AdderFuncWithThis(x)(1, 2)) 5
    }

    Test "Choice property" {
        let x = WIGtest.Instance 
        Equal (x.StringOrInt) (Choice1Of2 0)
        x.StringOrInt <- Choice2Of2 "hi"
        Equal (x.StringOrInt) (Choice2Of2 "hi")
        x.StringOrInt <- Choice1Of2 1
        Equal (x.StringOrInt) (Choice1Of2 1)
    }

    Test "Option property" {
        let x = WIGtest.Instance 
        Equal (x.OptionalInt) None
        x.OptionalInt <- Some 1
        Equal (x.OptionalInt) (Some 1)
        x.OptionalInt <- None
        Equal (x.OptionalInt) None       
    }

    Test "Optional choice property" {
        let x = WIGtest.Instance 
        Equal (x.OptionalStringOrFunction) None
        x.OptionalStringOrFunction <- Some (Choice2Of2 "hi")
        Equal (x.OptionalStringOrFunction) (Some (Choice2Of2 "hi"))
        x.OptionalStringOrFunction <- Some (Choice1Of2 (FuncWithArgs(fun (a, b) -> a + b)))
        Equal (
            match x.OptionalStringOrFunction with
            | Some (Choice1Of2 f) -> f.Call(1, 2)
            | _ -> 0
        ) 3
        x.OptionalStringOrFunction <- None
        Equal (x.OptionalStringOrFunction) None
    }

    Test "Pattern.Config" {
        let c = 
            ConfigObj(1, (fun (a, b) -> string a + string b), 
                FirstOpt = 2, SecondOpt = (fun (a, b) -> string a + "," + string b))
        Equal (c.FirstReq) 1
        Equal (c.SecondReq(1, 2)) "12"
        Equal (c.FirstOpt) 2
        Equal (c.SecondOpt(1, 2)) "1,2"
    }

    Test "Passing variadic" {
        WIGtest.ArgsFuncInStrings(f)
        WIGtest.ArgsFuncInStrings(fun (x, y) -> f (x, y))
        WIGtest.ArgsFuncInStrings(fun (x, y) -> Console.Log(x, y))
        WIGtest.ArgsFuncInStrings(Console.Log)
        Equal () ()
    }

    Test "Indexed property" {
        let a = OneBasedArr(1)
        a.[1] <- "hi"
        Equal (a) (New [| "0" => "hi" |])
        Equal (a.[1]) "hi"

        let b = ObjWithOptionalFields()
        b.["x"] <- Some "hi"
        Equal (b) (New [| "x" => "hi" |])
        Equal (b.["x"]) (Some "hi")
        b.["x"] <- None
        Equal (b) (New [| |])

        b.AsLowerCase("X") <- Some "hi"
        Equal (b) (New [| "x" => "hi" |] )
        Equal (b.AsLowerCase("X")) (Some "hi")
        b.AsLowerCase("X") <- None
        Equal (b) (New [| |])
    }