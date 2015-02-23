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

module IntelliFactory.WebSharper.Tests.WIG

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.Testing
open IntelliFactory.WebSharper.InterfaceGenerator.Tests

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

        WIGtest.ArgsFuncIn(fun (a, b) -> a + b) =? 3    
        WIGtest.ArgsFuncIn2(fun (a, b) -> a + b) =? 3    
        WIGtest.ArgsFuncOut()(1, 2) =? 3
        let x = JustX(5)       
        WIGtest.GetGetThis()(x) =? x
        WIGtest.FuncInWithThis(fun (t: JustX) -> string t.X) =? "0"
        WIGtest.ArgFuncInWithThis(fun (t: JustX) a -> string t.X + string a) =? "01"
        WIGtest.ArgsFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b) =? "012"
        WIGtest.TupledFuncInWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b) =? "012"

        WIGtest.CallWith1 id =? 1
        WIGtest.CallWith1 ((+) 2) =? 3
        WIGtest.CallWith2 (fun (a, b) -> a + b) =? 3
        WIGtest.CallWith10 (fun (a, b, c, d, e, f, g, h, i, j) -> a + b + c + d + e + f + g + h + i + j) =? 55
    }

    Test "Named tuple" {
        WIGtest.ArgsFuncIn(fun ((a, b) as ab) -> a + b + fst ab + snd ab) =? 6    
    }

    Test "Functions with ParamArray" {
        let doNotRun() = 
            (WIGtest.TestCurriedSig 0 "" : obj) |> ignore
            (WIGtest.TestIntOrStringReturned() : Choice<int, string>) |> ignore
            (WIGtest.TestWithNoInterop : FuncWithArgs<int * int, int> -> obj) |> ignore

        WIGtest.Sum(1) =? 1
        WIGtest.Sum(1, 2) =? 3
        WIGtest.Sum(1, 2, 3) =? 6
        WIGtest.SumBy((+) 1, 1) =? 2
        WIGtest.SumBy((+) 1, 1, 2) =? 5
        WIGtest.SumBy((+) 1, 1, 2, 3) =? 9
        WIGtest.SumByThenMap((+) 1, (+) 2, 1) =? 4
        WIGtest.SumByThenMap((+) 1, (+) 2, 1, 2) =? 7
        WIGtest.SumByThenMap((+) 1, (+) 2, 1, 2, 3) =? 11

        WIGtest.GetSum() [| 1 |] =? 1
        WIGtest.GetSum() [| 1; 2 |] =? 3
        WIGtest.GetSum() [| 1; 2; 3 |] =? 6
        WIGtest.GetSumBy() ((+) 1, [| 1 |]) =? 2
        WIGtest.GetSumBy() ((+) 1, [| 1; 2 |]) =? 5
        WIGtest.GetSumBy() ((+) 1, [| 1; 2; 3 |]) =? 9
        WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1 |]) =? 4
        WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1; 2 |]) =? 7
        WIGtest.GetSumByThenMap() ((+) 1, (+) 2, [| 1; 2; 3 |]) =? 11
        
        WIGtest.GetSum7AndRest() (1, 2, 3, 4, 5, 6, 7, [| 8; 9; 10 |]) =? 55

        WIGtest.CallWithRest(fun r -> Array.sum r) =? 55
        WIGtest.CallWith1AndRest(fun (a, r) -> a + Array.sum r) =? 55
        WIGtest.CallWith2AndRest(fun (a, b, r) -> a + b + Array.sum r) =? 55
        WIGtest.CallWith7AndRest(fun (a, b, c, d, e, f, g, r) -> a + b + c + d + e + f + g + Array.sum r) =? 55
    }

    Test "Function property" {
        let x = WIGtest.Instance 
        x.AdderFunc(1, 2) =? 3
        x.AdderFunc <- fun (a, b) -> a + b + 1
        x.AdderFunc(1, 2) =? 4 
        x.AdderFuncWithThis(x)(1, 2) =? 3
        x.X <- 1
        x.AdderFuncWithThis(x)(1, 2) =? 4
        x.AdderFuncWithThis <- fun t (a, b) -> t.X + a + b + 1
        x.AdderFuncWithThis(x)(1, 2) =? 5
    }

    Test "Choice property" {
        let x = WIGtest.Instance 
        x.StringOrInt =? Choice1Of2 0
        x.StringOrInt <- Choice2Of2 "hi"
        x.StringOrInt =? Choice2Of2 "hi"
        x.StringOrInt <- Choice1Of2 1
        x.StringOrInt =? Choice1Of2 1
    }

    Test "Option property" {
        let x = WIGtest.Instance 
        x.OptionalInt =? None
        x.OptionalInt <- Some 1
        x.OptionalInt =? Some 1
        x.OptionalInt <- None
        x.OptionalInt =? None       
    }

    Test "Optional choice property" {
        let x = WIGtest.Instance 
        x.OptionalStringOrFunction =? None
        x.OptionalStringOrFunction <- Some (Choice2Of2 "hi")
        x.OptionalStringOrFunction =? Some (Choice2Of2 "hi")
        x.OptionalStringOrFunction <- Some (Choice1Of2 (FuncWithArgs(fun (a, b) -> a + b)))
        (
            match x.OptionalStringOrFunction with
            | Some (Choice1Of2 f) -> f.Call(1, 2)
            | _ -> 0
        ) =? 3
        x.OptionalStringOrFunction <- None
        x.OptionalStringOrFunction =? None
    }

    Test "Pattern.Config" {
        let c = 
            ConfigObj(1, (fun (a, b) -> string a + string b), 
                FirstOpt = 2, SecondOpt = (fun (a, b) -> string a + "," + string b))
        c.FirstReq =? 1
        c.SecondReq(1, 2) =? "12"
        c.FirstOpt =?2
        c.SecondOpt(1, 2) =? "1,2"
    }

    Test "Passing variadic" {
        WIGtest.ArgsFuncInStrings(f)
        WIGtest.ArgsFuncInStrings(fun (x, y) -> f (x, y))
        WIGtest.ArgsFuncInStrings(fun (x, y) -> Console.Log(x, y))
        WIGtest.ArgsFuncInStrings(Console.Log)
        () =? ()
    }

    Test "Indexed property" {
        let a = OneBasedArr(1)
        a.[1] <- "hi"
        a =? New [| "0" => "hi" |] 
        a.[1] =? "hi"

        let b = ObjWithOptionalFields()
        b.["x"] <- Some "hi"
        b =? New [| "x" => "hi" |] 
        b.["x"] =? Some "hi"
        b.["x"] <- None
        b =? New [| |]

        b.AsLowerCase("X") <- Some "hi"
        b =? New [| "x" => "hi" |] 
        b.AsLowerCase("X") =? Some "hi"
        b.AsLowerCase("X") <- None
        b =? New [| |]
    }