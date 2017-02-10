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

module WebSharper.Tests.Optimizations

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let GlobalTupled (a, b) =
    a + 2 * b 

[<JavaScript>]
let GlobalCurried a b =
    a + 2 * b

[<JavaScript>]
let TupledArg f : int =
    f (1, 2)

[<JavaScript>]
let CurriedArg f : int =
    f 1 2

[<JavaScript>]
let rec RecCurriedArg f : int =
    let r = f 1 2 
    if r = 5 then
        RecCurriedArg (fun x y -> f x (y + 1))
    else
        r 

[<JavaScript>]
type TypeWithTupled(f) =
    let g = JavaScript.FuncWithArgs(f)
    member this.Apply() = g.Call(1, 2) : int

    static member TupledArg f = 
        f (1, 2)

[<JavaScript>]
type TypeWithCurried(f) =
    let g = JavaScript.FuncWithArgs(fun (a, b) -> f a b)
    member this.Apply() = g.Call(1, 2) : int

    member this.Other a b h = h a b

    [<Inline>]
    member this.OtherU(a, b, h) = this.Other a b h

    static member CurriedArg f = 
        f 1 2

[<JavaScript>]
let Tests =

    let LocalTupled (a, b) =
        a + 2 * b 

    let LocalCurried a b =
        a + 2 * b

    TestCategory "Optimizations" {
        Test "Tupled function" {
            equal (GlobalTupled (1, 2)) 5
            equal (LocalTupled (1, 2)) 5
        }

        Test "Tupled function argument" {
            equal (TupledArg (fun (a, b) -> a + 2 * b)) 5
            equal (TupledArg GlobalTupled) 5
            equal (TupledArg LocalTupled) 5
            equal (TypeWithTupled(fun (a, b) -> a + 2 * b).Apply()) 5
            equal (TypeWithTupled.TupledArg GlobalTupled) 5
        }

        Test "Curried function" {
            equal (GlobalCurried 1 2) 5
            equal (LocalCurried 1 2) 5
        }

        Test "Curried function argument" {
            equal (CurriedArg (fun a b -> a + 2 * b)) 5
            equal (CurriedArg GlobalCurried) 5
            equal (CurriedArg LocalCurried) 5
            equal (TypeWithCurried(fun a b -> a + 2 * b).Apply()) 5
            equal (TypeWithCurried.CurriedArg GlobalCurried) 5
            equal (RecCurriedArg GlobalCurried) 7
        }

        Test "Curried method" {
            equal (TypeWithCurried(fun a b -> 0).Other 1 2 (fun a b -> a + 2 * b)) 5
            equal (TypeWithCurried(fun a b -> 0).OtherU(1, 2, (fun a b -> a + 2 * b))) 5
        }
    }

[<JavaScript>]
let TupledArgWithGlobal() =
    TupledArg GlobalTupled

[<JavaScript>]
let TupledArgWithLocal() =
    let LocalTupled (a, b) =
        a + b 
    TupledArg LocalTupled

[<JavaScript>]
let CurriedArgWithGlobal() =
    CurriedArg GlobalCurried

[<JavaScript>]
let CurriedArgWithLocal() =
    let LocalCurried a b =
        a + b
    CurriedArg LocalCurried
