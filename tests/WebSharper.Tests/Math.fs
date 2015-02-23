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

module WebSharper.Tests.Math

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random
type private Math = System.Math

[<JavaScript>]
let Tests =

    Section "Math"

    let d = 0.1
    let ( =~ ) a b  = abs (a - b) < 1E-3
    let ( =~? ) a b = a =~ b =? true

    let pFloat = R.Map abs R.Float

    Test "Math.Abs" {
        Math.Abs -1y  =? 1y
        Math.Abs -5s  =? 5s
        Math.Abs -3   =? 3
        Math.Abs -2L  =? 2L
        Math.Abs -4.f =? 4.f
        Math.Abs -6.  =? 6.
        Assert.For 100 R.Natural (fun x ->
            Math.Abs x =? x
            Math.Abs (-x) =? x)
        Assert.For 100 pFloat (fun x ->
            Math.Abs x =? x
            Math.Abs -x =? x)
    }

    Test "Math.Acos" {
        Math.Acos 0.44 =~? 1.115197653
    }

    Test "Math.Asin" {
        Math.Asin 0.44 =~? 0.4555986734
    }

    Test "Math.Atan" {
        Math.Atan 1.25 =~? 0.8960553846
    }

    Test "Math.Atan2" {
        Math.Atan2 (1.25, 2.4) =~? 0.4801750296
    }

    Test "Math.BigMul" {
        Math.BigMul (12, 13) =? 156L
    }

    Test "Math.Ceiling" {
        Math.Ceiling 1.01 =? 2.
        Math.Ceiling 1.   =? 1.
    }

    Test "Math.Cos" {
        Assert.For 100 R.Float (fun x ->
            Math.Cos x =~? Math.Cos (x + 2. * Math.PI))
    }

    Test "Math.Cosh" {
        Assert.For 100 R.Float (fun x ->
            Math.Cosh x =~? 1. / 2. * (Math.Exp x + Math.Exp -x))
    }

    Test "Math.E" {
        round (Math.E * 1000.) =? 2718.
    }

    Test "Math.Floor" {
        Math.Floor 1.0  =? 1.0
        Math.Floor 1.9  =? 1.0
    }

    Test "Math.Exp" {
        Assert.For 100 pFloat (fun x ->
            Math.Log (Math.Exp x) =~? x)
    }

    Test "Math.Log" {
        Math.Log 10.      =~? 2.302585
        Math.Log(10., 9.) =~? 1.047952
        Assert.For 100 pFloat (fun x ->
            Math.E ** Math.Log x =~? x)
        Assert.For 100 (R.Tuple2Of (pFloat, pFloat)) (fun (x, y) ->
            let x = x + 1.
            let y = y + 2.
            y ** Math.Log(x, y) =~? x)
    }

    Test "Math.Log10" {
        Math.Log10 10. =~? 1.0
        Assert.For 100 pFloat (fun x ->
            10. ** Math.Log10 x =~? x)
    }

    Test "Math.Max" {
        Math.Max(1., 2.)   =? 2.
        Math.Max(1.f, 2.f) =? 2.f
        Math.Max(1uy, 2uy) =? 2uy
        Math.Max(1y, 2y)   =? 2y
        Math.Max(1s, 2s)   =? 2s
        Math.Max(1, 2)     =? 2
        Math.Max(1L, 2L)   =? 2L
        Math.Max(1us, 2us) =? 2us
        Math.Max(1u, 2u)   =? 2u
        Math.Max(1UL, 2UL) =? 2UL
        Assert.For 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) ->
            let res = Math.Max(x, y)
            if x >= y then res =? x else res =? y)
        Assert.For 100 (R.Tuple2Of (R.Float, R.Float)) (fun (x, y) ->
            let res = Math.Max(x, y)
            if x >= y then res =? x else res =? y)
    }

    Test "Math.Min" {
        Math.Min(1., 2.)   =? 1.
        Math.Min(1.f, 2.f) =? 1.f
        Math.Min(1uy, 2uy) =? 1uy
        Math.Min(1y, 2y)   =? 1y
        Math.Min(1s, 2s)   =? 1s
        Math.Min(1, 2)     =? 1
        Math.Min(1L, 2L)   =? 1L
        Math.Min(1us, 2us) =? 1us
        Math.Min(1u, 2u)   =? 1u
        Math.Min(1UL, 2UL) =? 1UL
        Assert.For 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) ->
            let res = Math.Min(x, y)
            if x >= y then res =? y else res =? x)
        Assert.For 100 (R.Tuple2Of (R.Float, R.Float)) (fun (x, y) ->
            let res = Math.Min(x, y)
            if x >= y then res =? y else res =? x)
    }

    Test "Math.PI" {
        round (100. * Math.PI) =? 314.
    }

    Test "Math.Pow" {
        Math.Pow(2.8, 1.4) =~? 4.2269
        Assert.For 100 (R.Tuple2Of (pFloat, R.Float))
            (fun (x, y) -> Math.Pow(x, y) =~? x ** y)
    }

    Test "Math.Round" {
        Math.Round 1.5  =? 2.
        Assert.For 100 R.Float (fun x ->
            Math.Round x =? Math.Floor (x + 0.5))
    }

    Test "Math.Sign" {
        Math.Sign -3.  =? -1
        Math.Sign -3.f =? -1
        Math.Sign -3y  =? -1
        Math.Sign -3s  =? -1
        Math.Sign -3   =? -1
        Math.Sign -3L  =? -1
        Assert.For 100 R.Natural (fun x ->
            let x = x + 1
            Math.Sign x =? 1
            Math.Sign -x =? -1)
        Assert.For 100 pFloat (fun x ->
            let x = 1. + x
            Math.Sign x  =? 1
            Math.Sign -x =? -1)
    }

    Test "Math.Sin" {
        Math.Sin 1.25 =~? 0.9489846194
    }

    Test "Math.Sinh" {
        Math.Sinh 1.25 =~? 1.60191908
    }

    Test "Math.Sqrt" {
        Math.Sqrt 145. =~? 12.0416
        Assert.For 100 pFloat (fun x ->
            Math.Sqrt x ** 2. =~? x)
    }

    Test "Math.Tan" {
        Math.Tan 1.25 =~? 3.009569674
    }

    Test "Math.Tanh" {
        Math.Tanh 1.25 =~? 0.84828364
    }
