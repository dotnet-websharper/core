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

    let pFloat = R.Map abs R.Float

    Test "Math.Abs" {
        Equal (Math.Abs -1y)  1y
        Equal (Math.Abs -5s)  5s
        Equal (Math.Abs -3)   3
        Equal (Math.Abs -2L)  2L
        Equal (Math.Abs -4.f) 4.f
        Equal (Math.Abs -6.)  6.
        ForR 100 R.Natural (fun x -> Do {
            Equal (Math.Abs x) x
            Equal (Math.Abs -x) x
        })
        ForR 100 pFloat (fun x -> Do {
            Equal (Math.Abs x) x
            Equal (Math.Abs -x) x
        })
    }

    Test "Math.Acos" {
        ApproxEqual (Math.Acos 0.44) 1.115197653
    }

    Test "Math.Asin" {
        ApproxEqual (Math.Asin 0.44) 0.4555986734
    }

    Test "Math.Atan" {
        ApproxEqual (Math.Atan 1.25) 0.8960553846
    }

    Test "Math.Atan2" {
        ApproxEqual (Math.Atan2 (1.25, 2.4)) 0.4801750296
    }

    Test "Math.BigMul" {
        Equal (Math.BigMul (12, 13)) 156L
    }

    Test "Math.Ceiling" {
        Equal (Math.Ceiling 1.01) 2.
        Equal (Math.Ceiling 1.)   1.
    }

    Test "Math.Cos" {
        ForR 100 R.Float (fun x -> Do {
            ApproxEqual (Math.Cos x) (Math.Cos (x + 2. * Math.PI))
        })
    }

    Test "Math.Cosh" {
        ForR 100 R.Float (fun x -> Do {
            ApproxEqual (Math.Cosh x) (1. / 2. * (Math.Exp x + Math.Exp -x))
        })
    }

    Test "Math.E" {
        Equal (round (Math.E * 1000.)) 2718.
    }

    Test "Math.Floor" {
        Equal (Math.Floor 1.0)  1.0
        Equal (Math.Floor 1.9)  1.0
    }

    Test "Math.Exp" {
        ForR 100 pFloat (fun x -> Do {
            ApproxEqual (Math.Log (Math.Exp x)) x
        })
    }

    Test "Math.Log" {
        ApproxEqual (Math.Log 10.)      2.302585
        ApproxEqual (Math.Log(10., 9.)) 1.047952
        ForR 100 pFloat (fun x -> Do {
            ApproxEqual (Math.E ** Math.Log x) x
        })
        ForR 100 (R.Tuple2Of (pFloat, pFloat)) (fun (x, y) -> Do {
            let x = x + 1.
            let y = y + 2.
            ApproxEqual (y ** Math.Log(x, y)) x
        })
    }

    Test "Math.Log10" {
        ApproxEqual (Math.Log10 10.) 1.0
        ForR 100 pFloat (fun x -> Do {
            ApproxEqual (10. ** Math.Log10 x) x
        })
    }

    Test "Math.Max" {
        Equal (Math.Max(1., 2.))   2.
        Equal (Math.Max(1.f, 2.f)) 2.f
        Equal (Math.Max(1uy, 2uy)) 2uy
        Equal (Math.Max(1y, 2y))   2y
        Equal (Math.Max(1s, 2s))   2s
        Equal (Math.Max(1, 2))     2
        Equal (Math.Max(1L, 2L))   2L
        Equal (Math.Max(1us, 2us)) 2us
        Equal (Math.Max(1u, 2u))   2u
        Equal (Math.Max(1UL, 2UL)) 2UL
        ForR 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) ->
            let res = Math.Max(x, y)
            if x >= y then
                Do { Equal res x }
            else Do { Equal res y }
        )
        ForR 100 (R.Tuple2Of (R.Float, R.Float)) (fun (x, y) ->
            let res = Math.Max(x, y)
            if x >= y then
                Do { Equal res x }
            else Do { Equal res y }
        )
    }

    Test "Math.Min" {
        Equal (Math.Min(1., 2.))   1.
        Equal (Math.Min(1.f, 2.f)) 1.f
        Equal (Math.Min(1uy, 2uy)) 1uy
        Equal (Math.Min(1y, 2y))   1y
        Equal (Math.Min(1s, 2s))   1s
        Equal (Math.Min(1, 2))     1
        Equal (Math.Min(1L, 2L))   1L
        Equal (Math.Min(1us, 2us)) 1us
        Equal (Math.Min(1u, 2u))   1u
        Equal (Math.Min(1UL, 2UL)) 1UL
        ForR 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) ->
            let res = Math.Min(x, y)
            if x >= y then
                Do { Equal res y }
            else Do { Equal res x }
        )
        ForR 100 (R.Tuple2Of (R.Float, R.Float)) (fun (x, y) ->
            let res = Math.Min(x, y)
            if x >= y then
                Do { Equal res y }
            else Do { Equal res x }
        )
    }

    Test "Math.PI" {
        Equal (round (100. * Math.PI)) 314.
    }

    Test "Math.Pow" {
        ApproxEqual (Math.Pow(2.8, 1.4)) 4.2269
        ForR 100 (R.Tuple2Of (pFloat, R.Float)) (fun (x, y) -> Do {
            ApproxEqual (Math.Pow(x, y)) (x ** y)
        })
    }

    Test "Math.Round" {
        Equal (Math.Round 1.5) 2.
        ForR 100 R.Float (fun x -> Do {
            Equal (Math.Round x) (Math.Floor (x + 0.5))
        })
    }

    Test "Math.Sign" {
        Equal (Math.Sign -3.)  -1
        Equal (Math.Sign -3.f) -1
        Equal (Math.Sign -3y)  -1
        Equal (Math.Sign -3s)  -1
        Equal (Math.Sign -3)   -1
        Equal (Math.Sign -3L)  -1
        ForR 100 R.Natural (fun x -> Do {
            let x = x + 1
            Equal (Math.Sign x) 1
            Equal (Math.Sign -x) -1
        })
        ForR 100 pFloat (fun x -> Do {
            let x = 1. + x
            Equal (Math.Sign x)  1
            Equal (Math.Sign -x) -1
        })
    }

    Test "Math.Sin" {
        ApproxEqual (Math.Sin 1.25) 0.9489846194
    }

    Test "Math.Sinh" {
        ApproxEqual (Math.Sinh 1.25) 1.60191908
    }

    Test "Math.Sqrt" {
        ApproxEqual (Math.Sqrt 145.) 12.0416
        ForR 100 pFloat (fun x -> Do {
            ApproxEqual (Math.Sqrt x ** 2.) x
        })
    }

    Test "Math.Tan" {
        ApproxEqual (Math.Tan 1.25) 3.009569674
    }

    Test "Math.Tanh" {
        ApproxEqual (Math.Tanh 1.25) 0.84828364
    }
