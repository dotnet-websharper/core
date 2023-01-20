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

module WebSharper.Tests.Math

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.RandomValues
type private Math = System.Math

[<JavaScript>]
let Tests =

    TestCategory "Math" {

        let positiveFloat = R.Map abs R.Float

        Test "Math.Abs" {
            equal (Math.Abs -1y)  1y
            equal (Math.Abs -5s)  5s
            equal (Math.Abs -3)   3
            equal (Math.Abs -2L)  2L
            equal (Math.Abs -4.f) 4.f
            equal (Math.Abs -6.)  6.
            equal (abs -1y)  1y
            equal (abs -5s)  5s
            equal (abs -3)   3
            equal (abs -2L)  2L
            equal (abs -4.f) 4.f
            equal (abs -6.)  6.
            propertyWith R.Natural (fun x -> Do {
                equal (Math.Abs x) x
                equal (Math.Abs -x) x
                equal (abs x) x
                equal (abs -x) x
            })
            propertyWith positiveFloat (fun x -> Do {
                equal (Math.Abs x) x
                equal (Math.Abs -x) x
                equal (abs x) x
                equal (abs -x) x
            })
        }

        Test "Math.Acos" {
            approxEqual (Math.Acos 0.44) 1.115197653
            approxEqual (acos 0.44) 1.115197653
        }

        Test "Math.Asin" {
            approxEqual (Math.Asin 0.44) 0.4555986734
            approxEqual (asin 0.44) 0.4555986734
        }

        Test "Math.Atan" {
            approxEqual (Math.Atan 1.25) 0.8960553846
            approxEqual (atan 1.25) 0.8960553846
        }

        Test "Math.Atan2" {
            approxEqual (Math.Atan2 (1.25, 2.4)) 0.4801750296
            approxEqual (atan2 1.25 2.4) 0.4801750296
        }

        Test "Math.BigMul" {
            equal (Math.BigMul (12, 13)) 156L
        }

        Test "Math.Ceiling" {
            equal (Math.Ceiling 1.01) 2.
            equal (Math.Ceiling 1.)   1.
            equal (ceil 1.01) 2.
            equal (ceil 1.)   1.
        }

        Property "Math.Cos" (fun x -> Do {
            approxEqual (Math.Cos x) (Math.Cos (x + 2. * Math.PI))
            approxEqual (cos x) (cos (x + 2. * Math.PI))
        })

        Property "Math.Cosh" (fun x -> Do {
            approxEqual (Math.Cosh x) (1. / 2. * (Math.Exp x + Math.Exp -x))
            approxEqual (cosh x) (1. / 2. * (exp x + exp -x))
        })

        Test "Math.E" {
            equal (round (Math.E * 1000.)) 2718.
        }

        Test "Math.Floor" {
            equal (Math.Floor 1.0)  1.0
            equal (Math.Floor 1.9)  1.0
            equal (floor 1.0)  1.0
            equal (floor 1.9)  1.0
        }

        PropertyWith "Math.Exp" positiveFloat (fun x -> Do {
            approxEqual (Math.Log (Math.Exp x)) x
            approxEqual (log (exp x)) x
        })

        Test "Math.Log" {
            approxEqual (Math.Log 10.)      2.302585
            approxEqual (Math.Log(10., 9.)) 1.047952
            approxEqual (log 10.)      2.302585
            propertyWith positiveFloat (fun x -> Do {
                approxEqual (Math.E ** Math.Log x) x
            })
            propertyWith (R.Tuple2Of (positiveFloat, positiveFloat)) (fun (x, y) -> Do {
                let x = x + 1.
                let y = y + 2.
                approxEqual (y ** Math.Log(x, y)) x
            })
        }

        Test "Math.Log10" {
            approxEqual (Math.Log10 10.) 1.0
            approxEqual (log10 10.) 1.0
            propertyWith positiveFloat (fun x -> Do {
                approxEqual (10. ** Math.Log10 x) x
            })
        }

        Test "Math.Max" {
            equal (Math.Max(1., 2.))   2.
            equal (Math.Max(1.f, 2.f)) 2.f
            equal (Math.Max(1uy, 2uy)) 2uy
            equal (Math.Max(1y, 2y))   2y
            equal (Math.Max(1s, 2s))   2s
            equal (Math.Max(1, 2))     2
            equal (Math.Max(1L, 2L))   2L
            equal (Math.Max(1us, 2us)) 2us
            equal (Math.Max(1u, 2u))   2u
            equal (Math.Max(1UL, 2UL)) 2UL
            property (fun (x: float, y: float) ->
                let res = Math.Max(x, y)
                if x >= y then
                    Do { equal res x }
                else Do { equal res y }
            )
            property (fun (x: float, y: float) ->
                let res = Math.Max(x, y)
                if x >= y then
                    Do { equal res x }
                else Do { equal res y }
            )
        }

        Test "Math.Min" {
            equal (Math.Min(1., 2.))   1.
            equal (Math.Min(1.f, 2.f)) 1.f
            equal (Math.Min(1uy, 2uy)) 1uy
            equal (Math.Min(1y, 2y))   1y
            equal (Math.Min(1s, 2s))   1s
            equal (Math.Min(1, 2))     1
            equal (Math.Min(1L, 2L))   1L
            equal (Math.Min(1us, 2us)) 1us
            equal (Math.Min(1u, 2u))   1u
            equal (Math.Min(1UL, 2UL)) 1UL
            property (fun (x: float, y: float) ->
                let res = Math.Min(x, y)
                if x >= y then
                    Do { equal res y }
                else Do { equal res x }
            )
            property (fun (x: float, y: float) ->
                let res = Math.Min(x, y)
                if x >= y then
                    Do { equal res y }
                else Do { equal res x }
            )
        }

        Test "Math.PI" {
            equal (round (100. * Math.PI)) 314.
        }

        Test "Math.Pow" {
            approxEqual (Math.Pow(2.8, 1.4)) 4.2269
            approxEqual (2.8 ** 1.4) 4.2269
            propertyWith (R.Tuple2Of (positiveFloat, R.Float)) (fun (x, y) -> Do {
                approxEqual (Math.Pow(x, y)) (x ** y)
            })
        }

        Test "Math.Round" {
            equal (Math.Round 1.5) 2.
            equal (round 1.5) 2.
            property (fun (x: float) -> Do {
                equal (Math.Round x) (Math.Floor (x + 0.5))
            })
        }

        Test "Math.Sign" {
            equal (Math.Sign -3.)  -1
            equal (Math.Sign -3.f) -1
            equal (Math.Sign -3y)  -1
            equal (Math.Sign -3s)  -1
            equal (Math.Sign -3)   -1
            equal (Math.Sign -3L)  -1
            propertyWith R.Natural (fun x -> Do {
                let x = x + 1
                equal (Math.Sign x) 1
                equal (Math.Sign -x) -1
            })
            propertyWith positiveFloat (fun x -> Do {
                let x = 1. + x
                equal (Math.Sign x)  1
                equal (Math.Sign -x) -1
            })
        }

        Test "Math.Sin" {
            approxEqual (Math.Sin 1.25) 0.9489846194
            approxEqual (sin 1.25) 0.9489846194
        }

        Test "Math.Sinh" {
            approxEqual (Math.Sinh 1.25) 1.60191908
            approxEqual (sinh 1.25) 1.60191908
        }

        Test "Math.Sqrt" {
            approxEqual (Math.Sqrt 145.) 12.0416
            approxEqual (sqrt 145.) 12.0416
            propertyWith positiveFloat (fun x -> Do {
                approxEqual (Math.Sqrt x ** 2.) x
            })
        }

        Test "Math.Tan" {
            approxEqual (Math.Tan 1.25) 3.009569674
            approxEqual (tan 1.25) 3.009569674
        }

        Test "Math.Tanh" {
            approxEqual (Math.Tanh 1.25) 0.84828364
            approxEqual (tanh 1.25) 0.84828364
        }

    }
