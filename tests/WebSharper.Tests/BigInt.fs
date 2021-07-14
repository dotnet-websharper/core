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

module WebSharper.Tests.BigInt

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type private BI = System.Numerics.BigInteger

[<JavaScript>]
let Tests =
    TestCategory "BigInt" {
        Test "BigInt sanity check" {
            isTrue (true)
        }

        Test "BigInt operators" {
            let a = BI(100)
            let b = BI(200)
            let c = BI(300)

            let d = BI(2)
            let e = BI(3)
            let f = BI(8)

            equal (a + b) c
            equal (c - b) a
            equal (c % b) a
            equal (a * b) (BI(20000))
            // equal (d ** e) f
        }
    }