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
            equal (b / a) (BI(2))
            equal (c / b) (BI(1))
            equal (d ** 3) f
        }

        Test "BigInt comparison" {
            let a = BI(100)
            let b = BI(200)

            isFalse (a > b)
            isFalse (a >= b)
            isTrue (a < b)
            isTrue (a <= b)
            isTrue (a >= (BI(100)))
            isTrue (a <= (BI(100)))
        }

        Test "BigInt constructors" {
            let a = BI(1000)

            // from byte arr
            equal a (BI([|232uy; 3uy; 0uy; 0uy|]))
        }

        Test "FSharp bigint literals" {
            let zero1 = 0I
            let zero2 = BI(0)
            let one1 = 1I
            let one2 = BI(1)
            let int32test1 = 100I
            let int32test2 = BI(100)
            let int64test1 = 255486129307I
            let int64test2 = BI(255486129307L)
            let fromstringtest1 = 10000000000000000000I
            let fromstringtest2 = BI(5000000000000000000L + 5000000000000000000L)

            equal zero1 zero2
            equal one1 one2
            equal zero1 zero2
            equal int32test1 int32test2
            equal int64test1 int64test2
            equal fromstringtest1 fromstringtest2
        }
    }