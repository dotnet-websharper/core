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

module WebSharper.Tests.Enum

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type E =
    | A   = 0b001
    | B   = 0b010
    | AB  = 0b011
    | C   = 0b100
    | ABC = 0b111

[<JavaScript>]
let Tests =
    TestCategory "Enums" {
        Test "Base" {
            equal (int E.A) 1
            equal (float E.A) 1.
            equal (int E.B) 2
            equal (int E.AB) 3
            equal (int E.C) 4
        }

        Test "Operators" {
            equal (int (E.A &&& E.B)) 0
            equal (E.A ||| E.B) E.AB
            equal (int (E.A ||| E.C)) 0b101
        }

        Test "HasFlag" {
            isTrue (E.A.HasFlag E.A)
            isTrue (E.B.HasFlag E.B)
            isTrue (E.AB.HasFlag E.A)
            isTrue (E.AB.HasFlag E.B)
            isTrue (E.AB.HasFlag E.AB)
            isTrue (E.ABC.HasFlag E.A)
            isTrue (E.ABC.HasFlag E.B)
            isTrue (E.ABC.HasFlag E.C)
            isTrue (E.ABC.HasFlag E.AB)
            isFalse (E.A.HasFlag E.AB)
            isFalse (E.B.HasFlag E.AB)
            isFalse (E.A.HasFlag E.B)
            isFalse (E.B.HasFlag E.A)
        }

        Test "Comparison" {
            isTrue (E.A = E.A)
            isTrue (E.A <= E.A)
            isTrue (E.A < E.B)
        }
    }
