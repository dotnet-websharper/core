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

module WebSharper.Tests.Nullable

open System

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

open FSharp.Linq
open Linq.NullableOperators

[<JavaScript>]
let Tests =
    TestCategory "Nullable" {

        Test "Value" {
            raises (Nullable<int>().Value)
            equal (Nullable(2).Value) 2
        }

        Test "HasValue" {
            isFalse (Nullable<int>().HasValue)
            isTrue (Nullable(2).HasValue)
        }

        Test "GetValueOrDefault(x)" {
            equal (Nullable<int>().GetValueOrDefault(2)) 2
            equal (Nullable(1).GetValueOrDefault(2)) 1
        }

        Test "GetValueOrDefault()" {
            equal (Nullable<int>().GetValueOrDefault()) 0
            equal (Nullable(32).GetValueOrDefault()) 32
        }

        Test "Operators" {
            equal (Nullable 1 ?+ 1) (Nullable 2)
            equal (Nullable() ?+ 1) (Nullable ())
            equal (Nullable 1 ?+? Nullable 1) (Nullable 2)
            equal (Nullable 1 ?+? Nullable()) (Nullable ())
            isFalse (Nullable () ?< 0)
            isFalse (Nullable () ?> 0)
            isTrue (Nullable () ?=? Nullable ())
            isTrue (Nullable 1 ?=? Nullable 1)
            equal (Nullable 2 ?*? Nullable 2) (Nullable 4)
        }

        Test "int" {
            equal (Nullable.int (Nullable 3.5)) (Nullable 3)
            equal (Nullable.int (Nullable -3.5)) (Nullable -3)
            equal (Nullable.int (Nullable 'a')) (Nullable 97)
        }

        Test "Auto-conversion" {
            equal (WebSharper.CSharp.Tests.Interop.AddNullables(2, 3)) 5
            equal (WebSharper.CSharp.Tests.Interop.AddNullables(2, Nullable())) 2
        }
    }
