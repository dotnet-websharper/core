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

module WebSharper.Tests.Double

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.RandomValues
type private D = System.Double

[<JavaScript>]
let Tests =
    TestCategory "Double" {

        Test "Double.IsInfinity" {
            isTrue (D.IsInfinity infinity)
            isFalse (D.IsInfinity 0.)
        }

        Test "Double.IsNaN" {
            isTrue (D.IsNaN nan)
            isFalse (D.IsNaN 0.)
        }

        Test "Double.IsNegativeInfinity" {
            isFalse (D.IsNegativeInfinity infinity)
            isTrue (D.IsNegativeInfinity -infinity)
            isFalse (D.IsNegativeInfinity 0.)
        }

        Test "Double.IsPositiveInfinity" {
            isTrue (D.IsPositiveInfinity infinity)
            isFalse (D.IsPositiveInfinity -infinity)
            isFalse (D.IsPositiveInfinity 0.)
        }

        Test "Double.Parse" {
            equal (D.Parse "1.5E3") 1.5E3
        }

    }
