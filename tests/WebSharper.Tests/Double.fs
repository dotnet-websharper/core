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

module WebSharper.Tests.Double

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random
type private D = System.Double

[<JavaScript>]
let Tests =
    Section "Double" {

        Test "Double.IsInfinity" {
            True (D.IsInfinity infinity)
            False (D.IsInfinity 0.)
        }

        Test "Double.IsNaN" {
            True (D.IsNaN nan)
            False (D.IsNaN 0.)
        }

        Test "Double.IsNegativeInfinity" {
            False (D.IsNegativeInfinity infinity)
            True (D.IsNegativeInfinity -infinity)
            False (D.IsNegativeInfinity 0.)
        }

        Test "Double.IsPositiveInfinity" {
            True (D.IsPositiveInfinity infinity)
            False (D.IsPositiveInfinity -infinity)
            False (D.IsPositiveInfinity 0.)
        }

        Test "Double.Parse" {
            Equal (D.Parse "1.5E3") 1.5E3
        }

    }