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
    Section "Double"

    Test "Double.IsInfinity" {
        D.IsInfinity infinity =? true
        D.IsInfinity 0. =? false
    }

    Test "Double.IsNaN" {
        D.IsNaN nan =? true
        D.IsNaN 0. =? false
    }

    Test "Double.IsNegativeInfinity" {
        D.IsNegativeInfinity infinity =? false
        D.IsNegativeInfinity -infinity =? true
        D.IsNegativeInfinity 0. =? false
    }

    Test "Double.IsPositiveInfinity" {
        D.IsPositiveInfinity infinity =? true
        D.IsPositiveInfinity -infinity =? false
        D.IsPositiveInfinity 0. =? false
    }

    Test "Double.Parse" {
        D.Parse "1.5E3" =? 1.5E3
    }
