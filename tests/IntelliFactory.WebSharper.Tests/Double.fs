// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Tests.Double

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random
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
