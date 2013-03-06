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

module IntelliFactory.WebSharper.Tests.Object

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module J = IntelliFactory.WebSharper.JavaScript

type T = { K : int }

[<JavaScript>]
let Tests =
    Section "Object"

    Test "Construction" {
        J.TypeOf (obj ()) =? J.Kind.Object
    }

    Test "Equals" {
        let a = { K = 4 }
        let b = { K = 4 }
        System.Object.Equals (a, b) =? true
        a.Equals b =? true
        a = b =? true
    }

    Test "ReferenceEquals" {
        System.Object.ReferenceEquals (obj (), obj ()) =? false
        let r = obj ()
        System.Object.ReferenceEquals (r, r) =? true
        let a = { K = 4 }
        let b = { K = 4 }
        System.Object.ReferenceEquals(a, b) =? false
    }

    Test "ToString" {
        J.TypeOf (obj().ToString()) =? J.Kind.String
    }

    Test "GetHashCode" {
        J.TypeOf (obj().GetHashCode()) =? J.Kind.Number
    }

