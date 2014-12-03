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

module IntelliFactory.WebSharper.Tests.Object

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module J = IntelliFactory.WebSharper.JavaScript

type T = { K : int }

[<JavaScript>]
type O [<Inline "{}">] () = 
    member this.P1 
        with [<Inline "$this.p1">] get() = X<int>
        and  [<Inline "void($this.p1 = $v)">] set v = ()
    member this.P2 
        with [<Inline "$this.p2">] get() = X<int>
        and  [<Inline "void($this.p2 = $v)">] set v = ()

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

    Test "Construction with properties" {
        let o = O(P1 = 1, P2 = 2)
        o.P1 =? 1
        o.P2 =? 2
    }
