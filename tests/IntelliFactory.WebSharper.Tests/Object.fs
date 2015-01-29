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
open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.Testing

type T = { K : int }

[<JavaScript>]
type O [<Inline "{}">] () = 
    member this.P1 
        with [<Inline "$this.p1">] get() = X<int>
        and  [<Inline "void($this.p1 = $v)">] set v = ()
    member this.P2 
        with [<Inline "$this.p2">] get() = X<int>
        and  [<Inline "void($this.p2 = $v)">] set v = ()
    [<OptionalField>]
    member this.P3 
        with get() = X<int option> 
        and  set v = () 

type R = { [<OptionalField>] KO: int option }

[<JavaScript>]
let Tests =
    Section "Object"

    Test "Construction" {
        JS.TypeOf (obj ()) =? JS.Kind.Object
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
        JS.TypeOf (obj().ToString()) =? JS.Kind.String
    }

    Test "GetHashCode" {
        JS.TypeOf (obj().GetHashCode()) =? JS.Kind.Number
    }

    Test "Construction with properties" {
        let o = O(P1 = 1, P2 = 2)
        o.P1 =? 1
        o.P2 =? 2
    }

    Test "Optional fields" {
        let o = O()
        o.P3 =? None
        o.P3 <- Some 1
        o?P3 =? 1
        o.P3 =? Some 1
        let r = { KO = Some 2 }
        r.KO =? Some 2
        r?KO =? 2
        let r2 = { KO = None }
        r2 =? New []
    }
