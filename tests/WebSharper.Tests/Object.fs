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

module WebSharper.Tests.Object

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type T = { K : int }

[<JavaScript>]
type O [<Inline "{}">] () = 
    member this.P1 
        with [<Inline "$this.p1">] get() = X<int>
        and  [<Inline "void($this.p1 = $v)">] set (v: int) = ()
    member this.P2 
        with [<Inline "$this.p2">] get() = X<int>
        and  [<Inline "void($this.p2 = $v)">] set (v: int) = ()
    [<OptionalField>]
    member this.P3 
        with get() = X<int option> 
        and  set (v: int option) = () 

type R = { [<OptionalField>] KO: int option }
type [<OptionalField>] R2 = { KO2: int option; K2 : int }

[<JavaScript>]
type RN () =
    [< Name "x" >]
    let mutable y = 0

    [<Inline>]
    member this.Value 
        with get() = y
        and  set v = y <- v

type Abcde = { A: string; B: string; C: string; D: string; E: string }

[<JavaScript>]
let Tests =
    Section "Object" {

        Test "Construction" {
            Equal (JS.TypeOf (obj ())) JS.Kind.Object
        }

        Test "Equals" {
            let a = { K = 4 }
            let b = { K = 4 }
            True (System.Object.Equals (a, b))
            True (a.Equals b)
            True (a = b)
        }

        Test "ReferenceEquals" {
            False (System.Object.ReferenceEquals (obj (), obj ()))
            let r = obj ()
            True (System.Object.ReferenceEquals (r, r))
            let a = { K = 4 }
            let b = { K = 4 }
            False (System.Object.ReferenceEquals(a, b))
        }

        Test "ToString" {
            Equal (JS.TypeOf (obj().ToString())) JS.Kind.String
        }

        Test "GetHashCode" {
            Equal (JS.TypeOf (obj().GetHashCode())) JS.Kind.Number
        }

        Test "Construction with properties" {
            let o = O(P1 = 1, P2 = 2)
            Equal o.P1 1
            Equal o.P2 2
        }

        Test "Optional fields" {
            let o = O()
            Equal o.P3 None
            o.P3 <- Some 1
            Equal o?P3 1
            Equal o.P3 (Some 1)
            let r = { KO = Some 2 }
            Equal r.KO (Some 2)
            Equal r?KO 2
            let r2 = { KO = None }
            Equal r2 (New [])
            let r3 = { KO2 = Some 1; K2 = 2 }
            Equal r3.KO2 (Some 1)
            Equal r3.K2 2
            Equal r3 (New [ "KO2" => 1; "K2" => 2 ])
            let r4 = { KO2 = None; K2 = 2 }
            Equal r4.KO2 None
            Equal r4.K2 2
            Equal r4 (New [ "K2" => 2 ])
        }

        Test "Optimized field access" {
            let x = ref ""
            let f i =
                x := !x + i
                i
            Equal {
                A = f "a"
                B = f "b"
                C = f "c"
                D = f "d"
                E = f "e"
            }.C "c"
            Equal !x "abcde"
        }

        Test "NewObject" {
            let o = Object<int>([| "a", 1; "b", 2 |])
            Equal o (New [ "a" => 1; "b" => 2 ])
        }

        Test "Field rename" {
            let o = RN()
            Equal o?x 0
            o.Value <- 1
            Equal o.Value 1
            Equal o?x 1
        }

        Test "Extensions" {
            True ((New [ "a" => 1 ]).JS.HasOwnProperty("a"))
            Equal Object.Prototype.Constructor.Length 1
        }

    }