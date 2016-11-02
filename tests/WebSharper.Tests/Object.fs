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

module WebSharper.Tests.Object

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>] type T = { K : int }

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

[<JavaScript>]
type R = { [<OptionalField>] KO: int option }

[<JavaScript>]
type [<OptionalField>] R2 = { KO2: int option; K2 : int }

/// non JS-annotated version of R
type R4 = { [<OptionalField>] KO4: int option }

/// non JS-annotated version of R2
type [<OptionalField>] R5 = { KO5: int option; K5 : int }

type I = 
    abstract Get: unit -> int

[<Name "I2">]
type I2 = 
    abstract Get: unit -> int

[<Name "">]
type I3 = 
    abstract Get: unit -> int

type I4 = 
    [<Name "I4Value">]
    abstract Value: int with get, set

type R3 =
    { mutable R3A : int }
    
    interface I with
        [<JavaScript>]
        member this.Get() = this.R3A

    interface I2 with
        [<JavaScript>]
        member this.Get() = this.R3A

    interface I3 with
        [<JavaScript>]
        member this.Get() = this.R3A

    interface I4 with
        [<JavaScript>]
        member this.Value
            with get() = this.R3A
            and set v  = this.R3A <- v

[<JavaScript>]
type RN () =
    [< Name "x" >]
    let mutable y = 0

    [<Name "X">]
    member this.RNValue 
        with get() = y
        and  set v = y <- v

    [<Inline>]
    member this.Value 
        with get() = y
        and  set v = y <- v

[<JavaScript>]
type RNWithStub() =
    [<Stub; Name "x">]
    member val y = 0 with get, set

    [<Stub>]
    member val x = 0 with get, set

[<JavaScript; Struct>]
type TestStruct =
    val X : int
    val Y : string
    new (x, y) = { X = x; Y = y }

    member this.X2 = this.X

#if FSHARP41
[<JavaScript; Struct>]
type StructUnion = SU of int

[<JavaScript; Struct>]
type StructUnion2 = SU2 of int * string

[<JavaScript; Struct>]
type StructRecord =
    { SR : int }

[<JavaScript; Struct>]
type StructRecord2 =
    { SR2 : int; SR2b : string }
#endif

[<JavaScript>]
type Abcde = { A: string; B: string; C: string; D: string; E: string }

[<JavaScript>]
let mutable TestValue = 0

[<JavaScript>]
module RunThisOnClient =
    do TestValue <- 1

    [<JavaScript(false)>]
    module DontRunThisOnClient =
        do TestValue <- 2

    do TestValue <- TestValue + 2

do TestValue <- TestValue + 4

[<JavaScript>]
let Tests =
    TestCategory "Object" {

        Test "Construction" {
            equal (JS.TypeOf (obj ())) JS.Kind.Object
        }

        Test "Equals" {
            let a = { K = 4 }
            let b = { K = 4 }
            isTrue (System.Object.Equals (a, b))
            isTrue (a.Equals b)
            isTrue (a = b)
        }

        Test "ReferenceEquals" {
            isFalse (System.Object.ReferenceEquals (obj (), obj ()))
            let r = obj ()
            isTrue (System.Object.ReferenceEquals (r, r))
            let a = { K = 4 }
            let b = { K = 4 }
            isFalse (System.Object.ReferenceEquals(a, b))
        }

        Test "ToString" {
            equal (JS.TypeOf (obj().ToString())) JS.Kind.String
        }

        Test "GetHashCode" {
            equal (JS.TypeOf (obj().GetHashCode())) JS.Kind.Number
        }

        Test "Unchecked.defaultof" {
            equal Unchecked.defaultof<int> 0
            equal Unchecked.defaultof<float> 0.
            equal Unchecked.defaultof<Abcde> (As null)
        }

        Test "Construction with properties" {
            let o = O(P1 = 1, P2 = 2)
            equal o.P1 1
            equal o.P2 2
        }

        Test "Optional fields" {
            let o = O()
            equal o.P3 None
            o.P3 <- Some 1
            equal o?P3 1
            equal o.P3 (Some 1)

            let r = { KO = Some 2 }
            equal r.KO (Some 2)
            equal r?KO 2
            let r2 = { KO = None }
            equal r2 (New [])
            let r3 = { KO2 = Some 1; K2 = 2 }
            equal r3.KO2 (Some 1)
            equal r3.K2 2
            equal r3 (New [ "KO2" => 1; "K2" => 2 ])
            let r4 = { KO2 = None; K2 = 2 }
            equal r4.KO2 None
            equal r4.K2 2
            equal r4 (New [ "K2" => 2 ])

            // non JS-annotated versions
            let r = { KO4 = Some 2 }
            equal r.KO4 (Some 2)
            equal r?KO4 2
            let r2 = { KO4 = None }
            equal r2 (New [])
            let r3 = { KO5 = Some 1; K5 = 2 }
            equal r3.KO5 (Some 1)
            equal r3.K5 2
            equal r3 (New [ "KO5" => 1; "K5" => 2 ])
            let r4 = { KO5 = None; K5 = 2 }
            equal r4.KO5 None
            equal r4.K5 2
            equal r4 (New [ "K5" => 2 ])
        }

        Test "Optimized field access" {
            let x = ref ""
            let f i =
                x := !x + i
                i
            equal {
                A = f "a"
                B = f "b"
                C = f "c"
                D = f "d"
                E = f "e"
            }.C "c"
            equal !x "abcde"
        }

        Test "NewObject" {
            let o = Object<int>([| "a", 1; "b", 2 |])
            equal o (New [ "a" => 1; "b" => 2 ])
        }

        Test "Field rename" {
            let o = RN()
            equal o?x 0
            o.Value <- 1
            equal o.Value 1
            equal o?x 1
        }

        Test "Property rename" {
            let o = RN()
            equal (o?X()) 0
            o.Value <- 1
            equal (o?X()) 1
            equal o.RNValue 1
        }

        Test "Extensions" {
            isTrue ((New [ "a" => 1 ]).JS.HasOwnProperty("a"))
            equal Object.Prototype.Constructor.Length 1
        }

        #if FSHARP40

        Test "isNull" {
            isTrue (isNull null)
            isFalse (isNull (obj()))
        }

        #endif

        Test "Stub property" {
            let o = RN(Value = 1)
            let c = As<RNWithStub> o
            equal c.x 1
            c.x <- 2
            equal c.x 2
            equal c.y 2
            c.y <- 3
            equal c.y 3
        }

        Test "Module do" {
            equal TestValue 3
        }

        Test "Record with interface" {
            let r = { R3A = 4 }
            equal ((r :> I).Get()) 4
            equal ((r :> I2)?``I2$Get``()) 4
            equal ((r :> I3)?Get()) 4
            equal ((r :> I4)?I4Value()) 4
            (r :> I4).Value <- 5
            equal ((r :> I4)?I4Value()) 5
        }

        Test "Struct" {
            equal (TestStruct().X) 0
            equal (TestStruct(1, "").X) 1
        }
        
        #if FSHARP41
        Test "Struct union" {
            let f x =
                Console.Log "deconstructing struct union"
                match x with
                | SU a -> a
            equal (f(SU 1)) 1
            let g x =
                Console.Log "deconstructing struct union 2"
                match x with
                | SU2 (a, b) -> (a, b)
            equal (g(SU2 (1, "a"))) (1, "a")
        }

        Test "Struct record" {
            let x = { SR = 1 }
            equal x.SR 1
            let y = { SR2 = 1; SR2b = "a" }
            equal y.SR2 1
            equal y.SR2b "a"
        }
        #endif

    }
