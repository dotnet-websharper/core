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

    let mutable x = 1

    [<Name "X">]
    member this.RNValue 
        with get() = y
        and  set v = y <- v

    member this.X 
        with get() = x
        and  set v = x <- v

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

    member this.X2 = this.X + 1

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

[<JavaScript; Struct>]
type Position =
    {
        pos_fname: string
        pos_lnum: int
        pos_orig_lnum: int
    }

    member pos.Line = pos.pos_lnum

    member pos.NextLine =
        let pos = pos
        { pos with
            pos_lnum = pos.Line + 1
        }

[<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>]
type U =
    | U0
    | U1 of int 
    | U2 of string

[<JavaScript>]
type UJ =
    | [<Constant null>] UJ0
    | UJ1 of int 
    | UJ2 of string

[<JavaScript>]
[<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>]
type UN =
    | UN0
    | UN1 of int 
    | UN2 of string

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
type Farm() as this =
//    let cow = this.Cow() // this fails in .NET
    member this.Cow() = "moo"
    member x.Self() = this
//    member this.CowToo() = cow

[<JavaScript; Prototype false>]
type ClassWithNoPrototype(x) =
    let y = x + 1

    member this.Y() = y
    member this.X(a) = this.Y() - 1 + a

[<JavaScript>]
type MyObj () =
    class end

[<JavaScript>]
type AlwaysEqual (x) =
    member this.Value: int = x
    override this.Equals(o) = true
    override this.GetHashCode() = 0

type IA<'T> =
    abstract member Get : unit -> 'T

[<JavaScript>]
type MyType() =
    member __.M() = ()

    interface WebSharper.CSharp.Tests.IFoo

//[<JavaScript>]
//type MultipleIntf() =
//    interface IA<int> with
//        member x.Get() = 1
//    interface IA<string> with
//        member x.Get() = "hello"

[<JavaScript; Name "">]
type IParseState =
    abstract RaiseError<'b> : unit -> 'b

[<JavaScript; Name "">]
type IParseState2 =
    abstract RaiseError<'a, 'b> : unit -> 'a * 'b

[<JavaScript; AbstractClass>]
type ParseStateAbstract() =
    abstract RaiseError<'b> : unit -> 'b

[<JavaScript>]
type ParseState() =
    interface IParseState with
        member _.RaiseError()  = raise <| System.Exception("asd")

[<JavaScript>]
let Tests =
    TestCategory "Object" {

        Test "Construction" {
            equal (JS.TypeOf (obj ())) JS.Kind.Object
        }

        Test "Equals" {
            isFalse (System.Object.Equals (obj(), obj()))
            isFalse (obj().Equals(obj()))
            isFalse (obj() = obj())
            let r = obj ()
            isFalse (System.Object.Equals (MyObj(), MyObj()))
            isFalse (MyObj().Equals(MyObj()))
            isFalse (MyObj() = MyObj())
            let r = MyObj()
            isTrue (System.Object.Equals (r, r))
            isTrue (r.Equals(r))
            isTrue (r = r)
            let a = { K = 4 }
            let b = { K = 4 }
            isTrue (System.Object.Equals (a, b))
            isTrue (a.Equals b)
            isTrue (a = b)
        }

        Test "ReferenceEquals" {
            isFalse (System.Object.ReferenceEquals (obj(), obj()))
            let r = obj()
            isTrue (System.Object.ReferenceEquals (r, r))
            let a = { K = 4 }
            let b = { K = 4 }
            isFalse (System.Object.ReferenceEquals(a, b))
        }

        Test "Overridden Equals method" {
            let a = AlwaysEqual 1
            let b = AlwaysEqual 2
            equalAsync (async { return 1 }) 1
            equalMsg a b "WebSharper's equality"
            isFalseMsg (a ==. b) "JS equality not "
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

            jsEqual (r.JS.Constructor) (JS.Global?Object)
            jsEqual (r3.JS.Constructor) (JS.Global?Object)

            let ru = Union1Of2 r3 : Union<_, string>
            equal (match ru with Union1Of2 r -> r.K5 | _ -> 0) 2

            let ru2 = Union2Of2 r3 : Union<string, _>
            equal (match ru2 with Union2Of2 r -> r.K5 | _ -> 0) 2
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
            o.RNValue <- 2
            equal o.RNValue 2

            equal o.X 1
            o.X <- 4
            equal o.X 4
        }

        Test "Extensions" {
            isTrue ((New [ "a" => 1 ]).JS.HasOwnProperty("a"))
            equal Object.Prototype.Constructor.Length 1
        }

        Test "isNull" {
            isTrue (isNull null)
            isFalse (isNull (obj()))
        }

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
            equal ((r :> I2).Get()) 4
            equal ((r :> I2)?``I2$Get``()) 4
            equal ((r :> I3).Get()) 4
            equal ((r :> I3)?Get()) 4
            equal ((r :> I4).Value) 4
            equal ((r :> I4)?I4Value()) 4
            (r :> I4).Value <- 5
            equal ((r :> I4)?I4Value()) 5
        }

        Test "Struct" {
            equal (TestStruct().X) 0
            equal (TestStruct().Y) null
            equal (TestStruct(1, "").X) 1
            equal (TestStruct(1, "").X2) 2
            equal (TestStruct(1, "a").X) (TestStruct(1, "a").X)
            notEqual (TestStruct(1, "a")) (TestStruct(2, "a"))
            notEqual (TestStruct(1, "a")) (TestStruct(1, "b"))
        }

        Test "Union with Constant null" {
            let o = ref ""
            let f u =
                match u with
                | U0 -> ()
                | U1 x -> o := string x
                | U2 x -> o := x
            notEqual (box U0) null 
            f U0 
            equal !o ""
            f (U1 3) 
            equal !o "3"
            f (U2 "hi") 
            equal !o "hi"

            let fj u =
                match u with
                | UJ0 -> ()
                | UJ1 x -> o := string x
                | UJ2 x -> o := x
            jsEqual (box UJ0) null
            o := ""
            fj UJ0
            equal !o ""
            fj (UJ1 3)
            equal !o "3"
            fj (UJ2 "hi")
            equal !o "hi"

            let fn u =
                match u with
                | UN0 -> ()
                | UN1 x -> o := string x
                | UN2 x -> o := x
            jsEqual (box UN0) null
            o := ""
            fn UN0
            equal !o ""
            fn (UN1 3)
            equal !o "3"
            fn (UN2 "hi")
            equal !o "hi"
        }

        Test "Object with `as` alias" {
            let o = Farm()
            equal (o.Cow()) "moo"
            equal (o.Self().Cow()) "moo"
            // jsEqual o?this JS.Undefined // this is not a problem now with reference equality on objects
        } 

        Test "Class with Prototype(false)" {
            let o = ClassWithNoPrototype(40)
            equal (o.X(2)) 42
            jsEqual (o.JS.Constructor) (JS.Global?Object)
        }

        Test "Type test against interface" {
            let a = box { new I with member this.Get() = 1 }
            let b = box { new I2 with member this.Get() = 2 }
            isTrue (a :? I)
            isFalse (a :? I2)
            isTrue (b :? I2)
            isFalse (b :? I)
            equal ((a :?> I).Get()) 1
            equal ((b :?> I2).Get()) 2
        }
        
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

        Test "Generics in interface implementation" {
            let parseState = ParseState() 
            raises ((parseState :> IParseState).RaiseError())
            isTrue (parseState?RaiseError !==. JS.Undefined)
        }

        Test "Local generics in interface implementation" {
            let parseState =                                                                                            
                { new IParseState with 
                    member _.RaiseError()  = raise <| System.Exception("asd")
                }
            raises (parseState.RaiseError())
            isTrue (parseState?RaiseError !==. JS.Undefined)

            let parseStateAbstract =                                                                                            
                { new ParseStateAbstract() with 
                    override _.RaiseError()  = raise <| System.Exception("asd")
                }
            raises (parseStateAbstract.RaiseError())
            isTrue (parseStateAbstract?RaiseError !==. JS.Undefined)

            let parseState2 =                                                                                            
                { new IParseState2 with 
                    member _.RaiseError()  = raise <| System.Exception("asd")
                }
            raises (parseState2.RaiseError())
            isTrue (parseState2?RaiseError !==. JS.Undefined)
        }

        Skip "Default interface member consumption" {
            // let a = MyType() :> WebSharper.CSharp.Tests.IFoo
            // let b = { new WebSharper.CSharp.Tests.IFoo }

            // equal a.Bar 0
            // equal b.Bar 0
            equal 1 1
        }

        Skip "Interfaces with multiple generic implementations" {
        //    let mc = MultipleIntf()
        //    let iaInt = mc :> IA<int>
        //    let iaString = mc :> IA<string>
        //    equal (iaInt.Get()) 1
        //    equal (iaString.Get()) "hello"
            equal 1 1
        }

        Test "Struct copy handling #1399" {
            let orig =     
                {
                    pos_fname = "File"
                    pos_lnum = 10
                    pos_orig_lnum = 10
                }
            equal orig.NextLine.Line 11
            equal orig.Line 10
            equal orig.NextLine.pos_fname "File"
        }
    }
