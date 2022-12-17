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

module WebSharper.Tests.Regression

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<Inline "new Array(0)">]
let private Empty<'T> : 'T [] = X

[<JavaScript>]
let private bug431_f ((x, y) as t) =
    [| y; x |]

[<JavaScript>]
let private bug431_g () =
    let t = (1, 2)
    bug431_f t

[<JavaScript>]
let private bug35_Foo (t: 'T * 'T) x = x

[<JavaScript>]
let private bug35_Bar () = bug35_Foo ("a","b") "c"

type FuncHelper =
    static member inline Compose f g x = g(f(x))

[<Proxy(typeof<FuncHelper>)>]
[<JavaScript>]
type internal FuncHelperProxy =
    [<Inline>]
    static member Compose f g x = g(f(x))

module private Bug61_M =

    type T[<JavaScript>]() =
        [<JavaScript>]
        member this.F = 1

type private Bug61_T2 [<JavaScript>]() =
    inherit Bug61_M.T()

module internal Bug367 =

    [<JavaScript>]
    let x = 1

    module B =
        [<JavaScript>]
        let y = x

    [<JavaScript>]
    let z = B.y

module BugBB80 =

    [<Sealed>]
    type X [<JavaScript>] (k: ref<int>) =
        interface System.IDisposable with
            [<JavaScript>]
            member this.Dispose() = incr k

    [<JavaScript>]
    let test () =
        async {
            let a = ref 0
            do! async {
                    use x = new X(a)
                    return ()
                }
            return Test "Bug BB80" { equal !a 1 }
        }
        |> Async.Start

module Bug264 =
    [<JavaScript>]
    type Y<'X> = | Y of int

    [<JavaScript>]
    type X =
        {
            A : int
            mutable B : Y<X>
        }

    [<JavaScript>]
    let Make f : Y<_> = Y (f().A)

module Bug323 =
    [<JavaScript>]
    type BaseClass(x) =
        member this.Value = x

    [<JavaScript>]    
    type DescendantClass(x) =
        inherit BaseClass(x + 1)

        member this.OriginalValue = x
                 
[<JavaScript>]
module Bug352 =
    [<AbstractClass>]
    type A() =
        abstract Foo : unit -> int
        default this.Foo() = 1

    type B() =
        inherit A()
        override this.Foo() = 2    

module ExplicitConstructor =
    [<JavaScript>]    
    type BaseClass =
        val x : int
        new v = { x = v }
        member this.Value = this.x

    [<JavaScript>]    
    type DescendantClass =
        inherit BaseClass

        val x : int
        new v = { inherit BaseClass(v + 1); x = v }
        member this.OriginalValue = this.x

[<JavaScript>]
module Bug552 =

    type C<'T>(f: 'T -> obj) =
        [<Inline>]
        new() = C(Json.Encode)
        [<Inline>]
        static member Create() = C(Json.Encode)

[<JavaScript>]
module Bug553 =

    type C(x: int) as this =
        let mutable x = x 
        
        do this.AddDigit(0)

        new() = C(1)

        new (a, b) as this = C(a) then this.AddDigit(b)

        member this.X = x
        member this.AddDigit y =
            x <- 10 * x + y

[<JavaScript>]
module Bug512 =
    [<NamedUnionCases>]
    type TestType =
        | Float of Float:float
        | Prod of Prod1:TestType * TestType
        | Sum of Sum1:TestType * TestType

// for bug #328
type System.Object with
    [<Inline "$0.test">]
    member this.TestProperty = X<int>

    [<Inline "$0.test">]
    member this.TestMethod() = X<int>

    [<Inline "$0.testf($x)">]
    member this.TestMethod1 (x: int) = X<int[]>

    [<Inline "$0.testf($x, $y)">]
    member this.TestMethod2 (x: int, y: int) = X<int[]>

[<JavaScript>]
module LetLambda =
    let f =
        ()
        fun a b -> a + b

[<JavaScript>]
type IBar = 
    abstract member DoBar: unit -> unit

[<JavaScript>]
type IFoo<'T> =
    inherit System.Collections.Generic.IEnumerable<'T>
    inherit System.Collections.IEnumerable
    inherit IBar
    abstract member DoBar: unit -> unit

[<JavaScript>]
[<Inline>]
let inlinedIf a b c = if a() then b() else c()

module SelfAlias =
    [<JavaScript>]    
    type BaseClass () as self =
        let mutable value = 0
        do self.Value <- self.Value * 10 + 1
        
        member this.Value 
            with get () = value 
            and set v = value <- v

    [<JavaScript>]    
    type DescendantClass() as self =
        inherit BaseClass()

        do self.Value <- self.Value * 10 + 2

[<Pure; Direct "void($x[0]++)">]
let fakePureFunc (x: int ref) = () 

[<Pure; Inline "WebSharper.Tests.Regression.fakePureFunc($x)">]
let fakePureFuncInline (x: int ref) = () 

[<Direct "void($x[0]++)">]
let nonPureFunc (x: int ref) = ()  

//[<JavaScript>]
//let rec moduleFuncValue =
//    ()
//    fun x -> if x < 5 then moduleFuncValue (x + 1) else x

[<JavaScript>]
type Bug590 =
    static member Curried5 (a: string) b c d e = a + b + c + d + e

[<JavaScript>]
module Bug625 =
    let mutable w = "Wrong"
    
    [<Inline>]
    let ignore x = ()
    
    let f () = 
        w <- "Correct"
        1
    
    let g() = ignore (f ())        

open Bug625

[<JavaScript>]
type CurriedInst<'T>(x : 'T) =    
    member this.X = x

type [<JavaScript>] CurriedInst =
    static member Test<'T> (x: CurriedInst<'T>) f g h = h (f x.X : int) (g x.X : int)

type CurriedInst<'T> with  
    // this is optimized wrong, the this parameter is not enclosed properly
    member this.Test a b c = CurriedInst.Test this a b c :int

[<JavaScript>]
let TreeReduce (defaultValue: 'A) (reduction: 'A -> 'A -> 'A) (array: 'A[]) : 'A =
    let l = array.Length // this should not be inlined, as it is also captured
    let rec loop off len =
        match len with
        | n when n <= 0 -> defaultValue
        | 1 when off >= 0 && off < l ->
            array.[off]
        | n ->
            let l2 = len / 2
            let a = loop off l2
            let b = loop (off + l2) (len - l2)
            reduction a b
    loop 0 l

module Bug695 =
    type testtype = { mutable r: int }

type TypeCheckTestWithSingletonCase =
    | NotSingleton of string
    | Singleton

[<Inline "ThisDoesNotExists.DoNotImport.doSomething()">]
let tryDoSomethingButFail() = X<unit>

[<Inline "OutSideCode.NotInitialized.getValue()">]
let tryGetOutsideValueAndFail() = X<int>

[<Inline "$global.OutSideCode.NotInitialized.getValue()">]
let tryGetOutsideValue() = X<int>

type TestConfigObj [<JavaScript>]() =

    [<Name "x">]
    [<Stub>]
    member val X = Unchecked.defaultof<bool> with get, set

    [<DefaultValue>]
    val mutable private value : int

    [<JavaScript>]
    member this.Value
        with set x =
            this.value <- x

[<JavaScript>]
module Bug751 =
    type ExampleTree = 
      | TLeaf of int
      | TNode of (ExampleTree * ExampleTree)
      | TLink of ExampleTree

    let rec collect (t : ExampleTree) : int Set =
      match t with
      | TLeaf(i) -> Set.singleton i
      | TNode(l,r) -> Set.union <| collect l <| collect r
      | TLink(i) -> collect i

[<JavaScript>]
type UnusedStaticLetTest() =
    static let Hello = "hello"
    member val Hi = "hi"

[<JavaScript>]
type StaticLetFunctionTest() =
    static let DefaultMsg() = "mooo"
    member __.SayWhat() = DefaultMsg()

[<JavaScript>]
type MultipleStaticLetTest() =
    static let HIGH = "moooh"
    static let LOW = "meh"
    static let DefaultMsg() = "mooo"+HIGH+LOW
    member __.SayWhat() = DefaultMsg()

[<JavaScript>]
module TupledArgOpt =
    let t () = 1, 1
    
    let g (f: int * int -> int) =
        let x = t()
        // bug #756, this was throwing a compile-time error
        f x

    let g2 (_: obj) (f: int * int -> int) =
        f (1, 1)

    type O() =
        [<Inline>]
        member this.G f = g2 this f
       
[<JavaScript>]
module Bug906 =
    // bug #906, this was throwing a compile-time error 'not a named type'
    type Alias<'T> = 'T
    type Record<'T> = { x : Alias<'T> }

[<Struct>]
type Normal<'T, 'U> = Ok of 'T | Error

[<JavaScript>]
module Bug914 =
    type Alias<'T> = Normal<'T, string>
    let f i =
        match Ok i : Alias<int> with
        | Ok x -> string x
        | Error -> "error"

module Bug923 =
    type V2<[<Measure>] 'u> =
        struct
            val x : float<'u>
            val y : float<'u>
            new (x, y) = {x=x; y=y}
        end

        static member (+) (a : V2<_>, b : V2<_>) = 
            V2 (a.x + b.x, a.y + b.y)

    [<JavaScript>]
    let addFloatsWithMeasures (a: float<'a>) (b: float<'a>) = a + b

[<JavaScript>]
module Bug944 =
    type IFoo = 
        abstract member Fooey : string -> unit

    let AsFoo m = 
        match box m with
        | :? IFoo as f -> Some f
        | _ -> None

    let CheckNonPure m =
        let r = ref 0
        if (incr r; box m) :? IFoo then Some !r else None

[<JavaScript>]
module Bug948 =
    type MyType =
        | [<Constant(null)>] NullCase
        | NonNullCase
    
    let f (x: Union<Dom.Node, MyType>) =
        match x with
        | Union1Of2 _ -> "it's not my type"
        | Union2Of2 _ -> "it's my type"
    
    let test() =
        f (Union2Of2 NullCase)

[<JavaScript>]
module Bug951 =
    // this was previously failing to compile
    type Foo() =
        static let hey = 43
        abstract member clone: unit -> Foo
        default __.clone() = new Foo()

    // this was previously not renaming "Value" abstract name to not collide
    // with fixed name in subtype
    [<AbstractClass>]
    type A() =
        abstract Value: int

    type B() =
        inherit A()
        [<Name "get_Value">]
        member this.ValueB = 2
        override this.Value = 1 

    // this should cause a compile-time error:
    //[<AbstractClass>]
    //type A() =
    //    [<Name "Value">]
    //    abstract Value: int

    //type B() =
    //    inherit A()
    //    [<Name "Value">]
    //    member this.ValueB = 2
    //    override this.Value = 1 

[<JavaScript>]
module Bug991 =
    type Foo = int

    type IFooey = 
        abstract member ToFoo: string -> Foo
        abstract member FromFoo: Foo -> string

    type FooBase() =
        interface IFooey with
            override __.ToFoo s = 42 
            override __.FromFoo x = "meh"

    type Footacular() =
        inherit FooBase()

        interface IFooey with
            override __.ToFoo s = 42
            override __.FromFoo x = "wee"

/// Regression would make this fail at compile time
[<JavaScript>]
type Bug1010() =
    inherit WebSharper.InterfaceGenerator.Tests.Regression1010.B()

/// Regression would make this fail at compile time
[<JavaScript>]
module Bug1051 =
    type Base<'T> (x: 'T) = 
        class end

    type Sub() as this =
        inherit Base<unit>(())
    
        member __.A() = ()
        member __.B() = this.A()

/// Regression would make this fail at compile time
[<JavaScript>]
module Bug1074 =
    type  Val<'P> = VView of ref<'P> | VConst of 'P
        with
        [<Inline>] static member ( <* )(vf:Val<'a->'b> , a :    'a ) = VConst a
        [<Inline>] static member ( <* )(vf:Val<'a->'b> , aV:ref<'a>) = VView  aV

    let add1 a = a + 1
    let a11V = ref 11

    let mainX = VConst add1 <* a11V

/// Regression would make this fail at compile time
[<JavaScript>]
module Bug1091 =
    let foo = [| (try (0).ToString() with e -> e.ToString()) |]

[<JavaScript>]
type Bug1126 [<Inline "{}">] () =
    member x.X
        with [<Inline "$x.X">] get() = X<int>
        and [<Inline "void($x.X = $v)">] set (v: int) = X<unit>
    member x.Y
        with [<Inline "$x.Y">] get() = X<int>
        and [<Inline "void($x.Y = $v)">] set (v: int) = X<unit>
    member x.Z
        with [<Inline "$x.Z">] get() = X<int>
        and [<Inline "void($x.Z = $v)">] set (v: int) = X<unit>

module Bug1283 = 
    [<Inline>]
    let inline Sum vs =
        (LanguagePrimitives.GenericZero, vs)
        ||> Seq.fold(fun x y -> x + y 
        )

[<JavaScript>]
module Bug1284 = 
    type Record = {
        Field: float
    } with 
        static member Zero = {Field = 0.}
        static member (+) (a,b) = {Field = a.Field + b.Field}
        static member DivideByInt (a, b: int) = {Field = a.Field / float b}

    let addOne { Field = x } = { Field = x + 1. }

    let records = [ {Field = 1.}; {Field = 2.} ]

[<JavaScript>]
let Tests =
    TestCategory "Regression" {

        Test "Bug #26" {
            isTrueMsg ([||] = Empty<int>) "[||] = Empty<int>"
        }

        Test "Bug #35" {
            equal (bug35_Bar ()) "c"
        }

        Test "Bug #61" {
            let x = Bug61_T2()
            equal x.F 1
        }

        Test "Bug #367" {
            equal Bug367.B.y 1
            equal Bug367.z 1
        }

        Test "Bug #476" {
            let q = new System.Collections.Generic.Queue<int>()
            seq {
                q.Enqueue -1
                let c = ref 0
                while !c < 2 do
                    q.Enqueue !c
                    incr c
                q.Enqueue 2
                while !c < 4 do
                    q.Enqueue !c
                    incr c
                q.Enqueue 4
            }
            |> Seq.length |> ignore
            equal (q.ToArray()) [|-1; 0; 1; 2; 2; 3; 4|]
            let t (x: list<int>) = Seq.toArray (Seq.windowed 3 x)
            equal (t []) [||]
            equal (t [1]) [||]
            equal (t [1;2]) [||]
            equal (t [1;2;3]) [|[|1;2;3|]|]
            equal (t [1;2;3;4]) [|[|1;2;3|]; [|2;3;4|]|]
            equal (t [1;2;3;4;5]) [|[|1;2;3|]; [|2;3;4|]; [|3;4;5|]|]
        }

        Test "Bug #431" {
            equal (bug431_g()) [| 2; 1 |]
        }

        Test "Bug #484" {
            equal (string 0) "0"
        }

        do BugBB80.test()

        Test "Mutable" {
            equal (
                let mutable a = 2
                a <- 4
                a
            ) 4
        }

        Test "Capturing range variable value" {
            let n = ResizeArray()
            do
                for i in 1 .. 10 do
                    for j in 1 .. 10 do
                        n.Add(fun k -> k + i + j)
            equal (Seq.sum [for x in n -> x 5]) 1600
        }

        Test "Capturing enumeration variable value" {
            let n = ResizeArray()
            do
                for i in [ 1 .. 10 ] do
                    for j in [ 1 .. 10 ] do
                        n.Add(fun k -> k + i + j)
            equal (Seq.sum [for x in n -> x 5]) 1600
        }

        Test "Capturing value inside loop" {
            let arr = [| 1 .. 10 |]
            let funcs = ResizeArray()
            let res = ResizeArray()
            do
                for i in [| 0 .. 9 |] do
                    let j = arr.[i]
                    funcs.Add(fun () -> res.Add j)
                for f in funcs do
                    f()
            equal (res.ToArray()) arr
        }

        Test "Curried apply optimizations" {
            let f = (fun a b -> a + b) 1
            let g = (fun a b c -> a + b + c) 1 2
            let h = (fun a b c d -> a + b + c + d) 1 2 3
            let addL = [ fun x y -> x + y ]
            let o = (fun y -> addL.Head y)
            equal (f 10) 11
            equal (g 10) 13        
            equal (h 10) 16    
            equal (o 3 5) 8    
        }

        Test "Bug #231" {
            let arr =
                [|
                    let prev = ref None
                    for c in [| 2; 2; 4; 2 |] do
                        match !prev with
                        | Some p ->
                            if p = c then
                                yield c * 2
                                prev := None
                            else
                                yield p
                                prev := Some c
                        | _ ->
                            prev := Some c
                    match !prev with
                    | Some p -> yield p
                    | _ -> ()
                |]
            equal arr [| 4; 4; 2 |]
        }

        Test "Bug #249" {
            equal (double 1) 1.0
        }

        Test "Bug #264" {
            let Mk () =
                let v =
                    {
                        A = 0
                        B = Unchecked.defaultof<_>
                    } : Bug264.X
                v.B <- Bug264.Make (fun () -> { v with A = 2 })
                v
        
            isTrue (
                try Mk().B = Bug264.Y 2
                with _ -> false)
        }

        Test "Bug #323" {
            let a = Bug323.DescendantClass(3)
            equal (a.OriginalValue) 3
            equal (a.Value) 4    
        }

        Test "Bug #328" {
            let o = 
                New [
                    "test" => 3
                    "testf" => FuncWithRest<int, int[]> id
                ]
            equal o.TestProperty 3
            equal (o.TestMethod()) 3
            equal (o.TestMethod1(1)) [| 1 |]
            equal (o.TestMethod2(1, 2)) [| 1; 2 |]
        }

        Test "Curried inlining" {
            let add1 x = x + 1
            let twice x = x * 2
            equal (FuncHelper.Compose add1 twice 0) 2
            let f = FuncHelper.Compose add1 twice 
            equal (f 1) 4
            equal (f 2) 6
        }

        Test "Bug #352" {
            equal (Bug352.B().Foo()) 2    
        }

        Test "Bug #396" {
            let disposed = ref false
            let mySeq =
                seq {
                    try yield 0
                    finally disposed := true
                }
            do
                try for x in mySeq do
                        failwith ""
                with _ -> ()
            isTrue !disposed 
        }

        Test "Bug #480" {
            equal (1445122700705L / 32L) 45160084397L
        }

        Test "Bug #477 (mutable in closure)" {
            let f, g =
                let mutable x = 0
                (fun () -> x <- 1), (fun () -> x)
            equalMsg (g()) 0 "Before modifying (sanity check)"
            do f()
            equalMsg (g()) 1 "After modifying"
        }

        Test "Bug #479" {
            let test () =
                try
                    let add (v:int) m = v + m
                    let run func (c:int) (p:int) : int =
                        func p c
                    let x = add 2 3
                    let y = run add 2 3
                    x, y
                with e ->
                    Console.Log("#479", e)
                    0, 0
            equal (test()) (5, 5)
        }

        Test "Bug #552" {
            let a = Bug552.C<int>()
            let b = Bug552.C<int>.Create<string>()
            expect 0
        }

        Test "Bug #553" {
            let a = Bug553.C()
            equal a.X 10
            let b = Bug553.C(2, 3)
            equal b.X 203
        }

        Test "Match with as/when" {
            let get y = 
                match y with
                | Some x as asWhenTest when x <> 1 -> asWhenTest
                | _ -> None
            equal (get (Some 1)) None
            equal (get (Some 2)) (Some 2)
        }

        Test "inheritance with self identifier" {
            equal (SelfAlias.DescendantClass().Value) 12
        }

        Test "Module let function value" {
            equal ([ 1, 2; 3, 4 ] |> List.map (fun (a, b) -> LetLambda.f a b)) [ 3; 7 ]
        }

        Test "Bug #512" {
            let v = Bug512.Prod(Bug512.Float(1.0),Bug512.Float(1.0));
            let str = WebSharper.Json.Serialize v
            let data2 = WebSharper.Json.Deserialize<Bug512.TestType> str
            let str2 = WebSharper.Json.Serialize data2
            equal v data2
            equal str str2
        }

        Test "Pipe optimization" {
            let f x y = x + y
            let res1 =
                let x = 2
                (f x) x
            let res2 =
                2 |> (f 2) 
            let res3 =
                2 |> (fun x -> f x x)
            equal res1 4
            equal res2 4
            equal res3 4
        }

        Test "Inlining function arguments" {
            equal (inlinedIf (fun () -> true) (fun () -> 1) (fun () -> 2)) 1 
        }

        Test "Pure attribute" {
            let r = ref 0
            fakePureFunc r
            equal !r 0
            nonPureFunc r
            equal !r 1
            fakePureFuncInline r
            equal !r 1
        }

        Test "Long curried function" {
            let f a b c d e = a + b + c + d + e
            equal (f 1 1 1 1 1) 5
            equal (f 1 1 1 1 2) 6
            let g a b c d e = a + b + c + d + e
            equal ("a" |> g "a" "a" "a" "a") "aaaaa"
            equal ("b" |> g "b" "b" "b" "b") "bbbbb"
            equal ("a" |> Bug590.Curried5 "a" "a" "a" "a") "aaaaa"
            equal ("b" |> Bug590.Curried5 "b" "b" "b" "b") "bbbbb"
            let h = Bug590.Curried5
            equal (h "a" "a" "a" "a" "a") "aaaaa"
            equal (h "b" "b" "b" "b" "b") "bbbbb"
            let i = Bug590.Curried5 "x"
            equal ("a" |> i "a" "a" "a") "xaaaa"
            equal ("b" |> i "b" "b" "b") "xbbbb"
            let j = Bug590.Curried5 "x" "y"
            equal ("a" |> j "a" "a") "xyaaa"
            equal ("b" |> j "b" "b") "xybbb"
        }

        Test "Bug #625: Inlined ignore" {
            Bug625.g()
            equal Bug625.w "Correct"
        }

        Test "Bug #671" {
            let x = CurriedInst(10)
            let r = x.Test <| (fun a -> a + 1) <| (fun b -> b - 1) <| (fun a b -> a * b)
            equal r 99
        }

        Test "Bug #680 do not eta-reduce" {
            let openThePit howmany =
                failwithf "Attacked by %i flying monkeys" howmany
            let e saveMeDelayedExecution = saveMeDelayedExecution |> (openThePit 1000000)
            equal (sprintf "%A" e) "<fun>"       
        }

        Test "Bug #695 ItemGet should not be strongly pure" {
            let test_ws_bug (t: Bug695.testtype) =
                let old = t.r
                t.r <- 1
                old     
            let res = test_ws_bug { Bug695.r = 2 }   
            equal res 2
        }

        Test "Type check for union with singleton case" {
            let u1 : Union<string, TypeCheckTestWithSingletonCase> = Union1Of2 "hi"  
            isTrue (match u1 with Union1Of2 "hi" -> true | _ -> false)
            let u2 : Union<string, TypeCheckTestWithSingletonCase> = Union2Of2 (NotSingleton "hi")
            isTrue (match u2 with Union2Of2 (NotSingleton "hi") -> true | _ -> false)
            let u3 : Union<string, TypeCheckTestWithSingletonCase> = Union2Of2 Singleton
            isTrue (match u3 with Union2Of2 Singleton -> true | _ -> false)
        }

        Test "Do not import missing outside namespaces if not needed" {
            // this should fail here, and not globally
            raises (tryDoSomethingButFail()) 
        }

        Test "Do not import missing outside namespaces prematurely" {
            JS.Global?OutSideCode <- New [ 
                "NotInitialized" => New [ "getValue" => (fun() -> 1) ]
            ]
            equal (tryGetOutsideValue())  1     
            raises (tryGetOutsideValueAndFail())
        }

        Test "#731 Tail recursion should not overwrite outside variable" {
            let findInMs = ResizeArray()
            
            let test_websharper_bug (ns:string list) (ms:string list) : int list =
              let find (d: string) ns =
                let rec loop acc msl msr =
                  match msr with
                  | [] -> acc
                  | m::msr' -> 
                     findInMs.Add m
                     loop acc (m::msl) msr'
                loop [] [] ms in
              let rec loop acc record nsl nsr =
                let expand_finds fs = [] in
                let search_record = function
                  | [d] -> let foo = expand_finds (find d [d]) in []
                  | _ -> [] in
                match nsr with
                | [] -> 
                    search_record (List.rev record) @ acc
                | n::nsr ->
                    let acc = search_record record @ acc in
                    loop acc record (nsl@record@[n]) nsr
              in
              loop [] ["x"] [] ns

            test_websharper_bug ["x"] ["x"] |> ignore

            equal (findInMs.ToArray()) [| "x"; "x" |]
        }

        Test "#737 Local mutual tail recursive optimization switch case falling over" {
            let a() =
                let rec f x =
                    if x = 0 then g x 1 else f (x - 1)
                and g x y =
                    x + y
                f 5    
            equal (a()) 1
            let b() =
                let rec f x y z =
                    if x = 0 then 
                        if y = 0 then g z else y + z
                    else f (x - 1) y z
                and g x = 
                    f 0 1 x
                f 5 0 5
            equal (b()) 6
            let c() =
                let r = ref 0
                let rec f x y z =
                    if x = 0 then 
                        if y = 0 then g z else r := y + z
                    else f (x - 1) y z
                and g x = 
                    f 0 1 x
                f 5 0 5
                !r
            equal (c()) 6
            let rec f acc n =
                if n = 0 then
                    acc
                else
                    f ((fun x -> x + n) :: acc) (n - 1)
            equal (f [] 2 |> List.map (fun g -> g 1)) [2; 3]
            let rec h x y =
                if x = 0 then y else h (x - 1) (y + x)
            equal (h 4 4) 14
        }

        Test "#747 Property set" {
            let x = TestConfigObj()
            x.Value <- 4
            equal x?value 4
            equal x.X (As null)
            x.X <- true
            isTrue x.X
        }

        Test "#751 tail call position recognition error" {
            let a =
                Bug751.TNode (Bug751.TLeaf 1, Bug751.TLink (Bug751.TLeaf 2))
            equal (Bug751.collect a |> Array.ofSeq) [| 1; 2 |]
        }
        
        Test "#753 Unused static let" {
            let x = UnusedStaticLetTest()
            equal x.Hi "hi"
        }

        Test "Static lets" {
            equal (StaticLetFunctionTest().SayWhat()) "mooo"
            equal (MultipleStaticLetTest().SayWhat()) "mooomooohmeh"       
        }

        Test "#859 Optimizer incorrectly inlining through variable set" {
            let test =
                let arr = [| "correct"; "wrong" |]
                let mutable index = 0
                let res = arr.[index]
                index <- index + 1
                res
            equal test "correct"
        }

        Test "#866 Local function incorrectly treated as tupled" {
            let r1, r2 =
                let f (s: string) = string s.[0] + string s.[2]
                f "abc", f "123"
            equal r1 "ac"
            equal r2 "13"
        }

        Test "#914 generic struct type alias" {
            equal (Bug914.f 42) "42"
        }

        Test "#944 Interface type test on non-object value" {
            equal (Bug944.AsFoo 42) None
            let x = { new Bug944.IFoo with member __.Fooey _ = () }
            equal (Bug944.CheckNonPure x) (Some 1)
        }

        Test "#948 Erased union pattern matching fails on a union with a [<Constant null>] case" {
            equal (Bug948.test()) "it's my type"
        }

        Test "#951 abstract method key collisions and renaming fixes" {
            equal (Bug951.B().Value) 1
            equal (Bug951.B().ValueB) 2
            equal (Bug951.B()?get_Value()) 2
        }

        Test "#991 Overriding interface implementations" {
            equal ((Bug991.FooBase() :> Bug991.IFooey).FromFoo(0)) "meh"
            equal ((Bug991.Footacular() :> Bug991.IFooey).FromFoo(0)) "wee"
        }

        //Test "Recursive module value" {
        //    equal (moduleFuncValue 0) 5
        //}

        Test "#1010 WIG inheritance" {
            equal (Bug1010().M()) 42
        }

        Test "#1074 Ambiguity at translating trait call" {
            equal Bug1074.mainX (Bug1074.VView (ref 11))
        }

        Test "#1098 Tuple argument match with trailing wildcard and 'as'" {
            let mappedArr =
                [| (1, 2); (3, 4) |]
                |> Array.map (fun ((p, _) as o) ->
                    p, o
                )
            equal mappedArr [|As [|1; As [|1; 2|]|]; As [|3; As [|3; 4|]|]|]  
        }

        Test "#1126 Optimizer dropping expression while creating object expression" {
            let createObj b =
                let o = Bug1126()
                o.X <- 1
                if b then
                    o.Y <- 2
                o.Z <- 3
                o
            deepEqual (createObj true) (JS.Inline "{X: 1, Y: 2, Z: 3}")
        }

        Test "#1283 better statically resolved handling of operators and GetZero/One" {
            equal (Bug1283.Sum [1; 2; 3]) 6
        }

        Test "#1284 Generic support for Seq.sum/average" {
            equal (Seq.sum Bug1284.records).Field 3.
            equal (Seq.sumBy Bug1284.addOne Bug1284.records).Field 5.
            equal (Seq.average Bug1284.records).Field 1.5
            equal (Seq.averageBy Bug1284.addOne Bug1284.records).Field 2.5
        }
    }
