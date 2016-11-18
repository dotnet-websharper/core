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
type FuncHelperProxy =
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

#if FSHARP40
        Test "Bug #477 (mutable in closure)" {
            let f, g =
                let mutable x = 0
                (fun () -> x <- 1), (fun () -> x)
            equalMsg (g()) 0 "Before modifying (sanity check)"
            do f()
            equalMsg (g()) 1 "After modifying"
        }
#endif

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
            let h = Bug590.Curried5
            equal (h "a" "a" "a" "a" "a") "aaaaa"
            equal (h "b" "b" "b" "b" "b") "bbbbb"
            let i = Bug590.Curried5 "x"
            equal ("a" |> i "a" "a" "a") "xaaaa"
            equal ("b" |> i "b" "b" "b") "xbbbb"
        }

        Test "Bug #625: Inlined ignore" {
            Bug625.g()
            equal Bug625.w "Correct"
        }

//        Test "Recursive module value" {
//            equal (moduleFuncValue 0) 5
//        }
    }
