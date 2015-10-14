// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Tests.Tupled

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
type A() =
    member this.X(a, b) = a + b
    member this.Y((a, b)) = a + b
    member this.Z a b = a + b
    member this.C a b =
        let ab = a + b
        fun c -> ab + c 
    member this.CT a (b, c) =
        a + b + c

[<JavaScript>]
type B(a, b) =
    let c = a + b
    member this.Value = c

[<JavaScript>]
let DoNotInline x =
    ignore x    

[<JavaScript>]
let logged = ref <| obj()

[<JavaScript>]
let logArgC f x =
    logged := box x
    f x    

[<JavaScript>]
let logArgL f =
    ()
    fun x ->
        logged := box x
        f x    

[<JavaScript>]
[<Inline>]
let logArgCI f x =
    logged := box x
    f x    

[<JavaScript>]
[<Inline>]
let logArgLI f =
    ()
    fun x ->
        logged := box x
        f x    

[<Inline "$arr.map(function (v, i, a) { return $mapping([v, i, a]); })">]
let mapArray (mapping: 'T * int * 'T[] -> 'U) (arr: 'T) = X<'U[]>

[<Inline "function(t){ return someJSFunc(t[0], t[1]); }" >]
let getSomeJSFunc() = X<int * int -> int>

[<JavaScript>]
[<Inline>]
let addPair1 (a, b) = a + b

[<JavaScript>]
[<Inline>]
let addPair2 ((a, b)) = a + b

[<JavaScript>]
[<Inline>]
let addTriple1 (a, b) c = a + b + c

[<JavaScript>]
[<Inline>]
let addTriple2 ((a, b)) c = a + b + c

[<Inline "$f(1, 2)">]
let callWithArgs (f: FuncWithArgs<int * int, int>) = X<int>

[<Inline "$f([1, 2])">]
let callWithTuple f = X<int>

[<Inline "function (a, b){ return a + b; }">]
let normalAdd = Unchecked.defaultof<int * int -> int>

[<Inline "function(){return function (a){ return a[0] + a[1]; }}">]
let getTupledAdd = Unchecked.defaultof<unit -> int * int -> int>

[<Inline "$f($x)">]
let apply (f: 'T -> 'U) (x: 'T) = X<'U>

[<JavaScript>]
let Tests =
    TestCategory "Tupled functions" {

        Test "Methods" {
            let a = A()
            equal (a.X(1, 2)) 3
            let d = System.Func<_,_,_,_>(fun (_: obj) x y -> a.X(x, y)) 
            equal (callWithArgs (FuncWithArgs(a.X))) 3
            equal (a.Y(1, 2)) 3
            equal (callWithArgs (FuncWithArgs(a.Y))) 3
            equal (a.Z 1 2) 3
            let fx = a.X
            equal (fx (1, 2)) 3
            equal (callWithArgs (FuncWithArgs(fx))) 3
            let fy = a.Y
            equal (fy (1, 2)) 3
            equal (callWithTuple fy) 3
            let ct = a.CT
            equal (ct 1 (2, 3)) 6
        }

        Test "Methods with tuple input" {
            let a = A()
            let t = 1, 2
            equal (a.X t) 3
            equal (a.Y t) 3
            equal (callWithArgs (FuncWithArgs(a.X))) 3
            equal (callWithTuple a.Y) 3
        }

        Test "Constructor" {
            equal (B(1, 2).Value) 3
            let t = 1, 2
            equal (B(t).Value) 3
            equal (B(As t).Value) 3
        }

        Test "Functions" {
            let f (x, y) = x + y 
            equal (f(1, 2)) 3
            let t = 1, 2
            equal (f t) 3
            let g((x,y)) = x + y
            equal (g(1, 2)) 3
            equal (g t) 3
            let h =
                ()
                fun (x, y) -> x + y
            equal (h(1, 2)) 3
            equal (h t) 3 
        }

        Test "Corrector" {
            let a = A() 
            equal (a.C 1 2 3) 6
            let p = a.C 1 2
            equal (p 4) 7
            equal (p 5) 8
        }

        Test "Generic" {
            let add(x, y) = x + y
            equal (callWithArgs (FuncWithArgs(logArgC add))) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithArgs (FuncWithArgs(logArgL add))) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithArgs (FuncWithArgs(logArgCI add))) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithArgs (FuncWithArgs(logArgLI add))) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithTuple (logArgC add)) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithTuple (logArgL add)) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithTuple (logArgCI add)) 3
            equal (!logged) (box [| 1; 2 |])
            equal (callWithTuple (logArgLI add)) 3
            equal (!logged) (box [| 1; 2 |])
        }

        Test "Inlines" {
            equal (apply (getTupledAdd()) (1, 2)) 3
            equal (addPair1 (1, 2)) 3
            equal (addPair2 (1, 2)) 3
            equal (addTriple1 (1, 2) 3) 6
            equal (addTriple2 (1, 2) 3) 6
        }

    }
