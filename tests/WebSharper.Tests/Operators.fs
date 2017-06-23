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

module WebSharper.Tests.Operators

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

type CustomNumber(x: int) =
    let inner = x

    member this.Inner = inner
    static member op_Multiply(CN1: CustomNumber, CN2: CustomNumber) =
        CN1.Inner * CN2.Inner


[<Proxy(typeof<CustomNumber>)>]
type CN =
    
    [<Inline "$x">]
    new (x: int) = {}

    [<Inline "$this.x">]
    member this.Inner = X<int>

    [<Inline "55">]
    static member op_Multiply(CN1: CustomNumber, CN2: CustomNumber) = X<int>


[<JavaScript>]
type IValue<'T> =
    abstract member Value : 'T with get
    
[<JavaScript; Inline>]
let inline ( !! ) (o: ^x) : ^a = (^x: (member Value: ^a with get) o)

[<JavaScript>] 
type IntWithAdd(x) =
    member this.Value = x

    static member Add (x: IntWithAdd, y) = x.Value + y
    static member AddB (x, y: IntWithAdd) = x + y.Value

[<JavaScript; Inline>]
let inline ( ++ ) (a: ^x) (b: ^y) : ^a = ((^x or ^y): (static member Add: ^x * ^y -> ^a) (a, b))

[<JavaScript; Inline>]
let inline ( ++! ) (a: ^x) (b: ^y) : ^a = ((^x or ^y): (static member AddB: ^x * ^y -> ^a) (a, b))

[<JavaScript>]
type Singletons =
    | Case0
    | Case1
    | Case2

(* TODO: the coverage of the Operators module is far from complete. *)
[<JavaScript>]
let Tests =

    TestCategory "Operators" {

        Test "=" {
            equal null null
            notEqual null (obj ())
            notEqual (obj ()) null
        }

        Test "+" {
            equal (1 + 2) 3
        }

        Test "max" {
            equal (max 1 3) 3
            equal (max "asdf" "asdg") "asdg"
            equal (max (1, 2) (2, 1)) (2, 1)
        }

        Test "min" {
            equal (min 1 3) 1
            equal (min "asdf" "asdg") "asdf"
            equal (min (1, 2) (2, 1)) (1, 2)
            property (fun (x: int, y: int) -> Do {
                isTrue (max x y >= min x y)
            })
        }

        let add1 = ((+) 1)
        let twice = (( * ) 2)
        let add (x, y) = x + y

        Test ">>" {
            equal ((List.length >> add1 >> twice) [1; 2]) 6
            equal ((id >> add) (1, 2)) 3
        }

        Test "<<" {
            equal ((twice << add1 << List.length) [1; 2]) 6
            equal ((add << id) (1, 2)) 3
        }

        Test "|>" {
            equal ((1, 2) |> add) 3
            equal (0 |> add1 |> twice |> add1) 3   
            let x = ref 0
            equal ((1 |> fun a -> x := a); !x) 1
        }

        Test "<|" {
            equal (add <| (1, 2)) 3
        }

        Test "!" {
            let r = ref 3
            equal !r 3
        }

        Test ":=" {
            let r = ref 3
            r := 4
            equal !r 4
        }

        Test "incr" {
            let r = ref 3
            incr r
            equal !r 4
        }

        Test "decr" {
            let r = ref 3
            decr r
            equal !r 2
        }

        Test "int" {
            equal (int "3") 3
            equal (int "3.5") 3
            equal (int -3.5) -3
        }

        Test "float" {
            equal (float "3.5") 3.5
        }

        Test "|>!" {
            let a =
                ref 1
                |>! incr
            equal !a 2
        }

        Test "trait call" {
            let r = ref 3
            equal !!r 3
            let i = { new IValue<int> with member this.Value = 4 }
            equal !!i 4
        }
 
        Test "trait call with multiple types" {
            let a = IntWithAdd 40
            equal (IntWithAdd.Add (a, 2)) 42
            equal (a ++ 2) 42
            equal (2 ++! a) 42
        }

        Test "taking unit" {
            let x = ref 0
            let f() = x := !x + 1
            f()
            f()
            equal !x 2 
        }

        Test "singleton union case comparisons" {
            let u = Case1
            isTrue (u = Case1) 
            isTrue (u < Case2) 
            isTrue (Case0 < u) 
            isTrue (Case0 < Case2)
        }

        Test "* op proxy" {
            let u1 = CustomNumber(5)
            let u2 = CustomNumber(6)
            isTrue (u1*u2 = 55)
        }
    }