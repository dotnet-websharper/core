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

module WebSharper.Tests.Printf

open WebSharper
open WebSharper.Testing

[<JavaScript>]
type Hi() =
    override this.ToString() = "Hello"

type A = { X : int } with override x.ToString() = "X"

type X = { A: int; B: int }

type Y = { C: int; D: Y option }

type Z = { [<Name "F">] E : int * Z option }

type MyList<'T> =
    | Empty
    | Cons of 'T * MyList<'T>

//[<Direct "var orig = console.log; console.log = function (){window.lastLogged = arguments; orig.apply(console, arguments)}">]
//let WatchConsole () = ()
//
//[<Inline "window.lastLogged">]
//let GetLastLogged() = X

[<JavaScript>]
let Tests =
    TestCategory "Printf" {

        Test "Strings" {
            equal (sprintf "Web%s" "Sharper") "WebSharper"
            equal (sprintf "Web%s%s" "Shar" "per") "WebSharper"
            equal (sprintf "%5s" "hi") "   hi"
            equal (sprintf "%-5s" "hi") "hi   "
            equal (sprintf "%*s" 5 "hi") "   hi"
            equal (sprintf "hi%s" null) "hi"
        }

        Test "Char" {
            equal (sprintf "%c" 'a') "a"
        }

        Test "Integral" {
            equal (sprintf "%d" 9) "9"
            equal (sprintf "%+d" 9) "+9"
            equal (sprintf "% d" 9) " 9"
            equal (sprintf "%05d" 9) "00009"
            equal (sprintf "%0+5d" 9) "+0009"
            equal (sprintf "%05d" -9) "-0009"
            equal (sprintf "%+5d" 9) "   +9"
            equal (sprintf "%0*d" 5 9) "00009"
            equal (sprintf "%x" 7911) "1ee7"
            equal (sprintf "%X" 7911) "1EE7"
            equal (sprintf "%o" 735) "1337"
        }

        Test "Continuation" {
            equal (Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper") 10
            let lenf = Printf.ksprintf (fun s -> s.Length) 
            equal (lenf "Web%s" "Sharper") 10
            equal (lenf "Cloud%s" "Sharper") 12
        }

        Test "Floating-Point" {
            equal (sprintf "%10.5f" 9.) "   9.00000"
            equal (sprintf "%10.5f" 9.) "   9.00000"
        }

        Test "Generic" {
            equal (sprintf "%O World" (Hi())) "Hello World"            
        }

        Test "Pretty-Print" {
            equal (sprintf "%A World" (Hi())) "Hello World"            
            equal (sprintf "%A" [| 3; 4 |]) "[|3; 4|]"            
            equal (sprintf "%A" {A = 1; B = 2} ) "{A = 1; B = 2}"            
            equal (sprintf "%A" (3, 4)) "(3, 4)"
            equal (sprintf "%A" [3; 4]) "[3; 4]"
            equal (sprintf "%A" (Some 1)) "Some 1"
            equal (sprintf "%A" (Cons (1, Cons (2, Empty)))) "Cons (1, Cons (2, Empty))"
            equal (sprintf "%A" [|Some 1; Some 2|]) "[|Some 1; Some 2|]"
            equal (sprintf "%A" {C = 1; D = Some {C = 2; D = None}}) "{C = 1; D = Some {C = 2; D = None}}"            
            equal (sprintf "%A" {E = 1, Some {E = 2, None}}) "{E = (1, Some {E = (2, None)})}"            
            equal (sprintf "%A" (Array2D.init 2 2 (fun r c -> 2 * r + c))) "[[0; 1][2; 3]]"  
            let pr (x: obj) = sprintf "%A" x
            notEqual (pr (Some 1)) "Some 1"
        }

        Test "Console" {
    //        WatchConsole()
            printfn "Printing to console %s" "ok"
            equal 1 1
    //        GetLastLogged() =? [| "Printing to console ok" |]
        }

        Test "FailWithF" {
            raises (failwithf "error: %s" "test")
        }

        Test "KSprintF" {
            equal (Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper") 10
        }

    }