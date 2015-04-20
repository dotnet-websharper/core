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
    Section "Printf"

    Test "Strings" {
        Equal (sprintf "Web%s" "Sharper") "WebSharper"
        Equal (sprintf "Web%s%s" "Shar" "per") "WebSharper"
        Equal (sprintf "%5s" "hi") "   hi"
        Equal (sprintf "%-5s" "hi") "hi   "
        Equal (sprintf "%*s" 5 "hi") "   hi"
        Equal (sprintf "hi%s" null) "hi"
    }

    Test "Char" {
        Equal (sprintf "%c" 'a') "a"
    }

    Test "Integral" {
        Equal (sprintf "%d" 9) "9"
        Equal (sprintf "%+d" 9) "+9"
        Equal (sprintf "% d" 9) " 9"
        Equal (sprintf "%05d" 9) "00009"
        Equal (sprintf "%0+5d" 9) "+0009"
        Equal (sprintf "%05d" -9) "-0009"
        Equal (sprintf "%+5d" 9) "   +9"
        Equal (sprintf "%0*d" 5 9) "00009"
        Equal (sprintf "%x" 7911) "1ee7"
        Equal (sprintf "%X" 7911) "1EE7"
        Equal (sprintf "%o" 735) "1337"
    }

    Test "Continuation" {
        Equal (Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper") 10
        let lenf = Printf.ksprintf (fun s -> s.Length) 
        Equal (lenf "Web%s" "Sharper") 10
        Equal (lenf "Cloud%s" "Sharper") 12
    }

    Test "Floating-Point" {
        Equal (sprintf "%10.5f" 9.) "   9.00000"
        Equal (sprintf "%10.5f" 9.) "   9.00000"
    }

    Test "Generic" {
        Equal (sprintf "%O World" (Hi())) "Hello World"            
    }

    Test "Pretty-Print" {
        Equal (sprintf "%A World" (Hi())) "Hello World"            
        Equal (sprintf "%A" [| 3; 4 |]) "[|3; 4|]"            
        Equal (sprintf "%A" {A = 1; B = 2} ) "{A = 1; B = 2}"            
        Equal (sprintf "%A" (3, 4)) "(3, 4)"
        Equal (sprintf "%A" [3; 4]) "[3; 4]"
        Equal (sprintf "%A" (Some 1)) "Some 1"
        Equal (sprintf "%A" (Cons (1, Cons (2, Empty)))) "Cons (1, Cons (2, Empty))"
        Equal (sprintf "%A" [|Some 1; Some 2|]) "[|Some 1; Some 2|]"
        Equal (sprintf "%A" {C = 1; D = Some {C = 2; D = None}}) "{C = 1; D = Some {C = 2; D = None}}"            
        Equal (sprintf "%A" {E = 1, Some {E = 2, None}}) "{E = (1, Some {E = (2, None)})}"            
        Equal (sprintf "%A" (Array2D.init 2 2 (fun r c -> 2 * r + c))) "[[0; 1][2; 3]]"  
        let pr (x: obj) = sprintf "%A" x
        NotEqual (pr (Some 1)) "Some 1"
    }

    Test "Console" {
//        WatchConsole()
        printfn "Printing to console %s" "ok"
        Equal 1 1
//        GetLastLogged() =? [| "Printing to console ok" |]
    }

    Test "FailWithF" {
        Raises (failwithf "error: %s" "test")
    }

    Test "KSprintF" {
        Equal (Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper") 10
    }
