// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Tests.Printf

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

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
        sprintf "Web%s" "Sharper" =? "WebSharper"
        sprintf "Web%s%s" "Shar" "per" =? "WebSharper"
        sprintf "%5s" "hi" =? "   hi"
        sprintf "%-5s" "hi" =? "hi   "
        sprintf "%*s" 5 "hi" =? "   hi"
        sprintf "hi%s" null =? "hi"
    }

    Test "Char" {
        sprintf "%c" 'a' =? "a"    
    }

    Test "Integral" {
        sprintf "%d" 9 =? "9"
        sprintf "%+d" 9 =? "+9"
        sprintf "% d" 9 =? " 9"
        sprintf "%05d" 9 =? "00009"
        sprintf "%0+5d" 9 =? "+0009"
        sprintf "%05d" -9 =? "-0009"
        sprintf "%+5d" 9 =? "   +9"
        sprintf "%0*d" 5 9 =? "00009"
        sprintf "%x" 7911 =? "1ee7"
        sprintf "%X" 7911 =? "1EE7"
        sprintf "%o" 735 =? "1337"
    }

    Test "Continuation" {
        Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper" =? 10
        let lenf = Printf.ksprintf (fun s -> s.Length) 
        lenf "Web%s" "Sharper" =? 10
        lenf "Cloud%s" "Sharper" =? 12
    }

    Test "Floating-Point" {
        sprintf "%10.5f" 9. =? "   9.00000"
        sprintf "%10.5f" 9. =? "   9.00000"
    }

    Test "Generic" {
        sprintf "%O World" (Hi()) =? "Hello World"            
    }

    Test "Pretty-Print" {
        sprintf "%A World" (Hi()) =? "Hello World"            
        sprintf "%A" [| 3; 4 |] =? "[|3; 4|]"            
        sprintf "%A" {A = 1; B = 2}  =? "{A = 1; B = 2}"            
        sprintf "%A" (3, 4) =? "(3, 4)"
        sprintf "%A" [3; 4] =? "[3; 4]"
        sprintf "%A" (Some 1) =? "Some 1"
        sprintf "%A" (Cons (1, Cons (2, Empty))) =? "Cons (1, Cons (2, Empty))"
        sprintf "%A" [|Some 1; Some 2|] =? "[|Some 1; Some 2|]"
        sprintf "%A" {C = 1; D = Some {C = 2; D = None}} =? "{C = 1; D = Some {C = 2; D = None}}"            
        sprintf "%A" {E = 1, Some {E = 2, None}} =? "{E = (1, Some {E = (2, None)})}"            
        sprintf "%A" (Array2D.init 2 2 (fun r c -> 2 * r + c)) =? "[[0; 1][2; 3]]"  
        let pr (x: obj) = sprintf "%A" x
        pr (Some 1) <>? "Some 1"         
    }

    Test "Console" {
//        WatchConsole()
        printfn "Printing to console %s" "ok"
        1 =? 1
//        GetLastLogged() =? [| "Printing to console ok" |]
    }

    Test "FailWithF" {
        Assert.Raises (fun () -> failwithf "error: %s" "test")
    }

    Test "KSprintF" {
        Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper" =? 10
    }
