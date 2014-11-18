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

type X = { A: int; B: int }

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

    Test "Floating-Point" {
        sprintf "%10.5f" 9. =? "   9.00000"
        sprintf "%10.5f" 9. =? "   9.00000"
    }

    Test "Generic" {
        sprintf "%O World" (Hi()) =? "Hello World"            
    }

    Test "Pretty-Print" {
        sprintf "%A World" (Hi()) =? "Hello World"            
        sprintf "%A" [| 3; 4 |] =? "[| 3; 4 |]"            
        sprintf "%A" { A = 1; B = 2 }  =? "{ A = 1; B = 2 }"            
    }

    Test "PrintfModule" {
        Printf.sprintf "Web%s" "Sharper" =? "WebSharper"
    }

    Test "Console" {
        printfn "Printing to console %s" "ok"
    }

    Test "FailWithF" {
        Assert.Raises (fun () -> failwithf "error: %s" "test")
    }

    Test "KSprintF" {
        Printf.ksprintf (fun s -> s.Length) "Web%s" "Sharper" =? 10
    }
