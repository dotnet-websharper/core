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

module IntelliFactory.JavaScript.Test.ParserTests

open System.Globalization
open System.Threading
module P = IntelliFactory.JavaScript.Parser
module S = IntelliFactory.JavaScript.Syntax

let private p s =
    P.ParseExpression (P.Source.FromString s)

let private pp s =
    P.ParseProgram (P.Source.FromString s)

let private ps s =
    match pp s with
    | [S.Action s] -> s
    | _ -> invalidArg "s" "Not a statement."

let Run () =

    Section "Parser"

    Test "literals" {
        p "this"  =? S.This
        p "null"  =? !~S.Null
        p "false" =? !~S.False
        p "true"  =? !~S.True
    }

    Test "numbers" {
        p "0"    =? !~ (S.Number "0")
        p "1E-1" =? !~ (S.Number "1E-1")
        p "1.25" =? !~ (S.Number "1.25")
    }

    Test "strings" {
        let ( =? ) a b  = a =? !~ b
        p @"''"         =? S.String ""
        p @"'hello'"    =? S.String "hello"
        p @"'(\')'"     =? S.String "(')"
        p @"'(\"")'"    =? S.String "(\")"
        p @"'(\\)'"     =? S.String "(\\)"
        p @"'(\b)'"     =? S.String "(\b)"
        p @"'(\f)'"     =? S.String "(\f)"
        p @"'(\n)'"     =? S.String "(\n)"
        p @"'(\r)'"     =? S.String "(\r)"
        p @"'(\t)'"     =? S.String "(\t)"
        p @"'(\v)'"     =? S.String "(\v)"
        p @"'(\xAb)'"   =? S.String "(\xAb)"
        p @"'(\uAb56)'" =? S.String "(\uAb56)"
    }

    Test "arrays" {
        p @"[]"  =? S.NewArray []
        p @"[,]" =? S.NewArray [None]
        let (!) x = Some (!~ (S.Number (string x)))
        p @"[,1,,2,,3,]" =? S.NewArray [None; !1; None; !2; None; !3]
    }

    Test "objects" {
        let num s = !~ (S.Number s)
        let one   = num "1"
        p @"{}"             =? S.NewObject []
        p @"{a: 1}"         =? S.NewObject ["a", one]
        p @"{""a\nb"": 1}"  =? S.NewObject ["a\nb", one]
        p @"{1.25: 1}"      =? S.NewObject ["1.25", one]
        p "{a: 1, b: 2, }"  =? S.NewObject ["a", one; "b", num "2"]
    }

    Test "regex" {
        p "/a/b"        =? S.NewRegex "/a/b"
        p "/[a-z]+/"    =? S.NewRegex "/[a-z]+/"
    }

    Test "identifiers" {
        p "alpha"   =? S.Var "alpha"
        p "throws"  =? S.Var "throws"
        p "\u0436x" =? S.Var "\u0436x"
        Throws<P.ParserError>(fun () -> p "throw" |> ignore)
    }

    Test "application" {
        let f = S.Var "f"
        let x = S.Var "x"
        let y = S.Var "y"
        p "f()" =? S.Application (f, [])
        p "f(x)" =? S.Application (f, [x])
        p "f(x,y)" =? S.Application (f, [x; y])
    }

    Test "new" {
        let f = S.Var "f"
        let x = S.Var "x"
        let y = S.Var "y"
        p "new f" =? S.New (f, [])
        p "new f()" =? S.New (f, [])
        p "new new f" =? S.New (S.New (f, []), [])
        p "new new f(x)" =? S.New (S.New (f, [x]), [])
        p "new new f(x)(y)" =? S.New (S.New (f, [x]), [y])
    }

    Test "member" {
        let x = S.Var "x"
        let y = S.Var "y"
        let ( / ) a b  = S.Binary (a, S.BinaryOperator.``.``, !~ b)
        let ( /. ) a b = S.Binary (a, S.BinaryOperator.``.``, b)
        p "x.y" =? x / S.String "y"
        p "x.y.z" =? x / S.String "y" / S.String "z"
        p @"x[""y""]" =? x / S.String "y"
        p @"x[y]" =? x /. y
        p @"x[1]" =? x / S.Number "1"
        p @"x[1.25E-1]" =? x / S.Number "1.25E-1"
    }

    Test "lhs" {
        p "new a.b(c)[d](e)" =?
            (S.New ((S.Var "a")?b, [S.Var "c"])).[S.Var "d"].[[S.Var "e"]]
    }

    Test "operators" {
        let ( ! ) x = S.Var x
        let ( +. ) a b = S.Binary (a, S.BinaryOperator.``+``, b)
        let ( *. ) a b = S.Binary (a, S.BinaryOperator.``*``, b)
        p "a + b * (c + d)" =? !"a" +. !"b" *. (!"c" +. !"d")
        p "-a" =? S.Unary (S.UnaryOperator.``-``, !"a")
        p "a++" =? S.Postfix (!"a", S.PostfixOperator.``++``)
        p "a, b + c" =? S.Binary (!"a", S.BinaryOperator.``,``, !"b" +. !"c")
        p "a ? b : c ? d : e" =?
            S.Conditional (!"a", !"b", S.Conditional (!"c", !"d", !"e"))
    }

    Test "block" {
        let t x y =
            match pp x with
            | [S.Action (S.Block x)] -> Seq.toList x =? y
            | _ -> ()
        t "{}" []
        t "{1;}" [S.Ignore (!~ (S.Number "1"))]
        t "{1}" [S.Ignore (!~ (S.Number "1"))]
    }

    Test "var" {
        let one = !~ (S.Number "1")
        ps "var x" =? S.Vars ["x", None]
        ps "var x = 1" =? S.Vars ["x", Some one]
        ps "var x, y = 1" =? S.Vars ["x", None; "y", Some one]
    }

    Test "if" {
        let num s = !~ (S.Number s)
        let one   = num "1"
        let two   = num "2"
        let three = num "3"
        let ( ! ) = S.Ignore
        ps "if (x) 1" =? S.If (S.Var "x", !one, S.Empty)
        ps "if (x) 1; else 2" =? S.If (S.Var"x", !one, !two)
        ps "if (x) if (y) 1; else 2; else 3" =?
            S.If (S.Var "x", S.If (S.Var "y", !one, !two), !three)
    }
