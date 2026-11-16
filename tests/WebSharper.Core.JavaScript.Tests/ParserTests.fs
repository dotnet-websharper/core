// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

module WebSharper.Core.JavaScript.Test.ParserTests

open NUnit.Framework

open System.Globalization
open System.Threading
module P = WebSharper.Core.JavaScript.Parser
module S = WebSharper.Core.JavaScript.Syntax

module S =
    let Var x = S.Var (S.Id.New x)
    let Vars xs = S.Vars (xs |> List.map (fun (x, v) -> S.Id.New x, v), S.VarDecl)

let private p s =
    P.ParseExpression (P.Source.FromString s)

let private pp s =
    P.ParseProgram (P.Source.FromString s)

let private ps s =
    match pp s with
    | [ s ] -> s
    | _ -> invalidArg "s" "Not a statement."

[<Test>]
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
        p "20n"  =? !~ (S.Number "20n")
        p "0b101n" =? !~ (S.Number "0b101n")
        p "0o77n"  =? !~ (S.Number "0o77n")
        p "0xFFn"  =? !~ (S.Number "0xFFn")
        p "1_000" =? !~ (S.Number "1_000")
        p "1_000n" =? !~ (S.Number "1_000n")
        p "1_2_3.4_5" =? !~ (S.Number "1_2_3.4_5")
        p "1.2e3_4" =? !~ (S.Number "1.2e3_4")
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
        p @"{a: 1}"         =? S.NewObject ["a", S.Simple, one]
        p @"{""a\nb"": 1}"  =? S.NewObject ["a\nb", S.Simple, one]
        p @"{1.25: 1}"      =? S.NewObject ["1.25", S.Simple, one]
        p "{a: 1, b: 2, }"  =? S.NewObject ["a", S.Simple, one; "b", S.Simple, num "2"]
    }

    Test "regex" {
        p "/a/b"        =? S.NewRegex "/a/b"
        p "/[a-z]+/"    =? S.NewRegex "/[a-z]+/"
    }

    Test "identifiers" {
        p "alpha"   =? S.Var "alpha"
        p "throws"  =? S.Var "throws"
        p "\u0436x" =? S.Var "\u0436x"
        Testing.Throws<P.ParserError>(fun () -> p "throw" |> ignore)
    }

    Test "application" {
        let f = S.Var "f"
        let x = S.Var "x"
        let y = S.Var "y"
        p "f()" =? S.Application (f, [], [])
        p "f(x)" =? S.Application (f, [], [x])
        p "f(x,y)" =? S.Application (f, [], [x; y])
    }

    Test "new" {
        let f = S.Var "f"
        let x = S.Var "x"
        let y = S.Var "y"
        p "new f" =? S.New (f, [], [])
        p "new f()" =? S.New (f, [], [])
        p "new new f" =? S.New (S.New (f, [], []), [], [])
        p "new new f(x)" =? S.New (S.New (f, [], [x]), [], [])
        p "new new f(x)(y)" =? S.New (S.New (f, [], [x]), [], [y])
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
            (S.New ((S.Var "a")?b, [], [S.Var "c"])).[S.Var "d"].[[S.Var "e"]]
    }

    Test "operators" {
        let ( ! ) x = S.Var x
        let ( +. ) a b = S.Binary (a, S.BinaryOperator.``+``, b)
        let ( *. ) a b = S.Binary (a, S.BinaryOperator.``*``, b)
        let ( **. ) a b = S.Binary (a, S.BinaryOperator.``**``, b)
        p "a + b * (c + d)" =? !"a" +. !"b" *. (!"c" +. !"d")
        p "-a" =? S.Unary (S.UnaryOperator.``-``, !"a")
        p "a++" =? S.Postfix (!"a", S.PostfixOperator.``++``)
        p "a, b + c" =? S.Binary (!"a", S.BinaryOperator.``,``, !"b" +. !"c")
        p "a ? b : c ? d : e" =?
            S.Conditional (!"a", !"b", S.Conditional (!"c", !"d", !"e"))
        p "a ** b" =? S.Binary (!"a", S.BinaryOperator.``**``, !"b")
        p "a ** b ** c" =? S.Binary (!"a", S.BinaryOperator.``**``, S.Binary (!"b", S.BinaryOperator.``**``, !"c"))
        p "a ** b + c" =? S.Binary (S.Binary (!"a", S.BinaryOperator.``**``, !"b"), S.BinaryOperator.``+``, !"c")
    }

    Test "arrow function - single-arg, concise body" {
        match p "x => x" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.length ids =? 1
            ids[0].Name =? "x"
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] ->
                match e with
                | WebSharper.Core.JavaScript.Syntax.Var id -> 
                    id.Name =? "x"
                | _ -> Assert.Fail "Expected variable return."
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "arrow function - multiple params, concise body" {
        match p "(x, y) => x + y" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.length ids =? 2
            ids[0].Name =? "x"
            ids[1].Name =? "y"
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] -> ()
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "arrow function - empty params, concise body" {
        match p "() => 42" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.isEmpty ids =? true
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] -> ()
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "arrow function - single-arg, block body" {
        match p "x => { return x }" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.length ids =? 1
            ids[0].Name =? "x"
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] ->
                match e with
                | WebSharper.Core.JavaScript.Syntax.Var id -> 
                    Assert.AreEqual("x", id.Name)
                | _ -> Assert.Fail "Expected variable return."
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "arrow function - multiple params, block body" {
        match p "(x, y) => { return x + y; }" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.length ids =? 2
            ids[0].Name =? "x"
            ids[1].Name =? "y"
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] -> ()
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "arrow function - empty params, block body" {
        match p "() => { return 42 }" with
        | WebSharper.Core.JavaScript.Syntax.Lambda (None, ids, body, true, _) ->
            List.isEmpty ids =? true
            match body with
            | [ WebSharper.Core.JavaScript.Syntax.Return (Some e) ] -> ()
            | _ -> Assert.Fail "Expected single return statement in body."
        | _ -> Assert.Fail "Expected lambda expression."
    }

    Test "block" {
        let t x y =
            match pp x with
            | [ S.Block x ] -> Seq.toList x =? y
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

    // ES2022 forms: parse, then write back in compact form
    let W = WebSharper.Core.JavaScript.Writer.ExpressionToString WebSharper.Core.JavaScript.Preferences.Compact
    let WP = WebSharper.Core.JavaScript.Writer.ProgramToString WebSharper.Core.JavaScript.Preferences.Compact
    let rt (s: string) (expected: string) = (W (p s)).Trim() =? expected
    let rtp (s: string) (expected: string) = (WP (pp s)).Trim().Replace("\r", "").Replace("\n", "") =? expected

    Test "template literals" {
        rt "`a${x}b`" "`a${x}b`"
        rt "`a${ `inner${y}` }b${z}`" "`a${`inner${y}`}b${z}`"
        rt @"`line\nbreak \` \${notahole}`" @"`line\nbreak \` \${notahole}`"
        rt "tag`x${y}`" "tag`x${y}`"
        rt "a.b`x`" "a.b`x`"
        rt "``" "``"
        rt "`${a}${b}`" "`${a}${b}`"
    }

    Test "spread, rest and default parameters" {
        rt "f(...xs, 1)" "f(...xs, 1)"
        rt "[1, ...xs]" "[1, ...xs]"
        rt "(a, b = 2, ...rest) => a" "(a, b=2, ...rest) => a"
        rt "function (a = 1, ...r) { return r }" "function(a=1, ...r){ return r;}"
        rt "{...a, b: 1}" "{...a, b:1}"
    }

    Test "optional chaining and nullish" {
        rt "a?.b" "a?.b"
        rt "a?.[i]" "a?.[i]"
        rt "a?.b.c(d)" "a?.b.c(d)"
        rt "f?.(1)" "f?.(1)"
        rt "() => ({ f: g }?.f?.(2))" "() =>({f:g}?.f?.(2))"
        rt "a ? .5 : b" "a?.5:b"
        rt "a ?? b" "a??b"
        rt "(a || b) ?? c" "(a||b)??c"
        rt "a ??= b" "a??=b"
        rt "a &&= b" "a&&=b"
        rt "a ||= b" "a||=b"
        rt "a **= 2" "a**=2"
    }

    Test "object literals" {
        rt "{ a, b }" "{a:a, b:b}"
        rt "{ f(x) { return x } }" "{f(x){ return x;}}"
        rt "{ get x() { return 1 }, set x(v) { } }" "{get x(){ return 1;}, set x(v){ }}"
        rt "{ [k]: 1 }" "{[k]:1}"
        rt "{ async f() { await g() }, *h() { yield 1 } }" "{async f(){ await g();}, *h(){ yield 1;}}"
        rt "{ get: 1, set: 2, async: 3 }" "{get:1, set:2, async:3}"
    }

    Test "functions" {
        rt "async x => await x" "async(x) => await x"
        rt "async (a, b) => a" "async(a, b) => a"
        rt "async function () { await f() }" "async function(){ await f();}"
        rt "function* () { yield 1; yield* xs }" "function*(){ yield 1;yield*xs;}"
        rt "async function* g() { yield await x }" "async function* g(){ yield await x;}"
        rt "async(1)" "async(1)"
    }

    Test "declarations and loops" {
        rtp "let x = 1; const y = 2;" "let x=1;const y=2;"
        rtp "for (const x of xs) f(x)" "for(const x of xs)f(x);"
        rtp "for (let k in o) f(k)" "for(let k in o)f(k);"
        rtp "for (x of xs) f(x)" "for(x of xs)f(x);"
        rtp "for (let i = 0; i < 3; i++) f(i)" "for(let i=0;i<3;i++)f(i);"
        rtp "{ function f() { } f() }" "{ function f(){ } f();}"
        rtp "async function f() { await g() }" "async function f(){ await g();}"
        rtp "l: for (;;) break l" "l: for(;;)break l;"
        rtp "try { f() } catch { g() }" "try { f();} catch(_){ g();}"
    }

    Report ()
