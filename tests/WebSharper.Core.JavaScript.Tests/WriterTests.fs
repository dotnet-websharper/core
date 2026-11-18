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

module WebSharper.Core.JavaScript.Test.WriterTests

open NUnit.Framework

module S = WebSharper.Core.JavaScript.Syntax
module W = WebSharper.Core.JavaScript.Writer
type private P = WebSharper.Core.JavaScript.Preferences

[<Test>]
let Run () =
    Section "Writer"

    Test "Bug #484" {
        let e = (!~(S.Number "0")).[!~(S.String "toString")].[[]]
        let s = W.ExpressionToString P.Compact e
        s.Trim() =? "(0).toString()"
    }

    let v x = S.Var (S.Id.New x)
    let n x = !~(S.Number x)
    let w e = (W.ExpressionToString P.Compact e).Trim()
    let ws s = (W.ProgramToString P.Compact [ s ]).Trim()

    Test "Function kinds" {
        w (S.Lambda (None, [ S.Id.New "x" ], [ S.Return (Some (S.Await (v "x"))) ], true, S.Async)) =? "async(x) => await x"
        w (S.Lambda (Some (S.Id.New "g"), [], [ S.Ignore (S.Yield (Some (n "1"), false)) ], false, S.Generator)) =? "function* g(){ yield 1;}"
        w (S.Lambda (None, [], [ S.Ignore (S.Yield (Some (v "xs"), true)) ], false, S.AsyncGenerator)) =? "async function*(){ yield*xs;}"
        ws (S.Function (S.Id.New "f", [], Some [], S.Async)) =? "async function f(){ }"
    }

    Test "Template literals" {
        w (S.TemplateLiteral (None, [ "a"; "b" ], [ v "x" ])) =? "`a${x}b`"
        w (S.TemplateLiteral (None, [ "`\\${\n" ], [])) =? "`\\`\\\\\\${\\n`"
        w (S.TemplateLiteral (Some (v "tag"), [ ""; "" ], [ v "x" ])) =? "tag`${x}`"
    }

    Test "Spread and optional chaining" {
        w (S.Application (v "f", [], [ S.Spread (v "xs") ])) =? "f(...xs)"
        w (S.NewArray [ Some (n "1"); Some (S.Spread (v "xs")) ]) =? "[1, ...xs]"
        w (S.Binary (v "a", S.BinaryOperator.``?.``, !~(S.String "b"))) =? "a?.b"
        w (S.Binary (v "a", S.BinaryOperator.``?.``, v "i")) =? "a?.[i]"
        w (S.OptionalApplication (v "f", [ n "1" ])) =? "f?.(1)"
    }

    Test "Nullish coalescing is parenthesized next to && and ||" {
        let b x op y = S.Binary (x, op, y)
        w (b (b (v "a") S.BinaryOperator.``||`` (v "b")) S.BinaryOperator.``??`` (v "c")) =? "(a||b)??c"
        w (b (v "a") S.BinaryOperator.``&&`` (b (v "b") S.BinaryOperator.``??`` (v "c"))) =? "a&&(b??c)"
    }

    Test "Default parameters and for...of" {
        w (S.Lambda (None, [ S.Id.New("x", def = n "1") ], [ S.Return (Some (v "x")) ], true, S.Normal)) =? "(x=1) => x"
        ws (S.ForDeclOf (S.ConstDecl, S.Id.New "x", v "xs", S.Ignore (v "x"))) =? "for(const x of xs)x;"
    }

    Report ()
