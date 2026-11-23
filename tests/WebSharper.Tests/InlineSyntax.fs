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

/// ES2022 syntax in inline JavaScript
module WebSharper.Tests.InlineSyntax

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<Inline "`a${$x}b${$y + 1}c`">]
let template (x: string) (y: int) = X<string>

[<Inline "String.raw`a${$x}\\n`">]
let taggedTemplate (x: int) = X<string>

[<Inline "((a, b = 10, ...rest) => a + b + rest.length)(...$xs)">]
let spreadAndDefaults (xs: int[]) = X<int>

[<Inline "[0, ...$xs, 4]">]
let arraySpread (xs: int[]) = X<int[]>

[<Inline "$o?.a?.b ?? 'none'">]
let optional (o: obj) = X<string>

[<Inline "$o?.f?.(2)">]
let optionalCall (o: obj) = X<obj>

[<Inline "{ v: 10, m(x) { return this.v + x } }">]
let objWithMethod () = X<obj>

[<Inline "$o.m?.(1)">]
let optionalMethodCall (o: obj) = X<int>

[<Inline "const r = []; for (const x of $xs) r.push(x * 2); return r">]
let forOf (xs: int[]) = X<int[]>

[<Inline "let x = $a; x ??= 5; let y = $b; y ||= 7; let z = 2; z **= 3; let w = $b; w &&= 9; return [x, y, z, w]">]
let assignOps (a: obj) (b: int) = X<obj[]>

[<Inline "const o = { n: null, c: 3 }; o.n ??= 1; o.c ||= 8; o['c'] **= 2; return [o.n, o.c]">]
let memberAssignOps () = X<int[]>

[<Inline "{ get v() { return this.x * 2 }, x: $x, [$k]: 1, ...$extra, after: 2 }">]
let objectLiteral (x: int) (k: string) (extra: obj) = X<obj>

[<Inline "[...(function* () { yield 1; yield* [2, 3]; })()]">]
let generator () = X<int[]>

[<Inline "function* g(n) { for (let i = 0; i < n; i++) yield i; } return [...g($n)]">]
let generatorDeclaration (n: int) = X<int[]>

[<Inline "(async (x) => await x + 1)($p)">]
let asyncArrow (p: Promise<int>) = X<Promise<int>>

[<Inline "outer: for (const a of [1, 2, 3]) { for (const b of [1, 2, 3]) { if (b == 2) continue outer; if (a == 3) break outer; $r.push(a * 10 + b); } }">]
let labels (r: int[]) = X<unit>

[<Inline "function f(x = 2, y = x * 3) { return x + y } return [f(), f(1), f(1, 1)]">]
let functionDefaults () = X<int[]>

[<JavaScript>]
let Tests =
    TestCategory "Inline JavaScript syntax" {

        Test "Template literals" {
            equal (template "x" 1) "axb2c"
            equal (taggedTemplate 1) "a1\\n"
        }

        Test "Spread, rest and default parameters" {
            equal (spreadAndDefaults [| 1 |]) 11
            equal (spreadAndDefaults [| 1; 2; 3; 4 |]) 5
            equal (arraySpread [| 1; 2; 3 |]) [| 0; 1; 2; 3; 4 |]
            equal (functionDefaults ()) [| 8; 4; 2 |]
        }

        Test "Optional chaining and nullish coalescing" {
            equal (optional null) "none"
            equal (optional (New [ "a" => New [ "b" => "B" ] ])) "B"
            equal (optional (New [ "a" => null ])) "none"
            isTrue (JS.TypeOf (optionalCall null) = JS.Kind.Undefined)
            equal (optionalCall (New [ "f" => (fun (x: int) -> x + 1) ])) (box 3)
            equal (optionalMethodCall (objWithMethod ())) 11
            isTrue (JS.TypeOf (optionalMethodCall (New [])) = JS.Kind.Undefined)
        }

        Test "Logical and exponent assignment" {
            equal (assignOps null 0) [| box 5; box 7; box 8; box 0 |]
            equal (assignOps 1 4) [| box 1; box 4; box 8; box 9 |]
            equal (memberAssignOps ()) [| 1; 9 |]
        }

        Test "Object literals" {
            let o = objectLiteral 3 "k" (New [ "e" => 4 ])
            equal o?v 6
            equal o?x 3
            equal o?k 1
            equal o?e 4
            equal o?after 2
        }

        Test "for...of and labels" {
            equal (forOf [| 1; 2 |]) [| 2; 4 |]
            let r = [||]
            labels r
            equal r [| 11; 21 |]
        }

        Test "Generators" {
            equal (generator ()) [| 1; 2; 3 |]
            equal (generatorDeclaration 3) [| 0; 1; 2 |]
        }

        Test "Async functions" {
            let! res = asyncArrow (Promise.Resolve 41) |> Promise.AsAsync
            equal res 42
        }
    }
