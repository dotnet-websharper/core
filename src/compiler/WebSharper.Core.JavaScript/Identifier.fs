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

module WebSharper.Core.JavaScript.Identifier

type R = System.Text.RegularExpressions.Regex
type RO = System.Text.RegularExpressions.RegexOptions

let InvalidCharacterPattern = R("[^_$a-zA-Z0-9]", RO.Compiled)
let ValidIdentifierPattern = R("^[_$a-zA-Z][_$a-zA-Z0-9]*$", RO.Compiled)

let ReservedWords =
    System.Collections.Generic.HashSet [|
        "abstract"  
        "arguments"
        "await"
        "boolean"
        "break"
        "byte"
        "case"
        "catch"
        "char"
        "class"
        "const"
        "continue"
        "debugger"
        "default"
        "delete"
        "do"
        "double"
        "else"
        "enum"
        "eval"
        "export"
        "extends"
        "false"
        "final"
        "finally"
        "float"
        "for"
        "function"
        "goto"
        "if"
        "implements"
        "import"
        "in"
        "instanceof"
        "int"
        "interface"
        "let"
        "long"
        "native"
        "new"
        "null"
        "package"
        "private"
        "protected"
        "public"
        "return"
        "short"
        "static"
        "super"
        "switch"
        "synchronized"
        "this"
        "throw"
        "throws"
        "transient"
        "true"
        "try"
        "typeof"
        "var"
        "void"
        "volatile"
        "while"
        "with" 
        "yield"
    |]

let IsReserved x = ReservedWords.Contains x

let ObjectMembers =
    System.Collections.Generic.HashSet [|
        "constructor"
        "hasOwnProperty"
        "isPrototypeOf"
        "propertyIsEnumerable"
        "prototype"
        "toLocaleString"
        "toSource"
        "toString"
        "unwatch"
        "valueOf"
        "watch"
    |]

let IsObjectMember x = ObjectMembers.Contains x

let IsValid x =
    x <> null
    && ValidIdentifierPattern.Match(x).Success
    && not (IsReserved x)

let MakeValid x =
    if x = null then "_"
    elif not (ValidIdentifierPattern.Match(x).Success) then InvalidCharacterPattern.Replace(x, "_")
    elif IsReserved x then x + "_1"
    else x

// Names are the argument written in base 64, digits taken from `digits`: 0-63
// give `a`-`$`, 64 gives `ba`, and so on. 64^5 = 2^30 < Int32.MaxValue < 64^6,
// so a name never has more than 6 digits.
//
// The formatter keeps no state between calls: the JavaScript and TypeScript
// writers share one instance across compilations, which run concurrently in
// the booster service (and in any host compiling on several threads). Each call
// builds its name in its own 6-char buffer; one-digit names are precomputed.
let MakeFormatter () =
    let digits = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"
    let single = Array.init 64 (fun i -> MakeValid (string digits.[i]))
    fun (x: int) ->
        if x < 64 then single.[x]
        else
            let buf = Array.zeroCreate<char> 6
            let mutable i = 6
            let mutable k = x
            while k >= 64 do
                i <- i - 1
                buf.[i] <- digits.[k % 64]
                k <- k / 64
            i <- i - 1
            buf.[i] <- digits.[k]
            MakeValid (System.String(buf, i, 6 - i))
