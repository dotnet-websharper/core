// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
        "_this"
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
    if IsValid x then x else
        "$" + InvalidCharacterPattern.Replace(x, "")

let MakeFormatter () =
    let b = System.Text.StringBuilder()
    let c = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"
    fun (x: int) ->
        let rec loop k =
            if k < 64 then
                b.Append c.[k] |> ignore
            else
                let (d, r) = System.Math.DivRem(k, 64)
                loop d
                b.Append c.[r] |> ignore
        loop x
        let r = b.ToString()
        b.Remove(0, r.Length) |> ignore
        MakeValid r
