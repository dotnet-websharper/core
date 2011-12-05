// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.JavaScript.Identifier

type R = System.Text.RegularExpressions.Regex
type RO = System.Text.RegularExpressions.RegexOptions

let InvalidCharacterPattern = R("[^_$a-zA-Z0-9]", RO.Compiled)
let ValidIdentifierPattern = R("^[_$a-zA-Z][_$a-zA-Z0-9]*$", RO.Compiled)

let IsReserved x =
    match x with
    | "abstract"
    | "as"
    | "boolean"
    | "break"
    | "byte"
    | "case"
    | "catch"
    | "char"
    | "class"
    | "const"
    | "continue"
    | "debugger"
    | "default"
    | "delete"
    | "do"
    | "double"
    | "else"
    | "enum"
    | "export"
    | "extends"
    | "false"
    | "final"
    | "finally"
    | "float"
    | "for"
    | "function"
    | "goto"
    | "if"
    | "implements"
    | "import"
    | "in"
    | "instanceof"
    | "int"
    | "interface"
    | "long"
    | "native"
    | "new"
    | "null"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "return"
    | "short"
    | "static"
    | "super"
    | "switch"
    | "synchronized"
    | "this"
    | "throw"
    | "throws"
    | "transient"
    | "try"
    | "typeof"
    | "use"
    | "var"
    | "void"
    | "volatile"
    | "while"
    | "with" ->
        true
    | _ ->
        false

let IsValid x =
    x <> null
    && ValidIdentifierPattern.Match(x).Success
    && not (IsReserved x)

let MakeValid x =
    if IsValid x then x else
        "_" + InvalidCharacterPattern.Replace(x, "_")

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
