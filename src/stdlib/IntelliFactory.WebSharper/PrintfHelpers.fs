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

/// Defines macros used by proxy definitions.
module private IntelliFactory.WebSharper.PrintfHelpers

module J = IntelliFactory.WebSharper.JavaScript

[<JavaScript>]
let toSafe (s: string) =
    if s ==. null then "" else s  

[<JavaScript>]
let plusForPos (n: obj, s) =
    if 0 <=. n then "+" + s else s     

[<JavaScript>]
let spaceForPos (n: obj, s) =
    if 0 <=. n then " " + s else s      

[<Inline "$s.substr(1)">]
let skip1 (s: string) = X<string>

[<JavaScript>]
let padNumLeft (s: string, l) =
    let f = (As<string[]> s).[0]
    if f = " " || f = "+" || f = "-" then
        f + (skip1 s).PadLeft(l - 1, '0')
    else s.PadLeft(l, '0')

[<JavaScript>]
let plusForPos0 (n: obj, s, l) =
    if 0 <=. n then "+" + s else s     

[<JavaScript>]
let rec prettyPrint (o: obj) =
    let printObject (o: obj) =
        let s = string o
        if s = "[object Object]" then
            "{ " + (J.GetFields o |> Array.map (fun (k, v) -> k + " = " + prettyPrint v) |> String.concat "; ") + " }"
        else s
    let t = J.TypeOf o
    if t  ==. J.String then
        "\"" + As o + "\""
    elif t  ==. J.Object then
        if J.InstanceOf o J.Global?Array then
            "[| " + (As o |> Array.map prettyPrint |> String.concat "; ") + " |]"
        else printObject o
    else string o
