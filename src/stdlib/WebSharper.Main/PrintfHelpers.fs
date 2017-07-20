// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Defines macros used by proxy definitions.
module private WebSharper.PrintfHelpers

open WebSharper.JavaScript

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
let printList (p: obj -> string, o: obj list) =
    "[" + (o |> Seq.map p |> String.concat "; ") + "]" 

[<JavaScript>]
let printArray (p: obj -> string, o: obj[]) =
    if o ===. null then "null" else
    "[|" + (o |> Array.map p |> String.concat "; ") + "|]" 

[<JavaScript>]
let printArray2D (p: obj -> string, o: obj[,]) =
    if o ===. null then "null" else
     "[[" + (
        seq {
            let l2 = Array2D.length2 o
            for i in 0 .. Array2D.length1 o - 1 ->
                seq { for j in 0 .. l2 - 1 -> p o.[i, j] } 
                |> String.concat "; "
        }
        |> String.concat "]["
     ) + "]]" 

[<JavaScript>]
let rec prettyPrint (o: obj) =
    let printObject (o: obj) =
        let s = string o
        if s = "[object Object]" then
            "{" + (JS.GetFields o |> Array.map (fun (k, v) -> k + " = " + prettyPrint v) |> String.concat "; ") + "}"
        else s
    if o ===. null then "null" else
    let t = JS.TypeOf o
    if t  ==. JS.String then
        "\"" + As o + "\""
    elif t  ==. JS.Object then
        if o :? System.Array then
            "[|" + (As o |> Array.map prettyPrint |> String.concat "; ") + "|]"
        else printObject o
    else string o
