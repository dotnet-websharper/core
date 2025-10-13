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

/// Defines macros used by proxy definitions.
module private WebSharper.Utils

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

[<JavaScript>]
let charRange (min: char) (max: char) : seq<char> =
    let minv = int min
    let count = 1 + int max - minv
    if count <= 0 then Seq.empty
    else Seq.init count (fun x -> char (x + minv))

[<JavaScript>]
let bigintRange (min: bigint) (max: bigint) : seq<bigint> =
    let count = 1I + max - min
    if count <= 0I then Seq.empty
    else Seq.unfold (fun state -> if state > max then None else Some(state, state + 1I)) min

[<JavaScript>]
let charStep (min: char) (step: char) (max: char) : seq<char> =
    let minv = int min
    let count = 1 + (int max - minv) / (int step)
    if count <= 0 then Seq.empty
    else Seq.init count (fun x -> char (x + minv))

[<JavaScript>]
let bigintStep (min: bigint) (step: bigint) (max: bigint) : seq<bigint> =
    let pos = step > 0I
    let count = 1I + max - min
    if count <= 0I then Seq.empty
    else Seq.unfold (fun state -> if (pos && state > max) || (not pos && state < max) then None else Some(state, state + step)) min

[<JavaScript>]
let nullableOp (a: obj) (b: obj) f = if a ==. null || b ==. null then null else f a b

[<JavaScript>]
let nullableOpL (a: obj) (b: obj) f = if a ==. null then null else f a b

[<JavaScript>]
let nullableOpR (a: obj) (b: obj) f = if b ==. null then null else f a b

[<JavaScript>]
let nullableCmp (a: obj) (b: obj) f = if a ==. null || b ==. null then false else f a b

[<JavaScript>]
let nullableCmpE (a: obj) (b: obj) f =
    if a ==. null then
        b ==. null
    elif b ==. null then 
        false 
    else f a b

[<JavaScript>]
let nullableCmpL (a: obj) (b: obj) f = if a ==. null then false else f a b

[<JavaScript>]
let nullableCmpR (a: obj) (b: obj) f = if b ==. null then false else f a b

[<JavaScript>]
let nullableConv (a: obj) f = if a ==. null then null else f a

[<JavaScript>]
let adjustSigned (number: obj) (length: int) =
    if number <. 0 then
        if length = 32 then
            number +. ((1 <<. 16) *. (1 <<. 16))
        else
            number +. (1 <<. length)
    else
        number

[<JavaScript>]
let plusForPosSignAdjusted s =
    "+" + s     

[<JavaScript>]
let spaceForPosSignAdjusted s =
    " " + s
