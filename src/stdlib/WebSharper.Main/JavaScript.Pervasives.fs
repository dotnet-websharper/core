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

/// Defines operators and functions that are automatically available whenever
/// `WebSharper` is open.
[<AutoOpen>]
module WebSharper.JavaScript.Pervasives

open System.Runtime.CompilerServices

open WebSharper
module M = WebSharper.Core.Macros

/// Casts an object to the desired type.
[<Inline "$x">]
let As<'T> (x: obj) = X<'T>

[<Inline "$x * $y">]
let ( *. ) x y = X<obj>

[<Inline "$x / $y">]
let ( /. ) x y = X<obj>

[<Inline "$x % $y">]
let ( %. ) x y = X<obj>

[<Inline "$x + $y">]
let ( +. ) x y = X<obj>

[<Inline "$x - $y">]
let ( -. ) x y = X<obj>

[<Inline "$x << $y">]
let ( <<. ) x y = X<obj>

[<Inline "$x >> $y">]
let ( >>. ) x y = X<obj>

[<Inline "$x >>> $y">]
let ( >>>. ) x y = X<obj>

[<Inline "$x < $y">]
let ( <. ) x y = X<bool>

[<Inline "$x > $y">]
let ( >. ) x y = X<bool>

[<Inline "$x >= $y">]
let ( >=. ) x y = X<bool>

[<Inline "$x <= $y">]
let ( <=. ) x y = X<bool>

[<Inline "$x == $y">]
let ( ==. ) x y = X<bool>

[<Inline "$x === $y">]
let ( ===. ) x y = X<bool>

[<Inline "$x != $y">]
let ( !=. ) x y = X<bool>

[<Inline "$x !== $y">]
let ( !==. ) x y = X<bool>

[<Inline "$x | $y">]
let ( |. ) x y = X<obj>

[<Inline "$x & $y">]
let ( &. ) x y = X<obj>

[<Inline "$x ^ $y">]
let ( ^. ) x y = X<obj>

[<Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = X<'T>

[<Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = X<unit>

[<Inline "[$x,$y]">]
let ( => ) (x: string) (y: obj) = (x, y)

[<JavaScript>]
let private NewFromSeq<'T> (fields: seq<string * obj>) : 'T =
    let r = JS.Inline "{}"
    for (k, v) in fields do
        (?<-) r k v
    As r

/// Constructs a new object as if an object literal was used.
[<Macro(typeof<M.New>); Inline>]
let New<'T> (fields: seq<string * obj>) = NewFromSeq<'T> fields

/// Constructs an proxy to a remote object instance.
[<Constant null>]
let Remote<'T> = X<'T>

/// Gets JavaScript properties in sequence dynamically from an object.
[<JavaScript; Macro(typeof<M.GetJS>)>]
let GetJS<'T> (x: obj) (items: seq<string>) =
    let mutable x = x
    for i in items do
        x <- x?(i)
    As<'T> x    

/// Erases generic parameters inside this expression during WebSharper translation.
/// You can get use this to translate `defaultof` inside a generic function.
[<Macro(typeof<M.DefaultToUndefined>); MethodImpl(MethodImplOptions.NoInlining)>]
let DefaultToUndefined<'T> (x: 'T) = x

module Optional =
    /// Converts an F# option value to a JavaScript erased option
    [<Inline>]
    let ofOption x =
        match x with
        | None -> Undefined
        | Some v -> Defined v

    /// Converts a JavaScript erased option to an F# option value
    [<Inline>]
    let toOption x =
        match x with
        | Undefined -> None
        | Defined v -> Some v

    [<Inline "$x !== undefined">]
    let isDefined x =
        match x with
        | Undefined -> false
        | Defined _ -> true

    [<Inline "$x === undefined">]
    let isUndefined x =
        match x with
        | Undefined -> true
        | Defined _ -> false

[<JavaScript>]
module DateTime =
    
    [<Direct("return $str.replace(/(\w)\1*/g, $f)")>]
    let replace str f = X<string>

    let DateFormatter (date: System.DateTime) (f: string) =
        let d = date.JS
        let padLeft (minLength: int) (x: string) =
            if x.Length < minLength then
                String.replicate (minLength - x.Length) "0" + x
            else
                x
        replace f (fun (m: obj) ->
            let mString = m |> As<string>
            match mString.Substring(0, 1) with
            | "y" ->
                match mString.Length with
                | 1 -> d.GetFullYear() % 10 |> string 
                | 2 -> d.GetFullYear() % 100 |> string |> padLeft 2
                | 3 -> d.GetFullYear() % 1000 |> string |> padLeft 3
                | 4 -> d.GetFullYear() |> string |> padLeft 4
                | 5 -> d.GetFullYear() |> string |> padLeft 5
                | _ -> mString
            | "M" ->
                match mString.Length with
                | 1 -> d.GetMonth() + 1 |> string 
                | 2 -> d.GetMonth() + 1 |> string |> padLeft 2
                | _ -> mString
            | "d" ->
                match mString.Length with
                | 1 -> d.GetMonth() + 1 |> string 
                | 2 -> d.GetMonth() + 1 |> string |> padLeft 2
                | _ -> mString
            | "h" ->
                let hours = d.GetHours()
                match mString.Length with
                | 1 -> (if hours > 12 then hours % 12 else hours) |> string 
                | 2 -> (if hours > 12 then hours % 12 else hours) |> string |> padLeft 2
                | _ -> mString
            | "H" ->
                let hours = d.GetHours()
                match mString.Length with
                | 1 -> hours |> string 
                | 2 -> hours |> string |> padLeft 2
                | _ -> mString
            | "m" ->
                match mString.Length with
                | 1 -> d.GetMinutes() |> string 
                | 2 -> d.GetMinutes() |> string |> padLeft 2
                | _ -> mString
            | "s" ->
                match mString.Length with
                | 1 -> d.GetSeconds() |> string 
                | 2 -> d.GetSeconds() |> string |> padLeft 2
                | _ -> mString
            | "f" -> d.GetMilliseconds() |> string 
            | _ -> mString
        )
        

module Union =
// {{ generated by genInterop.fsx, do not modify
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice2 (x: Choice<'T1, 'T2>) = X<Union<'T1, 'T2>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice2 x =
        match x with
        | Union1Of2 v -> Choice1Of2 v
        | Union2Of2 v -> Choice2Of2 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice3 (x: Choice<'T1, 'T2, 'T3>) = X<Union<'T1, 'T2, 'T3>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice3 x =
        match x with
        | Union1Of3 v -> Choice1Of3 v
        | Union2Of3 v -> Choice2Of3 v
        | Union3Of3 v -> Choice3Of3 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice4 (x: Choice<'T1, 'T2, 'T3, 'T4>) = X<Union<'T1, 'T2, 'T3, 'T4>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice4 x =
        match x with
        | Union1Of4 v -> Choice1Of4 v
        | Union2Of4 v -> Choice2Of4 v
        | Union3Of4 v -> Choice3Of4 v
        | Union4Of4 v -> Choice4Of4 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice5 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice5 x =
        match x with
        | Union1Of5 v -> Choice1Of5 v
        | Union2Of5 v -> Choice2Of5 v
        | Union3Of5 v -> Choice3Of5 v
        | Union4Of5 v -> Choice4Of5 v
        | Union5Of5 v -> Choice5Of5 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice6 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice6 x =
        match x with
        | Union1Of6 v -> Choice1Of6 v
        | Union2Of6 v -> Choice2Of6 v
        | Union3Of6 v -> Choice3Of6 v
        | Union4Of6 v -> Choice4Of6 v
        | Union5Of6 v -> Choice5Of6 v
        | Union6Of6 v -> Choice6Of6 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice7 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice7 x =
        match x with
        | Union1Of7 v -> Choice1Of7 v
        | Union2Of7 v -> Choice2Of7 v
        | Union3Of7 v -> Choice3Of7 v
        | Union4Of7 v -> Choice4Of7 v
        | Union5Of7 v -> Choice5Of7 v
        | Union6Of7 v -> Choice6Of7 v
        | Union7Of7 v -> Choice7Of7 v
// }}

/// The computation expression for JavaScript Promises.
[<Inline>]
let promise = Promise.Builder()
