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

/// Implements generic comparison, equality and hashing.
[<WebSharper.Name "Unchecked">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Core.Operators+Unchecked, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.UncheckedProxy

open WebSharper.JavaScript

[<Inline "$a instanceof Array">]
let isArray (a: obj) = X<bool>

[<Inline "$a instanceof Date">]
let isDate (a: obj) = X<bool>

let rec compareArrays (a: obj []) (b: obj []) =
    if a.Length < b.Length   then -1
    elif a.Length > b.Length then 1
    else
        let mutable cmp = 0
        let mutable i = 0
        while cmp = 0 && i < a.Length do
            cmp <- Unchecked.compare a.[i] b.[i]
            i <- i + 1
        cmp

[<Inline "$d.getTime()">]
let getTime (d: obj) : int = X

let rec compareDates (a: obj) (b: obj) =
    compare (getTime a) (getTime b)

/// Compares two values generically.
let Compare<'T> (a: 'T) (b: 'T) : int =
    let objCompare (a: obj) (b: obj) =
        let cmp = ref 0
        JS.ForEach a (fun k ->
            if not (JS.HasOwnProperty a k) then
                false
            elif not (JS.HasOwnProperty b k) then
                cmp := 1; true
            else
                cmp := Unchecked.compare a?(k) b?(k); !cmp <> 0)
        if !cmp = 0 then
            JS.ForEach b (fun k ->
                if not (JS.HasOwnProperty b k) then
                    false
                elif not (JS.HasOwnProperty a k) then
                    cmp := -1; true
                else false)
        !cmp
    if a ===. b then 0 else
        match JS.TypeOf a with
        | JS.Undefined ->
            match JS.TypeOf b with
            | JS.Undefined -> 0
            | _ -> -1
        | JS.Function ->
            failwith "Cannot compare function values."
        | JS.Boolean | JS.Number | JS.String ->
            if a <. b then -1 else 1
        | JS.Object ->
            if a ===. null then -1
            elif b ===. null then 1
            elif JS.In "CompareTo" a then (As<System.IComparable<_>> a).CompareTo(b)
            elif JS.In "CompareTo0" a then (As<System.IComparable> a).CompareTo(b)
            elif isArray a && isArray b then compareArrays (As a) (As b)
            elif isDate a && isDate b then compareDates a b
            else objCompare (As a) (As b)

/// Produces an undefined value.
[<Macro(typeof<Macro.DefaultOf>)>]
[<Inline "undefined">]
let DefaultOf<'T> = X<'T>

let arrayEquals (a: obj []) (b: obj []) =
    if a.Length = b.Length then
        let mutable eq = true
        let mutable i = 0
        while eq && i < a.Length do
            if not (Unchecked.equals a.[i] b.[i]) then
                eq <- false
            i <- i + 1
        eq
    else
        false

let dateEquals a b =
    getTime a ===. getTime b

[<Inline "$a.Equals($b)">]
let private equals (a: obj) (b: obj) = X<bool>

/// Tests if two values are equal.
let Equals (a: 'T) (b: 'T) : bool =
    let objEquals (a: obj) (b: obj) =
        let eqR = ref true
        JS.ForEach a (fun k ->
            eqR := not (JS.HasOwnProperty a k) || JS.HasOwnProperty b k && Unchecked.equals a?(k) b?(k)
            not !eqR)
        if !eqR then
            JS.ForEach b (fun k ->
                eqR := not (JS.HasOwnProperty b k) || JS.HasOwnProperty a k
                not !eqR)
        !eqR
    if a ===. b then true else
        match JS.TypeOf a with
        | JS.Object ->
            if a ===. null || a ===. JS.Undefined || b ===. null || b ===. JS.Undefined then false
            elif JS.In "Equals" a then equals a b
            elif isArray a && isArray b then arrayEquals (As a) (As b)
            elif isDate a && isDate b then dateEquals a b
            else objEquals (As a) (As b)
        | JS.Function ->
            if JS.In "$Func" a then
                a?``$Func`` ===. b?``$Func`` && a?``$Target`` ===. b?``$Target``
            elif JS.In "$Invokes" a && JS.In "$Invokes" b then
                arrayEquals a?``$Invokes`` b?``$Invokes``  
            else false
        | _ ->
            false

let hashMix (x: int) (y: int) : int =
    (x <<< 5) + x + y

let hashArray (o: obj []) =
    let mutable h = -34948909
    for i in 0 .. o.Length - 1 do
        h <- hashMix h (Unchecked.hash o.[i])
    h

let hashString (s: string) : int =
    if s ===. null then 0 else
        let mutable hash = 5381
        for i = 0 to s.Length - 1 do
            hash <- hashMix hash (int s.[i])
        hash

[<Inline "$o.GetHashCode()">]
let getHashCode(o: obj) = X<int>

let hashObject (o: obj) =
    if JS.In "GetHashCode" o then getHashCode o else
        let (++) = hashMix
        let h = ref 0
        JS.ForEach o (fun key ->
            h := !h ++ hashString key ++ Unchecked.hash ((?) o key)
            false)
        !h

/// Computes the hash of an object.
let Hash<'T> (o: 'T) : int =
    match JS.TypeOf o with
    | JS.Undefined -> 0
    | JS.Function  -> 0
    | JS.Boolean   -> if As o then 1 else 0
    | JS.Number    -> As o
    | JS.String    -> hashString (As o)
    | JS.Object    -> if o ==. null then 0
                      elif isArray o then hashArray (As o)
                      else hashObject o

