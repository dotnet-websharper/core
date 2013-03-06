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

/// Implements generic comparison, equality and hashing.
[<IntelliFactory.WebSharper.Core.Attributes.Name "Unchecked">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.Operators+Unchecked, \
     FSharp.Core, Version=2.0.0.0, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.UncheckedProxy

module J = IntelliFactory.WebSharper.JavaScript

[<Inline "$a.CompareTo($b)">]
let compareTo (a: obj) (b: obj) = X<int>

[<Inline "$a instanceof Array">]
let isArray (a: obj) = X<bool>

[<JavaScript>]
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

/// Compares two values generically.
[<JavaScript>]
let Compare<'T> (a: 'T) (b: 'T) : int =
    if a ===. b then 0 else
        match J.TypeOf a with
        | J.Undefined ->
            match J.TypeOf b with
            | J.Undefined -> 0
            | _           -> -1
        | J.Function ->
            failwith "Cannot compare function values."
        | J.Boolean | J.Number | J.String ->
            if a <. b then -1 else 1
        | _ ->
            if a ===. null              then -1
            elif b ===. null            then 1
            elif J.In "CompareTo" a     then compareTo a b
            elif isArray a && isArray b then compareArrays (As a) (As b)
            else compareArrays (As (J.GetFields a)) (As (J.GetFields b))

/// Produces an undefined value.
[<Inline "undefined">]
let DefaultOf<'T> = X<'T>

[<Inline "$a.Equals($b)">]
let private equals (a: obj) (b: obj) = X<bool>

[<JavaScript>]
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

/// Tests if two values are equal.
[<JavaScript>]
let rec Equals (a: 'T) (b: 'T) : bool =
    if a ===. b then true else
        match J.TypeOf a with
        | J.Object ->
            if a ===. null              then false
            elif b ===. null            then false
            elif J.In "Equals" a        then equals a b
            elif isArray a && isArray b then arrayEquals (As a) (As b)
            else arrayEquals (As (J.GetFields a)) (As (J.GetFields b))
        | _ ->
            false

[<JavaScript>]
let hashMix (x: int) (y: int) : int =
    (x <<< 5) + x + y

[<Inline "$o.GetHashCode()">]
let getHashCode(o: obj) = X<int>

[<JavaScript>]
let hashArray (o: obj []) =
    let mutable h = -34948909
    for i in 0 .. o.Length - 1 do
        h <- hashMix h (Unchecked.hash o.[i])
    h

[<JavaScript>]
let hashString (s: string) : int =
    if s ===. null then 0 else
        let mutable hash = 5381
        for i = 0 to s.Length - 1 do
            hash <- hashMix hash (int s.[i])
        hash

[<JavaScript>]
let hashObject (o: obj) =
    if J.In "GetHashCode" o then getHashCode o else
        let (++) = hashMix
        let h = ref 0
        J.ForEach o (fun key ->
            h := !h ++ hashString key ++ Unchecked.hash ((?) o key)
            false)
        !h

/// Computes the hash of an object.
[<JavaScript>]
let Hash<'T> (o: 'T) : int =
    match J.TypeOf o with
    | J.Undefined -> 0
    | J.Function  -> 0
    | J.Boolean   -> if As o then 1 else 0
    | J.Number    -> As o
    | J.String    -> hashString (As o)
    | J.Object    -> if o ==. null then 0
                     elif isArray o then hashArray (As o)
                     else hashObject o

