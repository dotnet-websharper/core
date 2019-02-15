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

[<WebSharper.NameAttribute "ValueOption">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Core.ValueOption, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ValueOptionModuleProxy

open WebSharper.JavaScript

[<Inline>]
let Bind f x =
    match x with
    | ValueSome x -> f x
    | ValueNone   -> ValueNone

[<Inline>]
let Contains v o =
    match o with
    | ValueSome x -> x = v
    | ValueNone -> false

[<Inline "$x.$">]
let Count (x: voption<_>) = X<int>

[<Inline>]
let DefaultValue v o =
    match o with
    | ValueSome x -> x 
    | ValueNone -> v

[<Inline>]
let DefaultWith f o =
    match o with
    | ValueSome x -> x 
    | ValueNone -> f()

[<Inline>]
let Exists p x =
    match x with
    | ValueSome x -> p x
    | ValueNone   -> false

[<Name "filter">]
let Filter f o =
    match o with
    | ValueSome x when f x -> o
    | _ -> ValueNone

[<Inline>]
let Flatten o =
    match o with
    | ValueSome x -> x
    | ValueNone -> ValueNone

[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (x: voption<'T>) : 'S =
    match x with
    | ValueSome x -> f s x
    | ValueNone   -> s

[<Name "foldBack">]
let FoldBack f x s =
    match x with
    | ValueSome x -> f x s
    | ValueNone   -> s

[<Inline>]
let ForAll p x =
    match x with
    | ValueSome x -> p x
    | ValueNone   -> true

[<Inline "$x.$0"; Pure>]
let GetValue (x: voption<'T>) = X<'T>

[<Inline "$x.$ == 0">]
let IsNone (x: voption<'T>) = false

[<Inline "$x.$ == 1">]
let IsSome (x: voption<'T>) =  false

[<Inline>]
let Iterate p x =
    match x with
    | ValueSome x -> p x
    | ValueNone   -> ()

[<Inline>]
let Map f x =
    match x with
    | ValueSome x -> ValueSome (f x)
    | ValueNone -> ValueNone

[<Inline>]
let Map2 f x y =
    match x, y with
    | ValueSome x, ValueSome y -> ValueSome (f x y)
    | _ -> ValueNone

[<Inline>]
let Map3 f x y z =
    match x, y, z with
    | ValueSome x, ValueSome y, ValueSome z -> ValueSome (f x y z)
    | _ -> ValueNone

[<Name "ofNullable">]
let OfNullable (o: System.Nullable<'T>) =
    if o ==. null then ValueNone else ValueSome o.Value                   

[<Name "ofObj">]
let OfObj o = 
    if o ==. null then ValueNone else ValueSome o

[<Inline>]
let OrElse v o =
    match o with
    | ValueSome x -> o 
    | ValueNone -> v

[<Inline>]
let OrElseWith f o =
    match o with
    | ValueSome x -> o 
    | ValueNone -> f()

[<Name "toArray">]
let ToArray x =
    match x with
    | ValueSome x -> [|x|]
    | ValueNone   -> [||]

[<Name "toList">]
let ToList x =
    match x with
    | ValueSome x -> [x]
    | ValueNone   -> []

[<Name "toNullable">]
let ToNullable o =
    match o with
    | ValueSome v -> System.Nullable(v)
    | _ -> System.Nullable()

[<Name "toObj">]
let ToObj o = 
    match o with
    | ValueSome v -> v
    | ValueNone -> null
