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

[<WebSharper.NameAttribute "Option">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Core.OptionModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.OptionModuleProxy

open WebSharper.JavaScript

[<Inline>]
let Bind f x =
    match x with
    | Some x -> f x
    | None   -> None

[<Inline>]
let Contains v o =
    match o with
    | Some x -> x = v
    | None -> false

[<Inline "$x ? 1 : 0">]
let Count (x: option<_>) = X<int>

[<Inline>]
let DefaultValue v o =
    match o with
    | Some x -> x 
    | None -> v

[<Inline>]
let DefaultWith f o =
    match o with
    | Some x -> x 
    | None -> f()

[<Inline>]
let Exists p x =
    match x with
    | Some x -> p x
    | None   -> false

[<Name "filter">]
let Filter f o =
    match o with
    | Some x when f x -> o
    | _ -> None

[<Inline>]
let Flatten o =
    match o with
    | Some x -> x
    | None -> None

[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (x: option<'T>) : 'S =
    match x with
    | Some x -> f s x
    | None   -> s

[<Name "foldBack">]
let FoldBack f x s =
    match x with
    | Some x -> f x s
    | None   -> s

[<Inline>]
let ForAll p x =
    match x with
    | Some x -> p x
    | None   -> true

[<Inline "$x.$0"; Pure>]
let GetValue (x: option<'T>) = X<'T>

[<Inline "$x==null">]
let IsNone (x: option<'T>) = false

[<Inline "$x!=null">]
let IsSome (x: option<'T>) =  false

[<Inline>]
let Iterate p x =
    match x with
    | Some x -> p x
    | None   -> ()

[<Inline>]
let Map f x =
    match x with
    | Some x -> Some (f x)
    | None -> None

[<Inline>]
let Map2 f x y =
    match x, y with
    | Some x, Some y -> Some (f x y)
    | _ -> None

[<Inline>]
let Map3 f x y z =
    match x, y, z with
    | Some x, Some y, Some z -> Some (f x y z)
    | _ -> None

[<Name "ofNullable">]
let OfNullable (o: System.Nullable<'T>) =
    if o ==. null then None else Some o.Value                   

[<Name "ofObj">]
let OfObj o = 
    if o ==. null then None else Some o

[<Inline>]
let OrElse v o =
    match o with
    | Some x -> o 
    | None -> v

[<Inline>]
let OrElseWith f o =
    match o with
    | Some x -> o 
    | None -> f()

[<Name "toArray">]
let ToArray x =
    match x with
    | Some x -> [|x|]
    | None   -> [||]

[<Name "toList">]
let ToList x =
    match x with
    | Some x -> [x]
    | None   -> []

[<Name "toNullable">]
let ToNullable o =
    match o with
    | Some v -> System.Nullable(v)
    | _ -> System.Nullable()

[<Name "toObj">]
let ToObj o = 
    match o with
    | Some v -> v
    | None -> null
