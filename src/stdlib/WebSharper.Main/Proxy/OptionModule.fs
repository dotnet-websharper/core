// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

[<WebSharper.Core.Attributes.NameAttribute "Option">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.OptionModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.OptionModuleProxy

open WebSharper.JavaScript

[<JavaScript>]
[<Name "bind">]
let Bind f x =
    match x with
    | Some x -> f x
    | None   -> None

[<Inline "$x.$">]
let Count (x: option<_>) = X<int>

[<JavaScript>]
[<Name "exists">]
let Exists p x =
    match x with
    | Some x -> p x
    | None   -> false

[<JavaScript>]
[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (x: option<'T>) : 'S =
    match x with
    | Some x -> f s x
    | None   -> s

[<JavaScript>]
[<Name "foldBack">]
let FoldBack f x s =
    match x with
    | Some x -> f x s
    | None   -> s

[<JavaScript>]
[<Name "forall">]
let ForAll p x =
    match x with
    | Some x -> p x
    | None   -> true

[<Inline "$x.$0">]
let GetValue (x: option<'T>) = X<'T>

[<Inline "$x.$==0">]
let IsNone (x: option<'T>) = false

[<Inline "$x.$==1">]
let IsSome (x: option<'T>) =  false

[<JavaScript>]
[<Name "iter">]
let Iterate p x =
    match x with
    | Some x -> p x
    | None   -> ()

[<JavaScript>]
[<Name "map">]
let Map f x =
    match x with
    | Some x    -> Some (f x)
    | None      -> None

[<JavaScript>]
[<Name "toArray">]
let ToArray x =
    match x with
    | Some x -> [|x|]
    | None   -> [||]

[<JavaScript>]
[<Name "toList">]
let ToList x =
    match x with
    | Some x -> [x]
    | None   -> []

[<JavaScript>]
[<Name "ofObj">]
let OfObj o = 
    if o ==. null then None else Some o

[<JavaScript>]
[<Name "toObj">]
let ToObj o = 
    match o with
    | Some v -> v
    | None -> null

[<JavaScript>]
[<Name "filter">]
let Filter f o =
    match o with
    | None -> None
    | Some v -> if f v then Some v else None