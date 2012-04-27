// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

[<IntelliFactory.WebSharper.Core.Attributes.NameAttribute "Option">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.OptionModule, \
     FSharp.Core, Version=2.0.0.0, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.OptionModuleProxy

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
