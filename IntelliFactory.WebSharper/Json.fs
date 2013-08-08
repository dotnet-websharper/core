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

module IntelliFactory.WebSharper.Json

module A = IntelliFactory.WebSharper.Core.Attributes
module J = IntelliFactory.WebSharper.JavaScript
module Js = IntelliFactory.WebSharper.Core.Json
module Re = IntelliFactory.WebSharper.Core.Resources

type Resource() =
    interface Re.IResource with
        member this.Render ctx html =
            html.WriteLine "<!--[if lte IE 7.0]>"
            let name = if ctx.DebuggingEnabled then "Json.js" else "Json.min.js"
            let ren = ctx.GetWebResourceRendering typeof<Resource> name
            ren.Emit(html, Re.Js)
            html.WriteLine "<![endif]-->"

[<A.Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = J.ClientSide<'T>

[<A.Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = J.ClientSide<unit>

[<A.Inline "$x">]
let As<'T> (x: obj) = J.ClientSide<'T>

[<A.Inline "JSON.parse($json)">]
[<A.Require(typeof<Resource>)>]
let Parse (json: string) = J.ClientSide<obj>

[<A.Inline "JSON.stringify($obj)">]
[<A.Require(typeof<Resource>)>]
let Stringify (obj: obj) = J.ClientSide<string>

/// Lookups an object by its FQN.
[<A.JavaScript>]
let lookup<'T> (x: string []) : obj =
    let k = x.Length
    let mutable r = J.Global
    let mutable i = 0
    while i < k do
        let n  = x.[i]
        let rn = (?) r n
        if J.TypeOf rn <> J.Undefined then
            r <- rn
            i <- i + 1
        else
            failwith ("Invalid server reply. Failed to find type: " + n)
    r

/// Restores the type of a serialized object by field copying.
[<A.JavaScript>]
let restore (ty: obj) (obj: obj) : obj =
    let r = J.New ty
    J.ForEach obj (fun k -> (?<-) r k ((?) obj k); false)
    r

/// Does a shallow generic mapping over an object.
[<A.JavaScript>]
let shallowMap (f: obj -> obj) (x: obj) : obj =
    if J.InstanceOf x J.Global?Array then
        As (Array.map f (As x))
    else
        match J.TypeOf x with
        | J.Object ->
            let r = obj ()
            J.ForEach x (fun y -> (?<-) r y (f ((?) x y)); false)
            r
        | _ ->
            x

[<A.JavaScript>]
[<A.Require(typeof<Resource>)>]
let Activate<'T> (json: obj) : 'T =
    let types = As<obj[]> ((?) json "$TYPES")
    for i = 0 to types.Length - 1 do
        types.[i] <- lookup (As types.[i])
    let rec decode (x: obj) : obj =
        if x = null then x else
            match J.TypeOf x with
            | J.Object ->
                if J.InstanceOf x J.Global?Array then
                    shallowMap decode x
                else
                    let o  = shallowMap decode ((?) x "$V")
                    let ti = (?) x "$T"
                    if J.TypeOf ti = J.Kind.Undefined then o else
                        restore types.[ti] o
            | _ ->
                x
    As (decode ((?) json "$DATA"))

