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

module WebSharper.Json

open WebSharper.JavaScript
module Js = WebSharper.Core.Json
module Re = WebSharper.Core.Resources

type Resource() =
    interface Re.IResource with
        member this.Render ctx =
            let name = if ctx.DebuggingEnabled then "Json.js" else "Json.min.js"
            let ren = Re.Rendering.GetWebResourceRendering(ctx, typeof<Resource>, name)
            fun html ->
                let html = html Re.Scripts
                html.WriteLine "<!--[if lte IE 7.0]>"
                ren.Emit(html, Re.Js)
                html.WriteLine "<![endif]-->"

[<Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = X<'T>

[<Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = X<unit>

[<Inline "$x">]
let As<'T> (x: obj) = X<'T>

[<Inline "JSON.parse($json)">]
[<Require(typeof<Resource>)>]
let Parse (json: string) = X<obj>

[<Inline "JSON.stringify($obj)">]
[<Require(typeof<Resource>)>]
let Stringify (obj: obj) = X<string>

/// Lookups an object by its FQN.
[<JavaScript>]
let lookup<'T> (x: string []) : obj =
    let mutable r = JS.Global
    // first item is the module name, skip it for now
    for i = 1 to x.Length - 1 do
        let n  = x.[i]
        let rn = (?) r n
        if JS.TypeOf rn <> JS.Undefined then
            r <- rn
        else
            failwith ("Invalid server reply. Failed to find type: " + n)
    r

/// Does a shallow generic mapping over an object.
[<JavaScript>]
let shallowMap (f: obj -> obj) (x: obj) : obj =
    if x :? System.Array then
        As (Array.map f (As x))
    else
        match JS.TypeOf x with
        | JS.Object ->
            let r = New []
            JS.ForEach x (fun y -> (?<-) r y (f ((?) x y)); false)
            r
        | _ ->
            x

[<JavaScript>]
let listConstructor = []?constructor;

[<JavaScript>]
[<Require(typeof<Resource>)>]
let Activate<'T> (json: obj) : 'T =
    let types = As<obj[]> ((?) json "$TYPES")
    for i = 0 to types.Length - 1 do
        types.[i] <- lookup (As types.[i])
    let rec decode (x: obj) : obj =
        if x = null then x else
            match JS.TypeOf x with
            | JS.Object ->
                if x :? System.Array then
                    shallowMap decode x
                else
                    let o  = shallowMap decode ((?) x "$V")
                    let ti = (?) x "$T"
                    if JS.TypeOf ti = JS.Kind.Undefined then o else
                        let t = types.[ti]
                        if t ===. listConstructor then
                            box (List.ofArray (As<obj[]> o))
                        else
                            let r = JS.New t
                            JS.ForEach o (fun k -> (?<-) r k ((?) o k); false)
                            r
            | _ ->
                x
    As (decode ((?) json "$DATA"))

