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
        member this.Render ctx html =
            let html = html Re.Scripts
            html.WriteLine "<!--[if lte IE 7.0]>"
            let name = if ctx.DebuggingEnabled then "Json.js" else "Json.min.js"
            let ren = Re.Rendering.GetWebResourceRendering(ctx, typeof<Resource>, name)
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
    let k = x.Length
    let mutable r = JS.Global
    let mutable i = 0
    while i < k do
        let n  = x.[i]
        let rn = (?) r n
        if JS.TypeOf rn <> JS.Undefined then
            r <- rn
            i <- i + 1
        else
            // The class is not present on the client,
            // it could have been pruned by the bundling.
            r <- JS.Undefined
            i <- k
    r

/// Does a shallow generic mapping over an object.
[<JavaScript>]
let shallowMap (f: obj -> obj) (x: obj) : obj =
    if JS.InstanceOf x JS.Global?Array then
        As (Array.map f (As x))
    else
        match JS.TypeOf x with
        | JS.Object ->
            let r = obj ()
            JS.ForEach x (fun y -> (?<-) r y (f ((?) x y)); false)
            r
        | _ ->
            x

[<JavaScript>]
[<Require(typeof<Resource>)>]
let Activate<'T> (json: obj) : 'T =
    let types : obj[] = json?("$TYPES")
    for i = 0 to types.Length - 1 do
        types.[i] <- lookup (As types.[i])
    let rec decode (x: obj) : obj =
        if x = null then x else
            match JS.TypeOf x with
            | JS.Object ->
                if JS.InstanceOf x JS.Global?Array then
                    shallowMap decode x
                else
                    let o = shallowMap decode (x?("$V"))
                    let ti = x?("$T")
                    if ti ===. JS.Undefined
                        || (As<Array<obj>> types).[ti] ===. JS.Undefined
                    then o
                    else
                        let r = JS.New (As<Array<obj>> types).[ti]
                        JS.ForEach o (fun k -> r?(k) <- o?(k); false)
                        r
            | _ ->
                x
    As (decode (json?("$DATA")))

