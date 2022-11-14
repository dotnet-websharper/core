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
let lookup<'T> (x: string) : Async<'T> =
    let lookupFrom (root: obj) (x: string[]) =
        let k = x.Length
        let mutable r = root // JS.Global
        let mutable i = 0
        while i < k do
            let n  = x.[i]
            let rn = (?) r n
            if JS.TypeOf rn <> JS.Undefined then
                r <- rn
                i <- i + 1
            else
                failwith ("Invalid server reply. Failed to find type: " + n)
        r |> As<'T>
        
    match x.IndexOf("::") with
    | -1 ->
        lookupFrom JS.Global (x.Split('.')) |> async.Return
    | i ->
        let m = x[..i-1]
        let x = x[i+2..]
        async {
            let! mi = JS.ImportDynamic(m).AsAsync()
            return lookupFrom mi (x.Split('.'))
        }

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

type SpecialTypes =
    | List = 1
    | Decimal = 2

[<JavaScript>]
[<Require(typeof<Resource>)>]
let Activate<'T> (json: obj) : Async<'T> =
    let types = if As json then json?("$TYPES") : obj[] else JS.Undefined
    let typeLoads = JavaScript.Array<Async<unit>>()
    let data =
        if types ===. JS.Undefined then
            json |> async.Return
        else
            for i = 0 to types.Length - 1 do
                match As<string> types.[i] with
                | "../WebSharper.Main/Microsoft.FSharp.Collections.FSharpList`1.js::default" -> 
                    types.[i] <- box SpecialTypes.List
                | "WebSharper.Decimal" -> 
                    types.[i] <- box SpecialTypes.Decimal
                | t -> 
                    typeLoads.Push (async { 
                        let! c = lookup t
                        types.[i] <- c
                    }) |> ignore
            json?("$DATA")
    let rec decode (x: obj) : obj =
        if x = null then x else
            match JS.TypeOf x with
            | JS.Object ->
                if x :? System.Array then
                    shallowMap decode x
                else
                    let o  = shallowMap decode (x?("$V"))
                    let ti = x?("$T")
                    if ti ===. JS.Undefined then o else
                        let t = types.[ti]
                        if t ===. SpecialTypes.List then
                            box (List.ofArray (As<obj[]> o))
                        elif t ===. SpecialTypes.Decimal then
                            box (JS.Global?WebSharper?Decimal?CreateDecimalBits(o))
                        else
                            let r = JS.New types.[ti]
                            JS.ForEach o (fun k -> (?<-) r k ((?) o k); false)
                            r
            | _ ->
                x
    async {
        do! Async.Parallel (typeLoads.Self) |> Async.Ignore
        return As (decode data)
    }
    

