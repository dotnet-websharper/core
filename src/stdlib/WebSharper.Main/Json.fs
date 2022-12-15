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
let lookupFrom (root: obj) (x: string[]) =
    let k = x.Length
    let mutable r = root
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

[<JavaScript>]
let splitAddress (s: string) =
    s.Split([| "." |], System.StringSplitOptions.RemoveEmptyEntries)   

/// Lookups an object by full address string.
[<JavaScript>]
let lookup<'T> (x: string) : Async<'T> =
    match x.IndexOf("::") with
    | -1 ->
        lookupFrom JS.Global (splitAddress x) |> async.Return
    | i ->
        let m = x[..i-1]
        let x = x[i+2..]
        async {
            let! mi = JS.ImportDynamic(m).AsAsync()
            return lookupFrom mi (splitAddress x)
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

[<JavaScript; RequireQualifiedAccess; Prototype(false)>]
type private SpecialTypes =
    | List 
    | Decimal of (obj -> obj)
    | Object of obj
    | JSModule of obj

[<JavaScript>]
let rec private decode (types: SpecialTypes[]) (x: obj) : obj =
    if x = null then x else
        match JS.TypeOf x with
        | JS.Object ->
            if x :? System.Array then
                shallowMap (decode types) x
            else
                let o  = shallowMap (decode types) (x?("$V"))
                let ti = x?("$T")
                if ti ===. JS.Undefined then o else
                    let t = types.[ti]
                    match t with
                    | SpecialTypes.List ->  
                        box (List.ofArray (As<obj[]> o))
                    | SpecialTypes.Decimal c ->  
                        Console.Log("Creating client side decimal", c, o)
                        c(o)
                    | SpecialTypes.Object ctor ->  
                        Object.Assign(JS.New ctor, o)
                    | SpecialTypes.JSModule m ->
                        m
        | _ ->
            x


[<JavaScript>]
[<Require(typeof<Resource>)>]
let ActivateAsync<'T> (json: obj) : Async<'T> =
    let types = if As json then json?("$TYPES") : SpecialTypes[] else JS.Undefined
    let typeLoads = JavaScript.Array<Async<unit>>()
    let data =
        if types ===. JS.Undefined then
            json
        else
            for i = 0 to types.Length - 1 do
                match As<string> types.[i] with
                | "../WebSharper.Main/Microsoft.FSharp.Collections.FSharpList`1.js::default" -> 
                    types.[i] <- SpecialTypes.List
                | "../WebSharper.MathJS.Extensions/WebSharper.Decimal.js::" -> 
                    typeLoads.Push (async { 
                        let! c = lookup "../WebSharper.MathJS.Extensions/WebSharper.Decimal.js::CreateDecimalBits"
                        types.[i] <- SpecialTypes.Decimal c
                    }) |> ignore
                | t -> 
                    if t.EndsWith "::" then
                        typeLoads.Push (async { 
                            let! c = lookup t
                            types.[i] <- SpecialTypes.JSModule c
                        }) |> ignore
                    else
                        typeLoads.Push (async { 
                            let! c = lookup t
                            types.[i] <- SpecialTypes.Object c
                        }) |> ignore
            json?("$DATA")
    async {
        do! Async.Parallel (typeLoads.Self) |> Async.Ignore
        return As (decode types data)
    }
    
[<JavaScript>]
[<Require(typeof<Resource>)>]
let Activate<'T> (json: obj) (imported: obj[]) : 'T =
    let types = if As json then json?("$TYPES") : SpecialTypes[] else JS.Undefined
    let data =
        if types ===. JS.Undefined then
            json
        else
            for i = 0 to types.Length - 1 do
                match As<string> types.[i] with
                | "../WebSharper.Main/Microsoft.FSharp.Collections.FSharpList`1.js::default" -> 
                    types.[i] <- SpecialTypes.List
                | "../WebSharper.MathJS.Extensions/WebSharper.Decimal.js::" -> 
                    types.[i] <- SpecialTypes.Decimal (lookupFrom (imported.[i]) [| "CreateDecimalBits" |])
                | t -> 
                    let fn =
                        match t.IndexOf("::") with
                        | -1 ->
                            splitAddress t
                        | i ->
                            let m = t[..i-1]
                            let x = t[i+2..]
                            splitAddress x
                    if Array.isEmpty fn then
                        types.[i] <- SpecialTypes.JSModule (imported.[i])
                    else
                        types.[i] <- SpecialTypes.Object (lookupFrom (imported.[i]) fn)
            json?("$DATA")
    As (decode types data)