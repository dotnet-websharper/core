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

module WebSharper.Core.Resources

open System
open System.IO
open System.Web
open System.Web.UI
module R = WebSharper.Core.Reflection
module CT = ContentTypes

type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

type Context =
    {
        DebuggingEnabled : bool
        DefaultToHttp : bool
        GetAssemblyRendering : R.AssemblyName -> Rendering
        GetSetting : string -> option<string>
        GetWebResourceRendering : Type -> string -> Rendering
    }

type MediaType =
    | Css
    | Js

let cleanLink dHttp (url: string) =
    if dHttp && url.StartsWith("//")
        then "http:" + url
        else url

let link dHttp (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("type", CT.Text.Css.Text)
    html.AddAttribute("rel", "stylesheet")
    html.AddAttribute("href", cleanLink dHttp url)
    html.RenderBeginTag "link"
    html.RenderEndTag()
    html.WriteLine()

let inlineStyle (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", CT.Text.Css.Text)
    html.RenderBeginTag "style"
    html.Write(text)
    html.RenderEndTag()
    html.WriteLine()

let script dHttp (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("src", cleanLink dHttp url)
    html.AddAttribute("type", CT.Text.JavaScript.Text)
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.RenderEndTag()

let inlineScript (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", CT.Text.JavaScript.Text)
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.Write(text)
    html.RenderEndTag()

type Rendering with
    member r.Emit(html: HtmlTextWriter, mt, ?defaultToHttp) =
        let dHttp = defaultArg defaultToHttp false
        match r with
        | Rendering.RenderInline text ->
            match mt with
            | Css -> inlineStyle html text
            | Js -> inlineScript html text
        | Rendering.RenderLink url ->
            match mt with
            | Css -> link dHttp html url
            | Js -> script dHttp html url
        | Rendering.Skip -> ()

let emit dHttp html (r: Rendering) mt =
    r.Emit(html, mt, dHttp)

type IResource =
    abstract Render : Context -> HtmlTextWriter -> unit

type Kind =
    | Basic of string
    | Complex of string * list<string>

let tryFindWebResource (t: Type) (spec: string) =
    let ok name = name = spec || (name.StartsWith spec && name.EndsWith spec)
    t.Assembly.GetManifestResourceNames()
    |> Seq.tryFind ok

[<AbstractClass>]
type BaseResource(kind: Kind) =

    new (spec: string) =
        new BaseResource(Basic spec)

    new (b: string, x: string, [<System.ParamArray>] xs: string []) =
        new BaseResource(Complex(b, x :: List.ofArray xs))

    interface IResource with
        member this.Render ctx html =
            let dHttp = ctx.DefaultToHttp
            match kind with
            | Basic spec ->
                let self = this.GetType()
                let assem = self.Assembly
                let aName = assem.GetName().Name
                let id = self.FullName
                let mt = if spec.EndsWith ".css" then Css else Js
                match ctx.GetSetting id with
                | Some url -> emit dHttp html (RenderLink url) mt
                | None ->
                    match tryFindWebResource self spec with
                    | Some e ->
                        emit dHttp html (ctx.GetWebResourceRendering self e) mt
                    | None ->
                        emit dHttp html (RenderLink spec) mt
            | Complex (b, xs) ->
                let id = this.GetType().FullName
                let b = defaultArg (ctx.GetSetting id) b
                let h = html
                for x in xs do
                    let url = b.TrimEnd [| '/' |] + "/" + x.TrimStart [| '/' |]
                    if url.EndsWith ".css" then link dHttp h url else script dHttp h url

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx html =
            let name = if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            let t = typeof<WebSharper.Core.JavaScript.Core.Id>
            let ren = ctx.GetWebResourceRendering t name
            emit ctx.DefaultToHttp html ren Js
