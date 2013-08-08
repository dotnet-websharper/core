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

module IntelliFactory.WebSharper.Core.Resources

open System
open System.IO
open System.Web
open System.Web.UI

module R = IntelliFactory.WebSharper.Core.Reflection

type MediaType =
    | Css
    | Js

let link (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("type", "text/css")
    html.AddAttribute("rel", "stylesheet")
    html.AddAttribute("href", url)
    html.RenderBeginTag "link"
    html.RenderEndTag()
    html.WriteLine()

let inlineStyle (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", "text/css")
    html.RenderBeginTag "style"
    html.Write(text)
    html.RenderEndTag()
    html.WriteLine()

let script (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("src", url)
    html.AddAttribute("type", "text/javascript")
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.RenderEndTag()

let inlineScript (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", "text/javascript")
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.Write(text)
    html.RenderEndTag()

type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

    member r.Emit(html: HtmlTextWriter, mt) =
        match r with
        | Rendering.RenderInline text ->
            match mt with
            | Css -> inlineStyle html text
            | Js -> inlineScript html text
        | Rendering.RenderLink url ->
            match mt with
            | Css -> link html url
            | Js -> script html url
        | Rendering.Skip -> ()

type Context =
    {
        DebuggingEnabled : bool
        GetAssemblyRendering : R.AssemblyName -> Rendering
        GetSetting : string -> option<string>
        GetWebResourceRendering : Type -> string -> Rendering
    }

let emit html (r: Rendering) mt =
    r.Emit(html, mt)

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
            match kind with
            | Basic spec ->
                let self = this.GetType()
                let assem = self.Assembly
                let aName = assem.GetName().Name
                let id = self.FullName
                let mt = if spec.EndsWith ".css" then Css else Js
                match ctx.GetSetting id with
                | Some url -> emit html (RenderLink url) mt
                | None ->
                    match tryFindWebResource self spec with
                    | Some e ->
                        emit html (ctx.GetWebResourceRendering self e) mt
                    | None ->
                        emit html (RenderLink spec) mt
            | Complex (b, xs) ->
                let id = this.GetType().FullName
                let b = defaultArg (ctx.GetSetting id) b
                let h = html
                for x in xs do
                    let url = b.TrimEnd [| '/' |] + "/" + x.TrimStart [| '/' |]
                    if url.EndsWith ".css" then link h url else script h url

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx html =
            let name = if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            let t = typeof<IntelliFactory.JavaScript.Core.Id>
            let ren = ctx.GetWebResourceRendering t name
            emit html ren Js
