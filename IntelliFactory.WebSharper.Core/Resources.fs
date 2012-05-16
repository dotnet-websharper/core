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

module IntelliFactory.WebSharper.Core.Resources

module R = IntelliFactory.WebSharper.Core.Reflection

type Context =
    {
        DebuggingEnabled    : bool
        GetAssemblyUrl      : R.AssemblyName -> string
        GetSetting          : string -> option<string>
        GetWebResourceUrl   : System.Type -> string -> string
    }

type IResource =
    abstract member Render : Context -> System.Web.UI.HtmlTextWriter -> unit

type Kind =
    | Basic of string
    | Complex of string * list<string>

let link (html: System.Web.UI.HtmlTextWriter) (url: string) =
    html.AddAttribute("type", "text/css")
    html.AddAttribute("rel", "stylesheet")
    html.AddAttribute("href", url)
    html.RenderBeginTag "link"
    html.RenderEndTag()
    html.WriteLine()

let script (html: System.Web.UI.HtmlTextWriter) (url: string) =
    html.AddAttribute("src", url)
    html.AddAttribute("type", "text/javascript")
    html.RenderBeginTag "script"
    html.RenderEndTag()

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
                let ok (name: string) =
                    name = spec
                    || name.StartsWith aName
                    && name.EndsWith spec
                let id  = self.FullName
                let url =
                    match ctx.GetSetting id with
                    | Some url -> url
                    | None ->
                        let embed =
                            assem.GetManifestResourceNames()
                            |> Seq.tryFind ok
                        match embed with
                        | Some e -> ctx.GetWebResourceUrl self e
                        | _ -> spec
                if spec.EndsWith ".css" then link html url else script html url
            | Complex (b, xs) ->
                let id = this.GetType().FullName
                let b = defaultArg (ctx.GetSetting id) b
                let h = html
                for x in xs do
                    let url = b + x
                    if url.EndsWith ".css" then link h url else script h url

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx html =
            if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            |> ctx.GetWebResourceUrl typeof<IntelliFactory.JavaScript.Core.Id>
            |> script html
