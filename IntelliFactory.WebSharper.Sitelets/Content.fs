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

namespace IntelliFactory.WebSharper.Sitelets

type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | PageContent of (Context<'Action> -> Page)

module Content =
    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Web
    open IntelliFactory.WebSharper

    module Activator = IntelliFactory.WebSharper.Html.Activator
    module M = IntelliFactory.WebSharper.Core.Metadata
    module R = IntelliFactory.WebSharper.Core.Reflection
    module J = IntelliFactory.WebSharper.Core.Json

    let metaJson<'T> (context: Context<'T>) (controls: seq<Control>) =
        let encode (c: Control) =
            let encoder = context.Json.GetEncoder(c.GetType())
            encoder.Encode c
        J.Encoded.Object [for c in controls -> (c.ID, encode c)]
        |> context.Json.Pack
        |> J.Stringify

    let escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    let toCustomContent genPage context : Http.Response =
        let htmlPage = genPage context
        let writeBody (stream: Stream) =
            // Finds all the client side controls on the page.
            let controls =
                htmlPage.Body
                |> Seq.collect (fun elem ->
                    elem.CollectAnnotations ())
            // Resolve resources for the set of types and this assembly
            let resources =
                controls
                |> Seq.map (fun x -> x.GetType())
                |> Seq.distinct
                |> Seq.map (fun t ->
                    M.Node.TypeNode (R.TypeDefinition.FromType t))
                |> context.Metadata.GetDependencies
            // Meta tag encoding the client side controls
            let mJson = metaJson context controls
            let renderHead (tw: UI.HtmlTextWriter) =
                // Render meta
                tw.WriteLine(
                    "<meta id='{0}' name='{0}' content='{1}' />",
                    IntelliFactory.WebSharper.Html.Activator.META_ID, 
                    escape mJson
                )
                // Render resources
                for r in resources do
                    r.Render context.ResourceContext tw
                let writer = new IntelliFactory.Html.Html.Writer(tw)
                for elem in htmlPage.Head do
                    writer.Write elem
                tw.WriteLine @"<script type='text/javascript'>"
                tw.WriteLine @"if (typeof IntelliFactory !=='undefined')"
                tw.WriteLine @"  IntelliFactory.Runtime.Start();"
                tw.WriteLine @"</script>"
            let renderBody (tw: UI.HtmlTextWriter) =
                let writer = new IntelliFactory.Html.Html.Writer(tw)
                for elem in htmlPage.Body do
                    writer.Write elem
            // Create html writer from stream
            use textWriter = new StreamWriter(stream)
            textWriter.AutoFlush <- true
            use htmlWriter =
                new System.Web.UI.HtmlTextWriter(textWriter)
            htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                renderHead renderBody htmlWriter
        {
            Status = Http.Status.Ok
            Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
            WriteBody = writeBody
        }

    let ToResponse (c: Content<'T>) (ctx: Context<'T>) =
        match c with
        | CustomContent x -> x ctx
        | PageContent genPage -> toCustomContent genPage ctx

    [<Obsolete>]
    let ToCustomContent (c: Content<'T>) =
        match c with
        | CustomContent _ -> c
        | PageContent genPage -> CustomContent (toCustomContent genPage)

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Content<'T>) =
        let genResp =
            match content with
            | CustomContent x -> x
            | PageContent genPage -> toCustomContent genPage
        CustomContent <| fun context -> f (genResp context)

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            {resp with Headers = headers})

    let SetStatus<'T> (status: Http.Status) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp -> {resp with Status = status})

    let Redirect<'T> (action: 'T) =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" (ctx.Link action)]
                WriteBody = ignore
            }

    let RedirectToUrl (url: string) =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }

    /// Constructs a status code response.
    let httpStatusContent<'T> status : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = ignore
            }

    let Unauthorized<'T> : Content<'T> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Content<'T> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Content<'T> =
        httpStatusContent Http.Status.NotFound

    let ServerError<'T> : Content<'T> =
        httpStatusContent Http.Status.InternalServerError
