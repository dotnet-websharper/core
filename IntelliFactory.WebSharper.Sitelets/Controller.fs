// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

open System.Collections.Generic
open System.Web.UI
open IntelliFactory.Html
open IntelliFactory.WebSharper

type private Writer = HtmlTextWriter -> unit
type private Control = IntelliFactory.WebSharper.Web.Control

/// Represents HTML pages with embedded WebSharper controls.
type Page =
    {
        Doctype : option<string>
        Title : option<string>
        Renderer : option<string> -> option<string> -> Writer -> Writer ->
            HtmlTextWriter -> unit
        Head : seq<Element<unit>>
        Body : seq<Element<Control>>
    }

    static member Default =
        let renderer (doctype : option<string>) (title: option<string>)
            writeHead writeBody (writer: System.Web.UI.HtmlTextWriter) =
            // Doctype
            match doctype with
            | Some dt -> writer.WriteLine dt
            | None -> ()
            writer.RenderBeginTag HtmlTextWriterTag.Html
            // Head section
            writer.RenderBeginTag HtmlTextWriterTag.Head
            writer.WriteLine()
            writeHead writer
            match title with
            | Some t ->
                writer.RenderBeginTag HtmlTextWriterTag.Title
                writer.Write t
                writer.RenderEndTag ()
            | None -> ()
            writer.RenderEndTag()
            // Body section
            writer.RenderBeginTag HtmlTextWriterTag.Body
            writer.WriteLine()
            writeBody writer
            writer.RenderEndTag()
            writer.RenderEndTag()
        {
            Doctype = Some "<!DOCTYPE html \
                PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \
                \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
            Title = None
            Head = []
            Renderer = renderer
            Body = []
        }

/// Provides services available to handlers at run-time.
type Context<'Action> =
    {

        /// Application path
        ApplicationPath : string

        /// Generates a (possibly relative) URL to a given action.
        Link : 'Action -> string

        /// The typed JSON provider for interacting with the client.
        Json : IntelliFactory.WebSharper.Core.Json.Provider

        /// WebSharper metadata required for serializing controls.
        Metadata : IntelliFactory.WebSharper.Core.Metadata.Info

        // Generates a URL respecting the application path.
        ResolveUrl : string -> string

        /// WebSharper resource rendering context required for resources.
        ResourceContext : IntelliFactory.WebSharper.Core.Resources.Context

        /// HTTP Request object
        Request : Http.Request
    }

/// Represents server responses to actions. The Page response is special-cased
/// for combinators to have access to it.
type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | PageContent of (Context<'Action> -> Page)

module Content =
    open System.Text.RegularExpressions
    open System.IO
    open IntelliFactory.WebSharper
    open System.Web

    module Activator = IntelliFactory.WebSharper.Html.Activator
    module M = IntelliFactory.WebSharper.Core.Metadata
    module R = IntelliFactory.WebSharper.Core.Reflection
    module J = IntelliFactory.WebSharper.Core.Json

    let private metaJson<'T> (context: Context<'T>) (controls: seq<Control>) =
        let encode (c: Control) =
            let encoder = context.Json.GetEncoder(c.GetType())
            encoder.Encode c
        J.Encoded.Object [for c in controls -> (c.ID, encode c)]
        |> context.Json.Pack
        |> J.Stringify

    let private escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    let ToCustomContent (content: Content<'Action>) : Content<'Action> =
        match content with
        | CustomContent _ as cc -> cc
        | PageContent genPage ->
            CustomContent <| fun context ->
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

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    let MapResponse (f: Http.Response -> Http.Response)
        (content: Content<'Action>) : Content<'Action> =
        match ToCustomContent content with
        | CustomContent genResp ->
            CustomContent <| fun context -> f (genResp context)
        | _ -> content

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    let WithHeaders (headers: seq<Http.Header>)
        : Content<'Action> -> Content<'Action> =
        MapResponse <| fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            {resp with Headers = headers}

    /// Sets the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    let SetStatus (status: Http.Status)
        : Content<'Action> -> Content<'Action> =
        MapResponse <| fun resp ->
            {resp with Status = status}

    /// Redirects to a given action.
    let Redirect (action: 'Acton) =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" (ctx.Link action)]
                WriteBody = ignore
            }

    /// Redirects to a given action.
    let RedirectToUrl (url: string) =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }

    /// Constructs a 500 Server Error response.
    let private HttpStatusContent<'Action> status : Content<'Action> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = ignore
            }

    /// Constructs a 401 Unauthorized response.
    let Unauthorized<'Action> : Content<'Action> =
        HttpStatusContent Http.Status.Unauthorized

    /// Constructs a 403 Forbidden response.
    let Forbidden<'Action> : Content<'Action> =
        HttpStatusContent Http.Status.Forbidden

    /// Constructs a 404 Not Found response.
    let NotFound<'Action> : Content<'Action> =
        HttpStatusContent Http.Status.NotFound

    /// Constructs a 500 Server Error response.
    let ServerError<'Action> : Content<'Action> =
        HttpStatusContent Http.Status.InternalServerError

/// Represents a controller that matches actions to responses.
type Controller<'Action> =
    {
        Handle : 'Action -> Content<'Action>
    }
