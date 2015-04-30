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

namespace WebSharper.Sitelets

module CT = WebSharper.Core.ContentTypes

type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | CustomContentAsync of (Context<'Action> -> Async<Http.Response>)
    | PageContent of (Context<'Action> -> Page)
    | PageContentAsync of (Context<'Action> -> Async<Page>)

module Content =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open System.Web
    open WebSharper

    type private Func<'A,'B> = System.Func<'A,'B>

    module Activator = WebSharper.Html.Client.Activator
    module M = WebSharper.Core.Metadata
    module R = WebSharper.Core.Reflection
    module J = WebSharper.Core.Json
    module XS = IntelliFactory.Xml.SimpleXml
    module XT = IntelliFactory.Xml.Templating
    module H = WebSharper.Html.Server.Html

    let metaJson<'T> (jP: Core.Json.Provider) (controls: seq<IControl>) =
        let encode (c: IControl) =
            let encoder = jP.GetEncoder(c.GetType())
            encoder.Encode c
        J.Encoded.Object [for c in controls -> (c.Id, encode c)]
        |> jP.Pack
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

    /// Watches a file for changes.
    let watchForChanges (path: string) (recompile: unit -> unit) =
        let dir = Path.GetDirectoryName(path)
        let file = Path.GetFileName(path)
        let watcher =
            new FileSystemWatcher(dir, file,
                EnableRaisingEvents = true,
                NotifyFilter = (NotifyFilters.LastWrite ||| NotifyFilters.Security))
        watcher.Changed.Add(fun _ -> recompile ())
        watcher :> System.IDisposable

    type Env =
        {
            AppPath : string
            Json : Core.Json.Provider
            Meta : Core.Metadata.Info
            ResourceContext : Core.Resources.Context
        }

        static member Create<'T>(ctx: Context<'T>) =
            {
                AppPath = ctx.ApplicationPath
                Json = ctx.Json
                Meta = ctx.Metadata
                ResourceContext = ctx.ResourceContext
            }

    let writeResources (env: Env) (controls: seq<IControl>) (tw: UI.HtmlTextWriter) =
        // Resolve resources for the set of types and this assembly
        let resources =
            controls
            |> Seq.map (fun x -> x.GetType())
            |> Seq.distinct
            |> Seq.map (fun t ->
                M.Node.TypeNode (R.TypeDefinition.FromType t))
            |> env.Meta.GetDependencies
        // Meta tag encoding the client side controls
        let mJson = metaJson env.Json controls
        // Render meta
        tw.WriteLine(
            "<meta id='{0}' name='{0}' content='{1}' />",
            WebSharper.Html.Client.Activator.META_ID, 
            escape mJson
        )
        // Render resources
        for r in resources do
            r.Render env.ResourceContext tw

    let writeStartScript (tw: UI.HtmlTextWriter) =
        tw.WriteLine(@"<script type='{0}'>", CT.Text.JavaScript.Text)
        tw.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        tw.WriteLine @"  IntelliFactory.Runtime.Start();"
        tw.WriteLine @"</script>"

    let getResourcesAndScripts env controls =
        use m = new StringWriter()
        let tw = new UI.HtmlTextWriter(m, " ")
        writeResources env controls tw
        writeStartScript tw
        m.ToString()

    let toCustomContentAsync (genPage: Context<'T> -> Async<Page>) context : Async<Http.Response> =
        async {
            let! htmlPage = genPage context
            let writeBody (stream: Stream) =
                // Finds all the client side controls on the page.
                let controls =
                    htmlPage.Body
                    |> Seq.collect (fun elem ->
                        elem.CollectAnnotations ())
                let renderHead (tw: UI.HtmlTextWriter) =
                    writeResources (Env.Create context) controls tw
                    let writer = new H.Writer(tw)
                    for elem in htmlPage.Head do
                        writer.Write elem
                    writeStartScript tw
                let renderBody (tw: UI.HtmlTextWriter) =
                    let writer = new H.Writer(tw)
                    for elem in htmlPage.Body do
                        writer.Write elem
                // Create html writer from stream
                use textWriter = new StreamWriter(stream)
                textWriter.AutoFlush <- true
                use htmlWriter = new System.Web.UI.HtmlTextWriter(textWriter)
                htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                    renderHead renderBody htmlWriter
            return {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
                WriteBody = writeBody
            }
        }

    let toCustomContent genPage context =
        Async.RunSynchronously(toCustomContentAsync genPage context)

    let JsonContent<'T, 'U> (f: Context<'T> -> 'U) =
        let encoder = ActionEncoding.JsonProvider.GetEncoder<'U>()
        Content.CustomContent <| fun ctx ->
            let x = f ctx
            {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "application/json"]
                WriteBody = fun s ->
                    use tw = new StreamWriter(s)
                    x
                    |> encoder.Encode
                    |> ActionEncoding.JsonProvider.Pack
                    |> WebSharper.Core.Json.Write tw
            }

    let JsonContentAsync<'T, 'U> (f: Context<'T> -> Async<'U>) =
        let encoder = ActionEncoding.JsonProvider.GetEncoder<'U>()
        Content.CustomContentAsync <| fun ctx ->
            async {
                let! x = f ctx
                return {
                    Status = Http.Status.Ok
                    Headers = [Http.Header.Custom "Content-Type" "application/json"]
                    WriteBody = fun s ->
                        use tw = new StreamWriter(s)
                        x
                        |> encoder.Encode
                        |> ActionEncoding.JsonProvider.Pack
                        |> WebSharper.Core.Json.Write tw
                }
            }

    let ToResponseAsync<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        match c with
        | CustomContent x -> async { return x ctx }
        | CustomContentAsync x -> x ctx
        | PageContent genPage -> toCustomContentAsync (fun c -> async { return genPage c }) ctx
        | PageContentAsync genPage -> toCustomContentAsync genPage ctx

    let ToResponse c ctx =
        Async.RunSynchronously(ToResponseAsync c ctx)

    let FromAsync ac =
        CustomContentAsync <| fun ctx -> async {
            let! c = ac
            return! ToResponseAsync c ctx
        }

    let delay1 f =
        fun arg -> async { return f arg }

    [<Obsolete>]
    let ToCustomContent (c: Content<'T>) =
        match c with
        | CustomContent _ | CustomContentAsync _ -> c
        | PageContent genPage -> CustomContentAsync (toCustomContentAsync (delay1 genPage))
        | PageContentAsync genPageAsync -> CustomContentAsync (toCustomContentAsync genPageAsync)

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Content<'T>) =
        let genResp =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
            | PageContent genPage -> toCustomContentAsync (delay1 genPage)
            | PageContentAsync genPage -> toCustomContentAsync genPage
        CustomContentAsync <| fun context ->
            async {
                let! result = genResp context
                return! f result
            }

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Content<'T>) =
        let genResp =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
            | PageContent genPage -> toCustomContentAsync (delay1 genPage)
            | PageContentAsync genPage -> toCustomContentAsync genPage
        CustomContentAsync <| fun context ->
            async {
                let! result = genResp context
                return f result
            }

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Content<'T>) =
        cont
        |> MapResponseAsync (fun resp ->
            async {
                let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
                return {resp with Headers = headers}
            })

    let SetStatus<'T> (status: Http.Status) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp -> {resp with Status = status})

    /// Emits a 301 Moved Permanently response to a given URL.
    let RedirectToUrl<'T> (url: string) : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }

    /// Emits a 301 Moved Permanently response to a given action.
    let Redirect<'T> (action: 'T) =
        CustomContent <| fun ctx ->
            let resp = RedirectToUrl (ctx.Link action)
            ToResponse resp ctx

    let RedirectPermanentToUrl url = RedirectToUrl url
    let RedirectPermanent url = Redirect url

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 307 (Some "Temporary Redirect")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (action: 'T) : Content<'T> =
        CustomContent <| fun ctx ->
            ToResponse (RedirectTemporaryToUrl (ctx.Link action)) ctx

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

    let MethodNotAllowed<'T> : Content<'T> =
        httpStatusContent Http.Status.MethodNotAllowed

    type HtmlElement = H.Element

    type Hole<'T> =
        | SH of ('T -> Async<string>)
        | EH of ('T -> Async<seq<HtmlElement>>)

    type Wrapper<'T> =
        {
            appPath : string
            extra : Dictionary<string, seq<XS.INode>>
            value : 'T
        }

    let rec toXml (node: HtmlElement) : XS.INode =
        match node with
        | H.TagContent x ->
            let attrs = Dictionary()
            for attr in x.Attributes do
                attrs.[XS.Name.Create attr.Name] <- attr.Value
            XS.ElementNode {
                Name = XS.Name.Create x.Name
                Children = Seq.toArray (Seq.map toXml x.Contents) :> seq<_>
                Attributes = attrs
            } :> _
        | H.TextContent x -> XS.TextNode x :> _
        | H.VerbatimContent x -> XS.CDataNode x :> _
        | H.CommentContent x -> XS.TextNode "" :> _

    [<Literal>]
    let SCRIPTS = "SCRIPTS"

    module Template =
        type LoadFrequency =
            | Once
            | PerRequest
            | WhenChanged

    [<Sealed>]
    type CustomXml private () =
        static let self = CustomXml()
        static member Instance = self
        interface XT.IXml<HtmlElement,HtmlElement> with
            member this.Text x = H.TextContent x
            member this.CData x = H.VerbatimContent x
            member this.ElementNode x = x
            member this.Element(name, attrs, children) =
                children
                |> Seq.map (fun x -> x :> H.INode)
                |> Seq.append
                    (attrs
                    |> Seq.map (fun (KeyValue (k, v)) ->
                        H.NewAttr k.Local v :> H.INode))
                |> H.NewTag name.Local

    /// Decides if an attribute should contain a URL by HTML rules.
    let isUrlAttribute : XS.Element -> XS.Name -> bool =
        let d =
            Dictionary
                (Map
                    ([
                        "a", "href"
                        "applet", "codebase"
                        "area", "href"
                        "audio", "src"
                        "base", "url"
                        "blockquote", "cite"
                        "body", "background"
                        "button", "formaction"
                        "command", "icon"
                        "del", "cite"
                        "embed", "src"
                        "form", "action"
                        "frame", "src"
                        "head", "profile"
                        "html", "manifest"
                        "iframe", "src"
                        "img", "src"
                        "input", "src"
                        "ins", "cite"
                        "link", "href"
                        "q", "cite"
                        "script", "src"
                        "source", "src"
                        "video", "src"
                    ]))
        fun elem name ->
            match d.TryGetValue(elem.Name.Local) with
            | true, v -> v = name.Local
            | _ -> false

    /// Replaces `~` with `appPath` in URL positions.
    let postProcess (appPath: string) (element: XS.Element) =
        let rec n (node: XS.INode) : XS.INode =
            match node.Node with
            | XS.ElementNode x -> XS.ElementNode (e x) :> _
            | _ -> node
        and e (element: XS.Element) : XS.Element =
            let attributes =
                let key = Seq.tryFind (isUrlAttribute element) element.Attributes.Keys
                match key with
                | Some key ->
                    let value = element.Attributes.[key]
                    if value.StartsWith("~") then
                        let d = Dictionary(element.Attributes)
                        d.[key] <- joinWithSlash appPath (value.Substring(1))
                        d :> IDictionary<_,_>
                    else
                        element.Attributes
                | None ->
                    element.Attributes
            {
                Attributes = attributes
                Children = Seq.map n element.Children
                Name = element.Name
            }
        e element

    [<Sealed>]
    type Template<'T>(pathSpec: string, freq: Template.LoadFrequency, holes: Map<string,Hole<'T>>) =

        let path (root: string) =
            if pathSpec.StartsWith("~/") then
                Path.Combine(root, pathSpec.Substring(2))
            else
                pathSpec

        let pageTemplate =
            let mutable t = XT.Template<Wrapper<'T>>()
            for (KeyValue (k, v)) in holes do
                match v with
                | SH f -> t <- t.WithAsync(k, fun x -> f x.value)
                | EH f -> t <- t.With(k, fun x -> x.extra.[k])
            t <- t.With(SCRIPTS, fun x -> x.extra.[SCRIPTS])
            t <- t.With(SCRIPTS.ToLower(Globalization.CultureInfo("en-US")), fun x -> x.extra.[SCRIPTS])
            t

        let basicTemplate =
            let mutable t = XT.CustomTemplate<HtmlElement,HtmlElement,'T>(CustomXml.Instance)
            for (KeyValue (k, v)) in holes do
                match v with
                | SH f -> t <- t.WithAsync(k, f)
                | EH f -> t <- t.WithAsync(k, f)
            t

        static let memoize f =
            let d = Dictionary()
            fun x ->
                match d.TryGetValue(x) with
                | true, y -> y
                | _ ->
                    let y = f x
                    d.[x] <- y
                    y

        let getTemplate (root: string) (parse: string -> _) =
            let path = path root
            match freq with
            | Template.Once ->
                let t = lazy parse path
                fun () -> t.Value
            | Template.PerRequest ->
                fun () -> parse path
            | Template.WhenChanged ->
                let cell = ref None
                let read () =
                    try Choice1Of2 (parse path)
                    with e -> Choice2Of2 e
                let rec load () =
                    let watcher =
                        match !cell with
                        | None ->
                            // NOTE: resource leak here, watcher
                            // does not get disposed. Not a problem if
                            // template object is static.
                            watchForChanges path reload
                        | Some (_, w) -> w
                    let v = read ()
                    cell := Some (v, watcher)
                    v
                and reload () = lock cell (load >> ignore)
                let unpack x =
                    match x with
                    | Choice1Of2 x -> x
                    | Choice2Of2 (e: exn) -> raise e
                fun () ->
                    lock cell <| fun () ->
                        match !cell with
                        | Some (x, w) ->
                            unpack x
                        | None ->
                            unpack (load ())

        let getBasicTemplate =
            memoize (fun (root: string) -> getTemplate root basicTemplate.ParseFragmentFile)

        let getPageTemplate =
            memoize (fun (root: string) -> getTemplate root pageTemplate.Parse)

        new (path) = Template(path, Template.WhenChanged, Map.empty)
        new (path, freq) = Template(path, freq, Map.empty)

        member this.With(name: string, f: Func<'T,string>) =
            Template(pathSpec, freq, Map.add name (SH (async.Return << f.Invoke)) holes)

        member this.With(name: string, f: Func<'T,HtmlElement>) =
            let h = EH (fun x -> async.Return <| Seq.singleton (f.Invoke(x)))
            Template(pathSpec, freq, Map.add name h holes)

        member this.With(name: string, f: Func<'T,#seq<HtmlElement>>) =
            let h = EH (fun x -> async.Return <| Seq.cast(f.Invoke(x)))
            Template(pathSpec, freq, Map.add name h holes)

        member this.With(name: string, f: Func<'T,Async<string>>) =
            Template(pathSpec, freq, Map.add name (SH f.Invoke) holes)

        member this.With(name: string, f: Func<'T,Async<HtmlElement>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.singleton (result)
                })
            Template(pathSpec, freq, Map.add name h holes)

        member this.With(name: string, f: Func<'T,Async<#seq<HtmlElement>>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.cast(result)
                })
            Template(pathSpec, freq, Map.add name h holes)

        member this.Compile(root) =
            getBasicTemplate (defaultArg root ".")
            |> ignore
            this

        member this.Run(value: 'T, ?root: string) : seq<HtmlElement> =
            let t = getBasicTemplate (defaultArg root ".")
            t().Run(value)

        member this.CheckPageTemplate(root: string) =
            ignore (getPageTemplate root ())

        member this.Run(env: Env, x: Async<'T>, ?root: string) : Async<XS.Element> =
            let tpl = getPageTemplate (defaultArg root ".") ()
            let controls = Queue()
            let extra = Dictionary()
            async {
            let! x = x
            for KeyValue (k, v) in holes do
                match v with
                | SH _ -> ()
                | EH es ->
                    let! children = es x
                    let div = H.NewTag "div" children
                    div.CollectAnnotations()
                    |> Seq.iter controls.Enqueue
                    extra.[k] <-
                        Seq.map toXml children
                        |> Seq.toArray
                        :> seq<_>
            extra.[SCRIPTS] <-
                getResourcesAndScripts env controls
                |> XS.CDataNode :> XS.INode
                |> Seq.singleton
            return tpl.Run {
                appPath = env.AppPath
                extra = extra
                value = x
            }
            |> postProcess env.AppPath
            }

    let WithTemplateAsync<'Action,'T>
        (template: Template<'T>)
        (content: Context<'Action> -> Async<'T>) : Content<'Action> =
        CustomContentAsync (fun ctx ->
            async {
            template.CheckPageTemplate(ctx.RootFolder)
            let! xml = template.Run(Env.Create ctx, content ctx, ctx.RootFolder)
            return {
                Status = Http.Status.Ok
                Headers =
                    [
                        Http.Header.Custom "Content-Type"
                            "text/html; charset=utf-8"
                    ]
                WriteBody = fun s ->
                    use w = new System.IO.StreamWriter(s)
                    w.WriteLine("<!DOCTYPE html>")
                    XS.Node.RenderHtml w xml
            }})

    let WithTemplate<'Action,'T>
        (template: Template<'T>)
        (content: Context<'Action> -> 'T) : Content<'Action> =
        WithTemplateAsync template (fun ctx -> async.Return (content ctx))
