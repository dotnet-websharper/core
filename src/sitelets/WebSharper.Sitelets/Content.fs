// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

open System.IO
module CT = WebSharper.Core.ContentTypes
module H = WebSharper.Html.Server.Html

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
    type private IRequiresResources = WebSharper.Html.Client.IRequiresResources
    type private IControl = WebSharper.Html.Client.IControl

    module Activator = WebSharper.Html.Client.Activator
    module M = WebSharper.Core.Metadata
    module R = WebSharper.Core.Reflection
    module J = WebSharper.Core.Json
    module XS = IntelliFactory.Xml.SimpleXml
    module XT = IntelliFactory.Xml.Templating

    let metaJson<'T> (jP: Core.Json.Provider) (controls: seq<IRequiresResources>) =
        let encode (c: IRequiresResources) =
            let encoder = jP.GetEncoder(c.GetType())
            encoder.Encode c
        [ for c in controls do
            match c with
            | :? IControl as c -> yield (c.Id, encode c)
            | _ -> ()
        ]
        |> J.Encoded.Object
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

    let writeResources (env: Env) (controls: seq<IRequiresResources>) (tw: Core.Resources.RenderLocation -> UI.HtmlTextWriter) =
        // Resolve resources for the set of types and this assembly
        let resources =
            controls
            |> Seq.collect (fun c -> c.Requires env.Meta)
            |> env.Meta.GetDependencies
        let hasResources = not (List.isEmpty resources)
        if hasResources then
            // Meta tag encoding the client side controls
            let mJson = metaJson env.Json controls
            // Render meta
            (tw Core.Resources.Meta).WriteLine(
                "<meta id='{0}' name='{0}' content='{1}' />",
                WebSharper.Html.Client.Activator.META_ID,
                escape mJson
            )
            // Render resources
            for r in resources do
                r.Render env.ResourceContext tw
        hasResources

    let writeStartScript (tw: UI.HtmlTextWriter) =
        tw.WriteLine(@"<script type='{0}'>", CT.Text.JavaScript.Text)
        tw.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        tw.WriteLine @"  IntelliFactory.Runtime.Start();"
        tw.WriteLine @"</script>"

    let getSeparateResourcesAndScripts env controls =
        use scriptsW = new StringWriter()
        let scriptsTw = new UI.HtmlTextWriter(scriptsW, " ")
        use stylesW = new StringWriter()
        let stylesTw = new UI.HtmlTextWriter(stylesW, " ")
        use metaW = new StringWriter()
        let metaTw = new UI.HtmlTextWriter(metaW, " ")
        let hasResources =
            writeResources env controls (function
                | Core.Resources.Scripts -> scriptsTw
                | Core.Resources.Styles -> stylesTw
                | Core.Resources.Meta -> metaTw)
        if hasResources then writeStartScript scriptsTw
        scriptsW.ToString(), stylesW.ToString(), metaW.ToString()

    let getResourcesAndScripts env controls =
        use w = new StringWriter()
        use tw = new UI.HtmlTextWriter(w, " ")
        let hasResources = writeResources env controls (fun _ -> tw)
        if hasResources then writeStartScript tw
        w.ToString()

    let toCustomContentAsync (genPage: Context<'T> -> Async<Page>) context : Async<Http.Response> =
        async {
            let! htmlPage = genPage context
            let writeBody (stream: Stream) =
                let body = Seq.cache htmlPage.Body
                // Finds all the client side controls on the page.
                let controls =
                    body
                    |> Seq.collect (fun elem ->
                        elem.CollectAnnotations ())
                let renderHead (tw: UI.HtmlTextWriter) =
                    let hasResources = writeResources (Env.Create context) controls (fun _ -> tw)
                    let writer = new H.Writer(tw)
                    for elem in htmlPage.Head do
                        writer.Write elem
                    if hasResources then writeStartScript tw
                let renderBody (tw: UI.HtmlTextWriter) =
                    let writer = new H.Writer(tw)
                    for elem in body do
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

    let ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        match c with
        | CustomContent x -> async { return x ctx }
        | CustomContentAsync x -> x ctx
        | PageContent genPage -> toCustomContentAsync (fun c -> async { return genPage c }) ctx
        | PageContentAsync genPage -> toCustomContentAsync genPage ctx

    let FromContext f =
        Content.CustomContentAsync (fun ctx -> async {
            let! content = f ctx
            return! ToResponse content ctx
        })
        |> async.Return

    let ToResponseAsync c ctx = ToResponse c ctx

    let FromAsync ac =
        CustomContentAsync <| fun ctx -> async {
            let! c = ac
            return! ToResponse c ctx
        }

    let delay1 f =
        fun arg -> async { return f arg }

    let ToCustomContent (c: Content<'T>) =
        match c with
        | CustomContent _ | CustomContentAsync _ -> c
        | PageContent genPage -> CustomContentAsync (toCustomContentAsync (delay1 genPage))
        | PageContentAsync genPageAsync -> CustomContentAsync (toCustomContentAsync genPageAsync)

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
            | PageContent genPage -> toCustomContentAsync (delay1 genPage)
            | PageContentAsync genPage -> toCustomContentAsync genPage
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return! f result
            }
        |> async.Return

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
            | PageContent genPage -> toCustomContentAsync (delay1 genPage)
            | PageContentAsync genPage -> toCustomContentAsync genPage
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return f result
            }
        |> async.Return

    let SetHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Headers = headers })

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            { resp with Headers = headers }
        )

    let WithHeader<'T> (name: string) (value: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom name value]

    let WithContentType<'T> (contentType: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom "Content-Type" contentType]

    let SetStatus<'T> (status: Http.Status) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Status = status })

    let SetBody<'T> (writeBody: System.IO.Stream -> unit) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with WriteBody = writeBody })

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
        CustomContentAsync <| fun ctx ->
            let resp = RedirectToUrl (ctx.Link action)
            ToResponse resp ctx

    let RedirectPermanentToUrl url = RedirectToUrl url |> async.Return
    let RedirectPermanent url = Redirect url |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 307 (Some "Temporary Redirect")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = ignore
            }
        |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (action: 'T) : Async<Content<'T>> =
        CustomContentAsync <| fun ctx -> async {
            let! content = RedirectTemporaryToUrl (ctx.Link action)
            return! ToResponse content ctx
        }
        |> async.Return

    /// Constructs a status code response.
    let httpStatusContent<'T> status : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = ignore
            }
        |> async.Return

    let Unauthorized<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotFound

    let ServerError<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.InternalServerError

    let MethodNotAllowed<'T> : Async<Content<'T>> =
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

    let toXmlElement (node: HtmlElement) : option<XS.Element> =
        match (toXml node).Node with
        | XS.ElementNode n -> Some n
        | _ -> None

    [<Literal>]
    let SCRIPTS = "SCRIPTS"
    [<Literal>]
    let STYLES = "STYLES"
    [<Literal>]
    let META = "META"

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
    type Template<'T>(getBasicTemplate, getPageTemplate, holes: Map<string,Hole<'T>>, controls: Queue<_>) =

        static let basicTemplate holes =
            let t = ref (XT.CustomTemplate<HtmlElement,HtmlElement,'T>(CustomXml.Instance))
            holes |> Seq.iter (fun (KeyValue (k, v)) ->
                match v with
                | SH f -> t := (!t).WithAsync(k, f)
                | EH f -> t := (!t).WithAsync(k, f))
            !t

        static let pageTemplate holes =
            let t = ref (XT.Template<Wrapper<'T>>())
            holes |> Seq.iter (fun (KeyValue (k, v)) ->
                match v with
                | SH f -> t := (!t).WithAsync(k, fun x -> f x.value)
                | EH f -> t := (!t).With(k, fun x -> x.extra.[k]))
            for name in [|SCRIPTS; STYLES; META|] do
                t := (!t).With(name, fun x -> x.extra.[name])
                t := (!t).With(name.ToLower(Globalization.CultureInfo("en-US")), fun x -> x.extra.[name])
            !t

        let getBasicTemplate' = lazy getBasicTemplate holes
        let getPageTemplate' = lazy getPageTemplate holes

        static let memoize f =
            let d = Dictionary()
            fun x ->
                match d.TryGetValue(x) with
                | true, y -> y
                | _ ->
                    let y = f x
                    d.[x] <- y
                    y

        static let getTemplate freq (path: string) (parse: string -> _) =
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
                fun () ->
                    lock cell <| fun () ->
                        match !cell with
                        | Some (Choice1Of2 x, w) ->
                            x
                        | Some (Choice2Of2 _, _)
                        | None ->
                            match load() with
                            | Choice1Of2 x -> x
                            | Choice2Of2 exn -> raise exn

        new (pathSpec: string, freq: Template.LoadFrequency) =
            let path (root: string) =
                if pathSpec.StartsWith("~/") then
                    Path.Combine(root, pathSpec.Substring(2))
                else
                    pathSpec
            let getBasicTemplate holes =
                memoize (fun root -> getTemplate freq (path root) (basicTemplate holes).ParseFragmentFile)
            let getPageTemplate holes =
                memoize (fun root -> getTemplate freq (path root) (pageTemplate holes).Parse)
            Template(getBasicTemplate, getPageTemplate, Map.empty, Queue())

        new (path) = Template(path, Template.WhenChanged)

        static member FromHtmlElement (rootElement: HtmlElement) =
            let getFragmentTemplate holes =
                let f = (basicTemplate holes).ParseNodes [(toXml rootElement).Node]
                fun _ _ -> f
            let getPageTemplate holes =
                let f =
                    match toXmlElement rootElement with
                    | Some e -> (pageTemplate holes).ParseElement e
                    | None -> failwith "Template.FromHtmlElement: must pass an element, not a CData or Text node"
                fun _ _ -> f
            let controls = Queue()
            rootElement.CollectAnnotations()
            |> Seq.iter controls.Enqueue
            Template<'T>(getFragmentTemplate, getPageTemplate, Map.empty, controls)

        member this.With(name: string, f: Func<'T,string>) =
            Template(getBasicTemplate, getPageTemplate, Map.add name (SH (async.Return << f.Invoke)) holes, controls)

        member this.With(name: string, f: Func<'T,HtmlElement>) =
            let h = EH (fun x -> async.Return <| Seq.singleton (f.Invoke(x)))
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,#seq<HtmlElement>>) =
            let h = EH (fun x -> async.Return <| Seq.cast(f.Invoke(x)))
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,Async<string>>) =
            Template(getBasicTemplate, getPageTemplate, Map.add name (SH f.Invoke) holes, controls)

        member this.With(name: string, f: Func<'T,Async<HtmlElement>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.singleton (result)
                })
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,Async<#seq<HtmlElement>>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.cast(result)
                })
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.Compile(root) =
            getBasicTemplate holes (defaultArg root ".")
            |> ignore
            this

        member this.Run(value: 'T, ?root: string) : seq<HtmlElement> =
            let t = getBasicTemplate'.Value (defaultArg root ".")
            t().Run(value)

        member this.CheckPageTemplate(root: string) =
            ignore (getPageTemplate holes root ())

        member this.Run(env: Env, x: Async<'T>, ?root: string) : Async<XS.Element> =
            let controls = Queue(controls)
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
            let scripts, styles, meta = getSeparateResourcesAndScripts env controls
            let tpl = getPageTemplate'.Value (defaultArg root ".") ()
            if tpl.Holes |> Seq.exists (fun h -> let h = h.ToUpperInvariant() in h = STYLES || h = META) then
                extra.[SCRIPTS] <- Seq.singleton (XS.CDataNode scripts :> _)
                extra.[STYLES] <- Seq.singleton (XS.CDataNode styles :> _)
                extra.[META] <- Seq.singleton (XS.CDataNode meta :> _)
            else
                let scripts = String.concat "" [|styles; meta; scripts|]
                extra.[SCRIPTS] <- Seq.singleton (XS.CDataNode scripts :> _)
            return tpl.Run {
                appPath = env.AppPath
                extra = extra
                value = x
            }
            |> postProcess env.AppPath
            }

    let WithTemplateAsync<'Action,'T>
        (template: Template<'T>)
        (content: Async<'T>) : Async<Content<'Action>> =
        CustomContentAsync (fun ctx ->
            async {
            template.CheckPageTemplate(ctx.RootFolder)
            let! xml = template.Run(Env.Create ctx, content, ctx.RootFolder)
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
        |> async.Return

    let WithTemplate<'Action,'T>
        (template: Template<'T>)
        (content: 'T) : Async<Content<'Action>> =
        WithTemplateAsync template (async.Return content)

type Content<'Action> with

    static member Custom (response: Http.Response) : Async<Content<'Action>> =
        Content.CustomContent <| fun _ -> response
        |> async.Return

    static member Custom (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> unit) : Async<Content<'Action>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg WriteBody ignore
        } : Http.Response)
        |> Content.Custom

    static member Json x : Async<Content<'Action>> =
        Content.JsonContent <| fun _ -> x
        |> async.Return

    static member Page (?Body: #seq<H.Element>, ?Head:#seq<H.Element>, ?Title: string, ?Doctype: string) : Async<Content<'Action>> =
        Content.Page {
            Doctype = Some (match Doctype with Some d -> d | None -> "<!DOCTYPE html>")
            Title = Title
            Head = match Head with None -> Seq.empty | Some x -> x :> seq<_>
            Body = match Body with None -> Seq.empty | Some x -> x :> seq<_>
            Renderer = Page.Default.Renderer
        }

    static member Page (page: Page) : Async<Content<'Action>> =
        Content.PageContent <| fun _ -> page
        |> async.Return

    static member Page (page: H.Element) : Async<Content<'Action>> =
        Content.WithTemplate (Content.Template.FromHtmlElement page) ignore

    static member Text (text: string, ?encoding: System.Text.Encoding) : Async<Content<'Action>> =
        let encoding = defaultArg encoding System.Text.Encoding.UTF8
        Content.Custom(
            WriteBody = fun s ->
                use w = new System.IO.StreamWriter(s, encoding)
                w.Write(text)
        )

    static member File (path: string, ?AllowOutsideRootFolder: bool, ?ContentType) : Async<Content<'Action>> =
        let allowOutsideRootFolder = defaultArg AllowOutsideRootFolder false
        Content.CustomContent <| fun ctx ->
            if Path.IsPathRooted path && not allowOutsideRootFolder then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.RootFolder).FullName
            let path =
                if path.StartsWith "~/" || path.StartsWith @"~\" then
                    Path.Combine(rootFolder, path.[2..])
                else
                    Path.Combine(rootFolder, path)
            let fi = System.IO.FileInfo(path)
            if fi.FullName.StartsWith rootFolder || allowOutsideRootFolder then
                {
                    Status = Http.Status.Ok
                    Headers = [if ContentType.IsSome then yield Http.Header.Custom "Content-Type" ContentType.Value]
                    WriteBody = fun out ->
                        use inp = fi.OpenRead()
                        let buffer = Array.zeroCreate (16 * 1024)
                        let rec loop () =
                            let read = inp.Read(buffer, 0, buffer.Length)
                            if read > 0 then out.Write(buffer, 0, read); loop ()
                        loop ()
                }
            else
                failwith "Cannot serve file from outside the application's root folder"
        |> async.Return
