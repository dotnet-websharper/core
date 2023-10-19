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

namespace WebSharper.Sitelets

open System.IO
open System.Threading
open System.Threading.Tasks
open WebSharper
open WebSharper.Core

module CT = WebSharper.Core.ContentTypes

[<CompiledName "FSharpContent">]
type Content<'Endpoint> =
    | CustomContent of (Context<'Endpoint> -> Http.Response)
    | CustomContentAsync of (Context<'Endpoint> -> Async<Http.Response>)

    static member ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        match c with
        | CustomContent x -> async.Return (x ctx)
        | CustomContentAsync x -> x ctx

    member c.Box() : Content<obj> =
        match c with
        | CustomContent x -> CustomContent (fun ctx -> x (Context.Map box ctx))
        | CustomContentAsync x -> CustomContentAsync (fun ctx -> x (Context.Map box ctx))

    static member Unbox (c: Content<obj>) : Content<'T> =
        match c with
        | CustomContent x -> CustomContent (fun ctx -> x (Context.Map unbox ctx))
        | CustomContentAsync x -> CustomContentAsync (fun ctx -> x (Context.Map unbox ctx))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    type private HtmlTextWriter = WebSharper.Core.Resources.HtmlTextWriter

    type private Func<'A,'B> = System.Func<'A,'B>

    module M = WebSharper.Core.Metadata
//    module R = WebSharper.Core.Reflection
    module J = WebSharper.Core.Json    

    let defaultEncoding = new System.Text.UTF8Encoding(false) :> System.Text.Encoding

    type ActivateControl =
        | DecodeFromJson of id: string * data: string * deserializer: AST.Address

    let escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    let writeResources (ctx: Web.Context) (controls: seq<#IRequiresResources>) (tw: Core.Resources.RenderLocation -> HtmlTextWriter) =
        // Resolve resources for the set of types and this assembly
        // Some controls may depend on Requires called first and Encode second, do not break this
        let requiresAndCode =
            controls
            |> Seq.collect (fun c -> c.Requires (ctx.Metadata, ctx.Json))
            |> Array.ofSeq
        let resources =
            let nodeSet =
                requiresAndCode
                |> Seq.choose (fun c -> match c with ClientRequire n -> Some n | _ -> None) 
                |> Set
            ctx.ResourceContext.ResourceDependencyCache.GetOrAdd(nodeSet, fun nodes ->
                ctx.Dependencies.GetResources ctx.Metadata nodes
            )
        let hasResources = not (List.isEmpty resources)
        if hasResources then
            // Meta tag encoding the client side controls
            let toActivate = 
                requiresAndCode
                |> Seq.indexed
                |> Array.ofSeq

            // Render resources
            for r in resources do
                Core.Resources.Rendering.RenderCached(ctx.ResourceContext, r, tw)
            let scriptsTw = tw Core.Resources.Scripts
            let activate (url: string) =
                let imported = System.Collections.Generic.Dictionary<AST.CodeResource, string>()

                let rec getCode a =
                    match a with
                    | ClientRequire _ ->
                        ""
                    | ClientJsonData d ->
                        J.Stringify d     
                    | ClientFunctionCall (f, args) ->
                        let i =
                            match f.Module with
                            | AST.DotNetType m ->
                                match imported.TryGetValue(m) with
                                | true, i ->
                                    i
                                | _ ->
                                    let i = "i" + string (imported.Count + 1)
                                    match f.Address |> List.rev with
                                    | [] -> failwith "empty address"
                                    | a :: r ->
                                        let j = i :: r |> String.concat "."
                                        imported.Add(m, j)
                                        match a with
                                        | "default" ->
                                            scriptsTw.WriteLine($"""import {i} from "{url}{m.Assembly}/{m.Name}.js";""")
                                        | _ ->
                                            scriptsTw.WriteLine($"""import {{ {a} as {i} }} from "{url}{m.Assembly}/{m.Name}.js";""")
                                        j

                            | _ ->
                                f.Address |> List.rev |> String.concat "."
                        $"""{i}({ args |> Seq.map getCode |> String.concat "," })"""
                    | ClientReplaceInDom (i, c) -> 
                        $"""{getCode c}.ReplaceInDom(document.getElementById("{i}"))"""
                    | ClientReplaceInDomWithBody (i, c) -> 
                        $"""{getCode c}.Body.ReplaceInDom(document.getElementById("{i}"))"""
                    | ClientAddEventListener (i, e, c) ->
                        $"""document.getElementById("{i}").addEventListener("{e}",{getCode c})"""
                    | ClientDOMElement i ->
                        $"""document.getElementById("{i}")"""
                    | ClientInitialize (_, c) ->
                        getCode c

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, c) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""let {v} = {getCode c};""")
                        scriptsTw.WriteLine($"""{v}.$preinit("{i}");""")
                    | _ -> ()

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, _) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""{v}.$init("{i}");""")
                    | ClientRequire _ ->
                        ()
                    | _ ->
                        scriptsTw.Write(getCode a)
                        scriptsTw.WriteLine(";")

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, _) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""{v}.$postinit("{i}");""")
                    | _ -> ()

            Some activate
        else    
            None

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member this.Item
            with get (x: string) =
                match x.ToLowerInvariant() with
                | "scripts" -> this.Scripts
                | "styles" -> this.Styles
                | "meta" -> this.Meta
                | _ -> failwith "Invalid rendered resource identifier"

    let getSeparateResourcesAndScripts ctx controls : RenderedResources =
        use scriptsW = new StringWriter()
        let scriptsTw = new HtmlTextWriter(scriptsW, " ")
        use stylesW = new StringWriter()
        let stylesTw = new HtmlTextWriter(stylesW, " ")
        use metaW = new StringWriter()
        let metaTw = new HtmlTextWriter(metaW, " ")
        let activation =
            writeResources ctx controls (function
                | Core.Resources.Scripts -> scriptsTw
                | Core.Resources.Styles -> stylesTw
                | Core.Resources.Meta -> metaTw)
        if Option.isSome activation then
            scriptsTw.WriteStartCode(ctx.ResourceContext.ScriptBaseUrl, ?activation = activation)
        {
            Scripts = scriptsW.ToString()
            Styles = stylesW.ToString()
            Meta = metaW.ToString()
        }

    let getResourcesAndScripts ctx controls =
        use w = new StringWriter()
        use tw = new HtmlTextWriter(w, " ")
        let activation = writeResources ctx controls (fun _ -> tw)
        if Option.isSome activation then
            tw.WriteStartCode(ctx.ResourceContext.ScriptBaseUrl, ?activation = activation)
        w.ToString()
    
    let toCustomContentAsync (genPage: Context<'T> -> Async<Page>) context : Async<Http.Response> =
        async {
            let! htmlPage = genPage context
            let writeBody (stream: Stream) =
                let body = Seq.cache htmlPage.Body
                let renderHead (tw: HtmlTextWriter) =
                    let activation = writeResources context body (fun _ -> tw)
                    for elem in htmlPage.Head do
                        elem.Write(context, tw)
                    if Option.isSome activation then
                        tw.WriteStartCode(context.ResourceContext.ScriptBaseUrl, ?activation = activation)
                let renderBody (tw: HtmlTextWriter) =
                    for elem in body do
                        elem.Write(context, tw)
                // Create html writer from stream
                use textWriter = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                use htmlWriter = new HtmlTextWriter(textWriter, " ")
                htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                    renderHead renderBody htmlWriter
                textWriter.Flush()
            return {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
                WriteBody = Http.WriteBody writeBody
            }
        }

    let JsonContent<'T, 'U> (f: Context<'T> -> 'U) =
        let encoder = Json.ServerSideProvider.GetEncoder<'U>()
        Content.CustomContent <| fun ctx ->
            let x = f ctx
            {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "application/json"]
                WriteBody = Http.WriteBody (fun s ->
                    use tw = new StreamWriter(s, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                    x
                    |> encoder.Encode
                    |> WebSharper.Core.Json.Write tw
                )
            }

    let JsonContentAsync<'T, 'U> (f: Context<'T> -> Async<'U>) =
        let encoder = Json.ServerSideProvider.GetEncoder<'U>()
        Content.CustomContentAsync <| fun ctx ->
            async {
                let! x = f ctx
                return {
                    Status = Http.Status.Ok
                    Headers = [Http.Header.Custom "Content-Type" "application/json"]
                    WriteBody = Http.WriteBody (fun s ->
                        use tw = new StreamWriter(s, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                        x
                        |> encoder.Encode
                        |> WebSharper.Core.Json.Write tw
                    )
                }
            }

    let ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        Content<_>.ToResponse c ctx

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

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
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
        |> MapResponse (fun resp -> { resp with WriteBody = Http.WriteBody writeBody })

    /// Emits a 301 Moved Permanently response to a given URL.
    let RedirectToUrl<'T> (url: string) : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = Http.EmptyBody
            }

    /// Emits a 301 Moved Permanently response to a given action.
    let Redirect<'T> (endpoint: 'T) =
        CustomContentAsync <| fun ctx ->
            let resp = RedirectToUrl (ctx.Link endpoint)
            ToResponse resp ctx

    let RedirectPermanentToUrl url = RedirectToUrl url |> async.Return
    let RedirectPermanent endpoint = Redirect endpoint |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 307 (Some "Temporary Redirect")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = Http.EmptyBody
            }
        |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (endpoint: 'T) : Async<Content<'T>> =
        CustomContentAsync <| fun ctx -> async {
            let! content = RedirectTemporaryToUrl (ctx.Link endpoint)
            return! ToResponse content ctx
        }
        |> async.Return

    /// Constructs a status code response.
    let httpStatusContent<'T> status : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = Http.EmptyBody
            }
        |> async.Return

    let Unauthorized<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotFound

    let NotImplemented<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotImplemented

    let ServerError<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.InternalServerError

    let MethodNotAllowed<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.MethodNotAllowed

    let Ok<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Ok          

    let getOkOrigin (allows: CorsAllows) (request: Http.Request) =
        request.Headers
        |> Seq.tryFind (fun h -> h.Name.ToLowerInvariant() = "origin")
        |> Option.bind (fun origin ->
            if allows.Origins |> Seq.contains origin.Value then Some origin.Value
            elif allows.Origins |> Seq.contains "*" then Some "*"
            else None
        )

    let Cors (cors: Cors<'T>) (allows: CorsAllows -> CorsAllows) (content: 'T -> Async<Content<'U>>) : Async<Content<'U>> =
        let allows = allows (defaultArg cors.DefaultAllows CorsAllows.Empty)
        let headers (ctx: Context<'U>) =
            match getOkOrigin allows ctx.Request with
            | Some origin ->
                [
                    yield Http.Header.Custom "Access-Control-Allow-Origin" origin
                    match List.ofSeq allows.Methods with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Allow-Methods" (String.concat ", " l)
                    match List.ofSeq allows.Headers with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Allow-Headers" (String.concat ", " l)
                    match List.ofSeq allows.ExposeHeaders with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Expose-Headers" (String.concat ", " l)
                    match allows.MaxAge with
                    | None -> ()
                    | Some age -> yield Http.Header.Custom "Access-Control-Max-Age" (string age)
                    if allows.Credentials then
                        yield Http.Header.Custom "Access-Control-Allow-Credentials" "true"
                ]
            | None -> []
        match cors.EndPoint with
        | None ->
            CustomContent <| fun ctx ->
                {
                    Status = Http.Status.Ok
                    Headers = headers ctx
                    WriteBody = Http.EmptyBody
                }
            |> async.Return
        | Some ep ->
            CustomContentAsync <| fun ctx -> async {
                let! content = content ep
                let! resp =
                    match content with
                    | CustomContent f -> async.Return (f ctx)
                    | CustomContentAsync f -> f ctx
                return { resp with Headers = Seq.append (headers ctx) resp.Headers }
            }
            |> async.Return

[<System.Runtime.CompilerServices.Extension; Sealed>]
type ContextExtensions =

    [<System.Runtime.CompilerServices.Extension>]
    static member GetSeparateResourcesAndScripts(this, controls) =
        Content.getSeparateResourcesAndScripts this controls

    [<System.Runtime.CompilerServices.Extension>]
    static member GetResourcesAndScripts(this, controls) =
        Content.getResourcesAndScripts this controls

type Content<'Endpoint> with

    static member Custom (response: Http.Response) : Async<Content<'Endpoint>> =
        Content.CustomContent <| fun _ -> response
        |> async.Return

    static member Custom (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> unit) : Async<Content<'Endpoint>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg (WriteBody |> Option.map Http.WriteBody) Http.EmptyBody
        } : Http.Response)
        |> Content.Custom

    static member CustomAsync (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> Task) : Async<Content<'Endpoint>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg (WriteBody |> Option.map Http.WriteBodyAsync) Http.EmptyBody
        } : Http.Response)
        |> Content.Custom

    static member Json x : Async<Content<'Endpoint>> =
        Content.JsonContent <| fun _ -> x
        |> async.Return

    static member Page (?Body: #seq<#WebSharper.Web.INode>, ?Head:#seq<#WebSharper.Web.INode>, ?Title: string, ?Doctype: string) : Async<Content<'Endpoint>> =
        Content.Page {
            Doctype = Some (match Doctype with Some d -> d | None -> "<!DOCTYPE html>")
            Title = Title
            Head = match Head with None -> Seq.empty | Some x -> Seq.cast x
            Body = match Body with None -> Seq.empty | Some x -> Seq.cast x
            Renderer = Page.Default.Renderer
        }

    static member Page (page: Page) : Async<Content<'Endpoint>> =
        Content.CustomContentAsync (Content.toCustomContentAsync (fun _ -> async { return page }))
        |> async.Return

    static member Text (text: string, ?encoding: System.Text.Encoding) : Async<Content<'Endpoint>> =
        let encoding = defaultArg encoding Content.defaultEncoding
        Content.CustomAsync(
            WriteBody = fun s ->
                task {
                    let w = new System.IO.StreamWriter(s, encoding, 1024)
                    do! w.WriteAsync(text)
                    do! w.FlushAsync()
                }
        )

    static member File (path: string, ?AllowOutsideRootFolder: bool, ?ContentType) : Async<Content<'Endpoint>> =
        let allowOutsideRootFolder = defaultArg AllowOutsideRootFolder false
        Content.CustomContent <| fun ctx ->
            if Path.IsPathRooted path && not allowOutsideRootFolder then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.WebRootFolder).FullName
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
                    WriteBody = Http.WriteBodyAsync(fun out ->
                        task {
                            use inp = fi.OpenRead()
                            do! inp.CopyToAsync(out, 16 * 1024)
                        }
                    )
                }
            else
                failwith "Cannot serve file from outside the application's root folder"
        |> async.Return

    static member MvcResult (result: obj) : Async<Content<'Endpoint>> =
        ({
            Status = Http.Status.Ok
            Headers =Seq.empty
            WriteBody = Http.MvcBody result
        } : Http.Response)
        |> Content.Custom

open System
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#nowarn "49" // allow uppercase parameter names

[<AutoOpen>]
module Internal =

    type Task<'A> with
        member t.Map (f: 'A -> 'B) =
            t.ContinueWith(fun (t: Task<'A>) -> f t.Result)

[<CompiledName "Content"; Struct; Extension; NoEquality; NoComparison>]
type CSharpContent =

    val private c: Content<obj>

    new(c) = { c = c }

    static member ToContent (c: Async<Content<obj>>) : Task<CSharpContent> =
        Async.StartAsTask(c)
            .ContinueWith(fun (t: Task<Content<obj>>) -> CSharpContent t.Result)

    static member Opt x =
        match box x with
        | null -> None
        | _ -> Some x

    member this.AsContent = this.c

    static member FromContent(c: Content<obj>) = new CSharpContent(c)

    static member TaskAsContent (t: Task<CSharpContent>) =
        t.Map(fun c -> c.AsContent)

    static member Json<'U> (x: 'U) : Task<CSharpContent> =
        Content.Json x
        |> CSharpContent.ToContent

    static member Page
        (
            [<Optional>] Body: Web.INode,
            [<Optional>] Head: Web.INode,
            [<Optional>] Title: string,
            [<Optional>] Doctype: string
        ) =
        Content.Page(
            ?Body = Option.map Seq.singleton (CSharpContent.Opt Body),
            ?Head = Option.map Seq.singleton (CSharpContent.Opt Head),
            ?Title = CSharpContent.Opt Title, ?Doctype = CSharpContent.Opt Doctype)
        |> CSharpContent.ToContent

    static member Page (page: Page) =
        Content.Page(page)
        |> CSharpContent.ToContent

    static member Text (text: string, [<Optional>] Encoding: System.Text.Encoding) =
        Content.Text(text, ?encoding = CSharpContent.Opt Encoding)
        |> CSharpContent.ToContent

    static member File (path: string, [<Optional>] AllowOutsideRootFolder: bool, [<Optional>] ContentType: string) =
        Content.File(path, AllowOutsideRootFolder, ?ContentType = CSharpContent.Opt ContentType)
        |> CSharpContent.ToContent

    static member MvcResult (result: obj) =
        Content.MvcResult(result)
        |> CSharpContent.ToContent

    static member Custom (response: Http.Response) =
        Content.Custom(response)
        |> CSharpContent.ToContent

    static member Custom
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Action<Stream>
        ) =
        Content.Custom(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member CustomAsync
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Func<Stream, Task>
        ) =
        Content.CustomAsync(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member FromContext (f: Func<Context, Task<CSharpContent>>) =
        Content.FromContext (fun x -> Async.AwaitTask (f.Invoke(Context x).Map(fun c -> c.AsContent)))
        |> CSharpContent.ToContent

    [<Extension>]
    static member ToResponse (content: CSharpContent, context: Context) =
        Content.ToResponse content.AsContent context
        |> Async.StartAsTask

    static member FromTask (content: Task<CSharpContent>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.FromAsync
        |> CSharpContent

    [<Extension>]
    static member MapResponse (content: Task<CSharpContent>, f: Func<Http.Response, Http.Response>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponse f.Invoke
        |> CSharpContent.ToContent

    [<Extension>]
    static member MapResponseAsync (content: Task<CSharpContent>, f: Func<Http.Response, Task<Http.Response>>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponseAsync (fun r -> Async.AwaitTask (f.Invoke r))
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeader (content: Task<CSharpContent>, name: string, value: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeader name value
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithContentType (content: Task<CSharpContent>, contentType: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithContentType contentType
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, status: Http.Status) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus status
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, code: int, [<Optional>] message: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus (Http.Status.Custom code (CSharpContent.Opt message))
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetBody (content: Task<CSharpContent>, writeBody: Action<Stream>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetBody writeBody.Invoke
        |> CSharpContent.ToContent

    static member RedirectPermanent (action: obj) =
        Content.RedirectPermanent action
        |> CSharpContent.ToContent

    static member RedirectPermanentToUrl (url: string) =
        Content.RedirectPermanentToUrl url
        |> CSharpContent.ToContent

    static member RedirectTemporary (action: obj) =
        Content.RedirectTemporary action
        |> CSharpContent.ToContent

    static member RedirectTemporaryToUrl (url: string) =
        Content.RedirectTemporaryToUrl url
        |> CSharpContent.ToContent

    static member Unauthorized() =
        Content.Unauthorized
        |> CSharpContent.ToContent

    static member Forbidden() =
        Content.Forbidden
        |> CSharpContent.ToContent

    static member NotFound() =
        Content.NotFound
        |> CSharpContent.ToContent

    static member ServerError() =
        Content.ServerError
        |> CSharpContent.ToContent

    static member MethodNotAllowed() =
        Content.MethodNotAllowed
        |> CSharpContent.ToContent
