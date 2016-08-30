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

open System
open System.Collections.Generic
open System.Configuration
open System.Diagnostics
open System.IO
open System.Reflection
open System.Web
open System.Web.Compilation
open System.Web.Hosting
open WebSharper.Web
module R = WebSharper.Core.Remoting

module internal SiteLoading =

    type private BF = BindingFlags

    /// Looks up assembly-wide Website attribute and runs it if present
    let TryLoadSiteA (assembly: Assembly) =
        let aT = typeof<WebsiteAttribute>
        match Attribute.GetCustomAttribute(assembly, aT) with
        | :? WebsiteAttribute as attr ->
            attr.Run () |> Some
        | _ -> None
    
    /// Searches for static property with Website attribute and loads it if found
    let TryLoadSiteB (assembly: Assembly) =
        let aT = typeof<WebsiteAttribute>
        assembly.GetModules(false)
        |> Seq.collect (fun m ->
            try m.GetTypes() |> Seq.ofArray
            with
            | :? ReflectionTypeLoadException as e ->
                e.Types |> Seq.filter (fun t -> not (obj.ReferenceEquals(t, null)))
            | _ -> Seq.empty
        )
        |> Seq.tryPick (fun ty ->
            ty.GetProperties(BF.Static ||| BF.Public ||| BF.NonPublic)
            |> Array.tryPick (fun p ->
                match Attribute.GetCustomAttribute(p, aT) with
                | :? WebsiteAttribute ->
                    try
                        let sitelet = p.GetGetMethod().Invoke(null, [||])
                        let upcastSitelet =
                            sitelet.GetType()
                                .GetProperty("Upcast", BF.Instance ||| BF.NonPublic)
                                .GetGetMethod(nonPublic = true)
                                .Invoke(sitelet, [||])
                                :?> Sitelet<obj>
                        Some (upcastSitelet, [])
                    with e ->
                        raise <| exn("Failed to initialize sitelet definition: " + ty.FullName + "." + p.Name, e)  
                | _ -> None
            )
        )

    let TryLoadSite (assembly: Assembly) =
        match TryLoadSiteA assembly with
        | Some _ as res -> res
        | _ -> TryLoadSiteB assembly

    let LoadFromAssemblies (app: HttpApplication) =
        Timed "Initialized sitelets" <| fun () ->
            let assemblies =
                BuildManager.GetReferencedAssemblies()
                |> Seq.cast<Assembly>
            let sitelets, actions = 
                Seq.choose TryLoadSite assemblies 
                |> List.ofSeq |> List.unzip
            (Sitelet.Sum sitelets, Seq.concat actions)

module private WebUtils =

    let [<Literal>] HttpContextKey = "HttpContext"

//    let currentSite =
//        lazy fst (SiteLoading.LoadFromAssemblies())

    let getUri (req: HttpRequestBase) : Uri =
        match req.ApplicationPath with
        | "" | "/" -> req.Url
        | _ ->
            if req.Url.IsAbsoluteUri then
                let uB = UriBuilder req.Url
                if uB.Path.StartsWith(req.ApplicationPath) then
                    uB.Path <- uB.Path.Substring(req.ApplicationPath.Length)
                uB.Uri
            else
                req.Url

    /// Converts ASP.NET requests to Sitelet requests.
    let convertRequest (ctx: HttpContextBase) : Http.Request =
        let METHOD = function
            | "CONNECT" -> Http.Method.Connect
            | "DELETE" -> Http.Method.Delete
            | "GET" -> Http.Method.Get
            | "HEAD" -> Http.Method.Head
            | "OPTIONS" -> Http.Method.Options
            | "POST" -> Http.Method.Post
            | "PUT" -> Http.Method.Put
            | "TRACE" -> Http.Method.Trace
            | rest -> Http.Method.Custom rest
        let req = ctx.Request
        let resp = ctx.Response
        let headers =
            seq {
                for key in req.Headers.AllKeys do
                    yield Http.Header.Custom key req.Headers.[key]
            }
        // app.Context.Request.Cookies
        let parameters =
            seq {
                for p in ctx.Request.Params.AllKeys do
                    yield (p, req.[p])
            }
        {
            Method = METHOD ctx.Request.HttpMethod
            Uri = getUri req
            Headers = headers
            Body = req.InputStream
            Post = new Http.ParameterCollection(req.Form)
            Get  = new Http.ParameterCollection(req.QueryString)
            Cookies =
                { new Http.CookieCollection with
                    member this.Item(key) =
                        match req.Cookies.Get(key) with
                        | null -> None
                        | c -> Some c.Value
                    member this.ToList() =
                        [
                            let e = req.Cookies.GetEnumerator()
                            while e.MoveNext() do
                                match e.Current with
                                | :? HttpCookie as c -> yield c.Name, c.Value
                                | _ -> ()
                        ]
                    member this.Set(c) =
                        let c' = HttpCookie(c.Name, c.Value, HttpOnly = c.HttpOnly, Secure = c.Secure)
                        if c.Domain.IsSome then c'.Domain <- c.Domain.Value
                        if c.Path.IsSome then c'.Path <- c.Path.Value
                        match c.Duration with
                        | Http.CookieDuration.Session -> ()
                        | Http.CookieDuration.PersistentUntil(expires) -> c'.Expires <- expires
                        | Http.CookieDuration.PersistentFor(duration) -> c'.Expires <- System.DateTime.UtcNow.Add(duration)
                        req.Cookies.Set c'
                    member this.Clear() = req.Cookies.Clear()
                }
            ServerVariables = new Http.ParameterCollection(req.ServerVariables)
            Files =
                let fs = req.Files
                let files =
                    lazy
                    req.Files.AllKeys
                    |> Array.mapi (fun i k ->
                        let f = req.Files.[i]
                        let f =
                            { new Http.PostedFile with
                                member this.ContentLength = f.ContentLength
                                member this.ContentType = f.ContentType
                                member this.FileName = f.FileName
                                member this.InputStream = f.InputStream
                                member this.SaveAs(p) = f.SaveAs(p)
                            }
                        k, f
                    )
                    |> Seq.groupBy fst
                    |> List.ofSeq
                    |> List.map (fun (k, fs) -> k, fs |> List.ofSeq |> List.map snd)
                { new Http.MultiCollection<Http.PostedFile> with
                    member this.Item(key) =
                        match files.Value |> List.tryFind (fun (k, _) -> k = key) with
                        | None -> []
                        | Some (_, files) -> files
                    member this.ToList() = files.Value
                }
        }

    /// Constructs the sitelet context object.
    let getContext (site: Sitelet<obj>) (ctx: HttpContextBase) (request: Http.Request) =
        let appPath = ctx.Request.ApplicationPath
        new Context<obj>(
            ApplicationPath = appPath,
            ResolveUrl = (fun url ->
                if url.StartsWith("~") then
                    joinWithSlash appPath (url.Substring(1))
                else
                    url),
            Json = Shared.Json,
            Link = (fun action ->
                match site.Router.Link action with
                | Some loc ->
                    if loc.IsAbsoluteUri then string loc else
                        joinWithSlash appPath (string loc)
                | None -> failwith "Failed to link to action"),
            Metadata = Shared.Metadata,
            Dependencies = Shared.Dependencies,
            ResourceContext = ResourceContext.ResourceContext appPath,
            Request = request,
            RootFolder = ctx.Server.MapPath("~"),
            UserSession = new AspNetFormsUserSession(ctx),
            Environment = Map [HttpContextKey, box ctx]
        )

    /// Writes a response.
    let respond (site: Sitelet<obj>) (ctx: HttpContextBase) (req: Http.Request) (action: obj) =
        // Create a context
        let context = getContext site ctx req
        // Handle action
        async {
            let! response = Content.ToResponse (site.Controller.Handle action) context
            let resp = ctx.Response
            resp.Status <- response.Status.ToString()
            for header in response.Headers do
                resp.AddHeader(header.Name, header.Value)
            if req.Cookies.[RpcHandler.CsrfTokenKey].IsNone then
                RpcHandler.SetCsrfCookie resp
            response.WriteBody resp.OutputStream
            resp.End()
        }

/// The ISS handler for WebSharper applications.
[<Sealed>]
type HttpHandler(request: Http.Request, action: obj, site: Sitelet<obj>) =
    let processRequest ctx = WebUtils.respond site ctx request action
    let (beginAction, endAction, cancelAction) = Async.AsBeginEnd (fun ctx -> processRequest ctx)

    interface SessionState.IRequiresSessionState

    interface IHttpHandler with
        member this.IsReusable = false
        member this.ProcessRequest(ctx) = this.ProcessRequest(HttpContextWrapper(ctx)) |> Async.RunSynchronously

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, _) = beginAction (HttpContextWrapper(ctx), cb, null)
        member this.EndProcessRequest(result) = endAction result

    member this.ProcessRequest(ctx) = processRequest ctx

/// IIS module, processing the URLs and serving the pages.
[<Sealed>]
type HttpModule() =

    let siteAndActions = ref None

    let tryGetHandler (ctx: HttpContextBase) =
        !siteAndActions
        |> Option.bind (fun (site, actions) ->
            let request = WebUtils.convertRequest ctx
            site.Router.Route(request)
            |> Option.map (fun action ->
                HttpHandler(request, action, site)))

    interface IHttpModule with
        member this.Init app =
            siteAndActions := Some (SiteLoading.LoadFromAssemblies(app))
            let handler =
                new EventHandler(fun x e ->
                    let app = (x :?> HttpApplication)
                    let ctx = HttpContextWrapper(app.Context)
                    if not (RpcHandler.IsRemotingRequest(ctx.Request)) then
                        tryGetHandler ctx |> Option.iter (fun h ->
                            if HttpRuntime.UsingIntegratedPipeline then
                                ctx.RemapHandler(h)
                            else
                                ctx.Handler <- h))
            if HttpRuntime.UsingIntegratedPipeline
            then app.add_PostAuthorizeRequest(handler)
            else app.add_PostMapRequestHandler(handler)

        member this.Dispose() = ()

    member this.TryProcessRequest(ctx: HttpContextBase) : option<Async<unit>> =
        tryGetHandler ctx
        |> Option.map (fun h -> h.ProcessRequest(ctx))

    static member DiscoverSitelet(assemblies: seq<Assembly>) =
        let assemblies = Seq.cache assemblies
        match Seq.tryPick SiteLoading.TryLoadSiteA assemblies with
        | Some (s, _) -> Some s
        | _ ->
            Seq.tryPick SiteLoading.TryLoadSiteB assemblies
            |> Option.map fst
