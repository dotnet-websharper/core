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

namespace IntelliFactory.WebSharper.Sitelets

open System
open System.Collections.Generic
open System.Configuration
open System.Diagnostics
open System.IO
open System.Reflection
open System.Web
open System.Web.Compilation
open System.Web.Hosting
module R = IntelliFactory.WebSharper.Core.Remoting

module internal SiteLoading =

    let TryLoadSite (assembly: Assembly) =
        let aT = typeof<WebsiteAttribute>
        match Attribute.GetCustomAttribute(assembly, aT) with
        | :? WebsiteAttribute as attr ->
            attr.Run () |> Some
        |_ -> None

    let LoadFromAssemblies (app: HttpApplication) =
        Timed "Initialized sitelets" <| fun () ->
            let assemblies =
                BuildManager.GetReferencedAssemblies()
                |> Seq.cast<Assembly>
            let pairs = Seq.choose TryLoadSite assemblies
            let sitelets = Seq.map fst pairs
            let actions = Seq.map snd pairs
            (Sitelet.Sum sitelets, Seq.concat actions)

module private WebUtils =

//    let currentSite =
//        lazy fst (SiteLoading.LoadFromAssemblies())

    let getUri (req: HttpRequest) : Uri =
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
    let convertRequest (ctx: HttpContext) : Http.Request =
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
            Cookies = req.Cookies
            ServerVariables = new Http.ParameterCollection(req.ServerVariables)
            Files =
                let fs = req.Files
                seq {
                    for k in fs.Keys do
                        yield HttpPostedFileWrapper(fs.[k]) :> _
                }
        }

    /// Constructs the sitelet context object.
    let getContext (site: Sitelet<obj>) (req: HttpRequest) (request: Http.Request) : Context<obj> =
        let appPath = req.ApplicationPath
        {
            ApplicationPath = appPath
            ResolveUrl = fun url ->
                if url.StartsWith("~") then
                    joinWithSlash appPath (url.Substring(1))
                else
                    url
            Json = ResourceContext.SharedJson()
            Link = fun action ->
                match site.Router.Link action with
                | Some loc ->
                    if loc.IsAbsoluteUri then string loc else
                        joinWithSlash appPath (string loc)
                | None -> failwith "Failed to link to action"
            Metadata = ResourceContext.MetaData()
            ResourceContext = ResourceContext.ResourceContext appPath
            Request = request
            RootFolder = HttpContext.Current.Server.MapPath("~")
        }

    /// Writes a response.
    let respond (site: Sitelet<obj>) (ctx: HttpContext) (req: Http.Request) (action: obj) =
        // Create a context
        let context = getContext site ctx.Request req
        // Handle action
        async {
            let! response =
                (site.Controller.Handle action, context)
                ||> Content.ToResponseAsync
            let resp = ctx.Response
            resp.Status <- response.Status.ToString()
            for header in response.Headers do
                resp.AddHeader(header.Name, header.Value)
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
        member this.ProcessRequest(ctx) = this.ProcessRequest(ctx) |> Async.RunSynchronously

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, _) = beginAction (ctx, cb, null)
        member this.EndProcessRequest(result) = endAction result

    member this.ProcessRequest(ctx) = processRequest ctx

/// IIS module, processing the URLs and serving the pages.
[<Sealed>]
type HttpModule() =
    let isNotRemotingRequest (r: HttpRequest) =
        let getHeader (x: string) =
            match r.Headers.[x] with
            | null -> None
            | x -> Some x
        not (R.IsRemotingRequest getHeader)
    interface IHttpModule with
        member this.Init app =
            let (site, actions) = SiteLoading.LoadFromAssemblies(app)
            let handler =
                new EventHandler(fun x e ->
                    let app = (x :?> HttpApplication)
                    let ctx = app.Context
                    let request = WebUtils.convertRequest ctx
                    if isNotRemotingRequest ctx.Request then
                        match site.Router.Route(request) with
                        | None -> ()
                        | Some action ->
                            let h = HttpHandler(request, action, site)
                            if HttpRuntime.UsingIntegratedPipeline then
                                ctx.RemapHandler(h)
                            else
                                ctx.Handler <- h)
            if HttpRuntime.UsingIntegratedPipeline
            then app.add_PostAuthorizeRequest(handler)
            else app.add_PostMapRequestHandler(handler)

        member this.Dispose() = ()
