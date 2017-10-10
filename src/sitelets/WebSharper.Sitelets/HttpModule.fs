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

#if NET461 // ASP.NET: Sitelets HttpModule

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

module private WebUtils =

    let [<Literal>] HttpContextKey = "HttpContext"

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
            ServerVariables = new Http.ParameterCollection(req.ServerVariables)
            Files =
                let fs = req.Files
                seq {
                    for i = 0 to fs.Count - 1 do
                        let f = req.Files.[i]
                        let k = fs.GetKey(i)
                        yield { new Http.IPostedFile with
                            member this.Key = k
                            member this.ContentLength = f.ContentLength
                            member this.ContentType = f.ContentType
                            member this.FileName = f.FileName
                            member this.InputStream = f.InputStream
                            member this.SaveAs(n) = f.SaveAs(n)
                        }
                }
        }

    /// Constructs the sitelet context object.
    let getContext (site: Sitelet<obj>) (ctx: HttpContextBase) resCtx appPath rootFolder (request: Http.Request) =
        new Context<obj>(
            ApplicationPath = appPath,
            Json = Shared.Json,
            Link = (fun action ->
                match site.Router.Link action with
                | Some loc ->
                    if loc.IsAbsoluteUri then string loc else
                        joinWithSlash appPath (string loc)
                | None -> failwith "Failed to link to action"),
            Metadata = Shared.Metadata,
            Dependencies = Shared.Dependencies,
            ResourceContext = resCtx,
            Request = request,
            RootFolder = rootFolder,
            UserSession = new AspNetFormsUserSession(ctx),
            Environment = Map [HttpContextKey, box ctx]
        )

    /// Writes a response.
    let respond (site: Sitelet<obj>) (ctx: HttpContextBase) resCtx appPath rootFolder (req: Http.Request) (action: obj) =
        // Create a context
        let context = getContext site ctx resCtx appPath rootFolder req
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
type HttpHandler(request: Http.Request, action: obj, site: Sitelet<obj>, resCtx, appPath, rootFolder) =
    let processRequest ctx = WebUtils.respond site ctx resCtx appPath rootFolder request action
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

    let mutable runtime = None

    let tryGetHandler (ctx: HttpContextBase) =
        runtime
        |> Option.bind (fun (site, resCtx, appPath, rootFolder) ->
            let request = WebUtils.convertRequest ctx
            site.Router.Route(request)
            |> Option.map (fun action ->
                HttpHandler(request, action, site, resCtx, appPath, rootFolder)))

    do  Context.IsDebug <- fun () -> HttpContext.Current.IsDebuggingEnabled
        Context.GetSetting <- fun s ->
            ConfigurationManager.AppSettings.[s]
            |> Option.ofObj

    interface IHttpModule with
        member this.Init app =
            let appPath = HttpRuntime.AppDomainAppVirtualPath
            runtime <- Some (
                Loading.LoadFromApplicationAssemblies() |> fst,
                ResourceContext.ResourceContext appPath,
                appPath,
                HttpRuntime.AppDomainAppPath
            )
            let handler =
                new EventHandler(fun x _ ->
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

#endif
