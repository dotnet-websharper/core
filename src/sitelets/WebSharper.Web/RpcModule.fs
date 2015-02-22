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

namespace WebSharper.Web

open System
open System.IO
open System.Security.Principal
open System.Web.Security
open System.Web
module R = WebSharper.Core.Remoting

type AspNetFormsUserSession(ctx: HttpContext) =

    let refresh (cookie: HttpCookie) =
        match cookie with
        | null -> ctx.User <- null
        | cookie ->
            let ticket = FormsAuthentication.Decrypt cookie.Value
            let principal = GenericPrincipal(FormsIdentity(ticket), [||])
            ctx.User <- principal

    do  // Using `try ... with` because `FormsAuthentication.Decrypt`
        // throws an exception when there is a cookie but its format is invalid
        try refresh ctx.Request.Cookies.[FormsAuthentication.FormsCookieName]
        with _ -> refresh null

    interface IUserSession with

        member this.IsAvailable = true

        member this.GetLoggedInUser() =
            async {
                match ctx.User with
                | null -> return None
                | x ->
                    if x.Identity.IsAuthenticated then
                        return Some x.Identity.Name
                    else return None
            }

        member this.LoginUser(user, ?persistent) =
            async {
                let cookie = FormsAuthentication.GetAuthCookie(user, defaultArg persistent false)
                ctx.Response.SetCookie cookie
                return refresh cookie
            }

        member this.Logout() =
            async {
                match ctx.Response.Cookies.[FormsAuthentication.FormsCookieName] with
                | null -> return ()
                | cookie ->
                    cookie.Expires <- DateTime.Now.AddDays(-1.)
                    return refresh null
            }

module private RpcUtil =
    let server = R.Server.Create None Shared.Metadata

[<Sealed>]
type RpcHandler() =
    let work (ctx: HttpContext) =
        let req = ctx.Request
        let resp = ctx.Response
        async {
            // Manage "preflight" OPTIONS request
            // sent by the browser if the site is https.
            let origin = req.Headers.["Origin"]
            if origin <> null && req.Url.Authority = Uri(origin).Authority then
                resp.AddHeader("Access-Control-Allow-Origin", origin)
                resp.AddHeader("Access-Control-Allow-Credentials", "true")
            match req.HttpMethod with
            | "OPTIONS" ->
                resp.AddHeader("Access-Control-Allow-Headers",
                    "x-websharper-rpc, content-type")
            | _ ->
                let getHeader (x: string) =
                    match req.Headers.[x] with
                    | null -> None
                    | v -> Some v
                let body =
                    use s = new StreamReader(req.InputStream)
                    s.ReadToEnd()
                let! response =
                    RpcUtil.server.HandleRequest { Headers = getHeader; Body = body }
                resp.ContentType <- response.ContentType
                resp.Write response.Content
            return resp.End()
        }

    let (beginPR, endPR, cancelPR) = Async.AsBeginEnd(work)

    do Remoting.SetContext (fun () ->
        let root = HttpContext.Current.Server.MapPath("~")
        let uri = HttpContext.Current.Request.Url
        let session = new AspNetFormsUserSession(HttpContext.Current)
        { new IContext with
            member this.RootFolder = root
            member this.RequestUri = uri
            member this.UserSession = session :> _
        }
        |> Some)

    interface SessionState.IRequiresSessionState

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, d) = beginPR (ctx, cb, d)
        member this.EndProcessRequest(res) = endPR res

    interface IHttpHandler with
        member this.IsReusable = true
        member this.ProcessRequest(ctx) = this.ProcessRequest(ctx)

    member this.ProcessRequest(ctx: HttpContext) =
        work ctx
        |> Async.RunSynchronously


/// The WebSharper RPC HttpModule. Handles RPC requests.
[<Sealed>]
type RpcModule() =
    interface IHttpModule with
        member this.Init(app: HttpApplication) =
            let handler =
                new EventHandler(fun x e ->
                    let app = (x :?> HttpApplication)
                    let ctx = app.Context
                    let req = app.Request
                    let getHeader (x: string) =
                        match req.Headers.[x] with
                        | null -> None
                        | v -> Some v
                    if R.IsRemotingRequest getHeader then
                        if HttpRuntime.UsingIntegratedPipeline then
                            ctx.RemapHandler(RpcHandler())
                        else
                            ctx.Handler <- RpcHandler())
            if HttpRuntime.UsingIntegratedPipeline then
                app.add_PostAuthorizeRequest(handler)
            else
                app.add_PostMapRequestHandler(handler)
        member this.Dispose() = ()
