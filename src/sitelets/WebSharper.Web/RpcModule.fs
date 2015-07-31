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
open System.Security.Cryptography
open System.Security.Principal
open System.Web.Security
open System.Web
module R = WebSharper.Core.Remoting

type AspNetFormsUserSession(ctx: HttpContextBase) =

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
    let [<Literal>] HttpContextKey = "HttpContext"
    let [<Literal>] CsrfTokenKey = "csrftoken"
    let [<Literal>] CsrfTokenHeader = "x-" + CsrfTokenKey

[<Sealed>]
type RpcHandler() =

    let checkCsrf (req: HttpRequestBase) (resp: HttpResponseBase) =
        let cookie = req.Cookies.[RpcUtil.CsrfTokenKey]
        let header = req.Headers.[RpcUtil.CsrfTokenHeader]
        if cookie = null then
            RpcHandler.SetCsrfCookie resp
            false
        else
            header = cookie.Value

    let work (ctx: HttpContextBase) =
        let req = ctx.Request
        let resp = ctx.Response
        async {
            // Manage "preflight" OPTIONS request
            // sent by the browser if the site is https or from another origin.
            let isSameAuthority origin =
                match Uri.TryCreate(origin, System.UriKind.Absolute) with
                | true, origin -> origin.Authority = req.Url.Authority
                | false, _ -> false
            let origin = req.Headers.["Origin"]
            if origin <> null && (isSameAuthority origin || Remoting.allowedOrigins.Contains (origin.ToLowerInvariant())) then
                resp.AddHeader("Access-Control-Allow-Origin", origin)
                resp.AddHeader("Access-Control-Allow-Credentials", "true")
            match req.HttpMethod with
            | "OPTIONS" ->
                resp.AddHeader("Access-Control-Allow-Headers",
                    "x-websharper-rpc, content-type, x-csrftoken")
            | _ when Remoting.csrfProtect && not (checkCsrf req resp) ->
                resp.StatusCode <- 403
                resp.StatusDescription <- "Forbidden"
                resp.Write "CSRF"
            | _ ->
                let getHeader (x: string) =
                    match req.Headers.[x] with
                    | null -> None
                    | v -> Some v
                let body =
                    use s = new StreamReader(req.InputStream)
                    s.ReadToEnd()
                let root = ctx.Server.MapPath("~")
                let uri = ctx.Request.Url
                let session = new AspNetFormsUserSession(ctx)
                let ctx =
                    { new IContext with
                        member this.RootFolder = root
                        member this.RequestUri = uri
                        member this.UserSession = session :> _ 
                        member this.Environment = upcast Map.ofList [(RpcUtil.HttpContextKey, ctx :> obj)] }
                let! response =
                    RpcUtil.server.HandleRequest({ Headers = getHeader; Body = body }, ctx)
                resp.ContentType <- response.ContentType
                resp.Write response.Content
            return resp.End()
        }

    let (beginPR, endPR, cancelPR) = Async.AsBeginEnd(work)

    interface SessionState.IRequiresSessionState

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, d) = beginPR (HttpContextWrapper(ctx), cb, d)
        member this.EndProcessRequest(res) = endPR res

    interface IHttpHandler with
        member this.IsReusable = true
        member this.ProcessRequest(ctx) = this.ProcessRequest(HttpContextWrapper(ctx)) |> Async.RunSynchronously

    member this.ProcessRequest(ctx: HttpContextBase) =
        work ctx

    static member IsRemotingRequest (r: HttpRequestBase) =
        let getHeader (x: string) =
            match r.Headers.[x] with
            | null -> None
            | x -> Some x
        R.IsRemotingRequest getHeader

    static member CsrfTokenKey = RpcUtil.CsrfTokenKey

    static member SetCsrfCookie (resp: HttpResponseBase) =
        use rng = new RNGCryptoServiceProvider()
        let bytes = Array.zeroCreate 32
        rng.GetBytes(bytes)
        HttpCookie(RpcUtil.CsrfTokenKey, Convert.ToBase64String bytes,
            Expires = System.DateTime.UtcNow.AddYears(1000))
        |> resp.SetCookie


/// The WebSharper RPC HttpModule. Handles RPC requests.
[<Sealed>]
type RpcModule() =

    let handler = RpcHandler()

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
                            ctx.RemapHandler(handler)
                        else
                            ctx.Handler <- handler)
            if HttpRuntime.UsingIntegratedPipeline then
                app.add_PostAuthorizeRequest(handler)
            else
                app.add_PostMapRequestHandler(handler)
        member this.Dispose() = ()

    member this.TryProcessRequest(ctx: HttpContextBase) : option<Async<unit>> =
        let getHeader (x: string) =
            match ctx.Request.Headers.[x] with
            | null -> None
            | v -> Some v
        if R.IsRemotingRequest getHeader then
            Some (handler.ProcessRequest(ctx))
        else None
