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

namespace WebSharper.Web

open System
open System.IO
open System.Security.Cryptography
open System.Security.Principal
module R = WebSharper.Core.Remoting

#if NET461 // ASP.Net: RPC HttpModule
open System.Web.Security
open System.Web
open System.Configuration

type AspNetFormsUserSession(ctx: HttpContextBase) =

    let refresh (cookie: HttpCookie) =
        match cookie with
        | null -> ctx.User <- null
        | cookie ->
            let ticket = FormsAuthentication.Decrypt cookie.Value
            let principal = GenericPrincipal(FormsIdentity(ticket), [||])
            ctx.User <- principal

    let mkCookie (user: string) (duration: option<TimeSpan>) =
        let cookie = FormsAuthentication.GetAuthCookie(user, duration.IsSome)
        if duration.IsSome then cookie.Expires <- DateTime.UtcNow.Add(duration.Value)
        ctx.Response.SetCookie cookie
        refresh cookie

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

        member this.LoginUser(user: string, ?persistent: bool) =
            async {
                let durationOpt =
                    match persistent with
                    | Some true -> Some (TimeSpan.FromDays(1000.*365.))
                    | _ -> None
                mkCookie user durationOpt
            }

        member this.LoginUser(user: string, duration: TimeSpan) =
            async {
                mkCookie user (Some duration)
            }

        member this.Logout() =
            async {
                match ctx.Response.Cookies.[FormsAuthentication.FormsCookieName] with
                | null -> return ()
                | cookie ->
                    cookie.Expires <- DateTime.Now.AddDays(-1.)
                    return refresh null
            }
#endif

module private RpcUtil =
    let [<Literal>] HttpContextKey = "HttpContext"
    let [<Literal>] CsrfTokenKey = "csrftoken"
    let [<Literal>] CsrfTokenHeader = "x-" + CsrfTokenKey

type CorsAndCsrfCheckResult =
    | Ok of headers : list<string * string>
    | Preflight of headers : list<string * string>
    | Error of httpStatusCode: int * httpStatusMessage: string * responseText: string

[<Sealed>]
type RpcHandler() =

    static let corsAndCsrfCheck (reqMethod: string) (reqUrl: Uri) (getCookie: string -> option<string>) (getHeader: string -> option<string>) (setInfiniteCookie: string -> string -> unit) =
        let checkCsrf() =
            match getCookie RpcUtil.CsrfTokenKey with
            | None ->
                setInfiniteCookie RpcHandler.CsrfTokenKey (RpcHandler.MakeCsrfCookie())
                false
            | Some c ->
                match getHeader RpcUtil.CsrfTokenHeader with
                | None -> false
                | Some h ->
                    // The CSRF token is a Base64 string,
                    // so potential double percent-decoding is not an issue.
                    System.Uri.UnescapeDataString h = System.Uri.UnescapeDataString c
        let isSameAuthority origin =
            match Uri.TryCreate(origin, System.UriKind.Absolute) with
            | true, origin -> origin.Authority = reqUrl.Authority
            | false, _ -> false
        let origin = getHeader "Origin"
        let explicitlyAcceptedOrigin =
            match origin with
            | Some origin when isSameAuthority origin || Remoting.allowedOrigins.Contains (origin.ToLowerInvariant()) -> Some origin
            | _ -> None
        let acceptedOrigin =
            if Remoting.allowedOrigins.Contains "*" then Some "*" else explicitlyAcceptedOrigin
        let headers =
            match acceptedOrigin with
            | Some origin ->
                (if origin = "*" then [] else ["Vary", "Origin"])
                @ [
                    "Access-Control-Allow-Origin", origin
                    "Access-Control-Allow-Credentials", "true"
                ]
            | _ -> []
        match reqMethod with
        | "OPTIONS" ->
            ("Access-Control-Allow-Headers", "x-websharper-rpc, content-type, x-csrftoken")
            :: headers
            |> Preflight
        | _ when Remoting.csrfProtect && not (explicitlyAcceptedOrigin.IsSome || checkCsrf()) ->
            Error (403, "Forbidden", "CSRF")
        | _ ->
            Ok headers

#if NET461 // ASP.NET: RPC module
    do  Context.IsDebug <- fun () -> HttpContext.Current.IsDebuggingEnabled
        Context.GetSetting <- fun s ->
            ConfigurationManager.AppSettings.[s]
            |> Option.ofObj

    let server = R.Server.Create Shared.Metadata Shared.Json
    let rootFolder = HttpRuntime.AppDomainAppPath
    let appPath = HttpRuntime.AppDomainAppVirtualPath
    let resCtx = ResourceContext.ResourceContext appPath

    let work (ctx: HttpContextBase) =
        let req = ctx.Request
        let resp = ctx.Response
        async {
            // Manage "preflight" OPTIONS request
            // sent by the browser if the site is https or from another origin.
            match corsAndCsrfCheck req.HttpMethod req.Url
                    (fun k -> match req.Cookies.[k] with null -> None | c -> Some c.Value)
                    (fun k -> match req.Headers.[k] with null -> None | h -> Some h)
                    (fun k v -> HttpCookie(k, v, Expires = System.DateTime.UtcNow.AddYears(1000)) |> resp.SetCookie)
                    with
            | Error (code, descr, text) ->
                resp.StatusCode <- code
                resp.StatusDescription <- descr
                resp.Write(text)
            | Preflight headers ->
                headers |> List.iter (fun (k, v) -> resp.AddHeader(k, v))
            | Ok headers ->
                headers |> List.iter (fun (k, v) -> resp.AddHeader(k, v))
                let getHeader (x: string) =
                    match req.Headers.[x] with
                    | null -> None
                    | v -> Some v
                let body =
                    use s = new StreamReader(req.InputStream)
                    s.ReadToEnd()
                let uri = ctx.Request.Url
                let session = new AspNetFormsUserSession(ctx)
                let ctx =
                    { new Context() with
                        member this.RootFolder = rootFolder
                        member this.RequestUri = uri
                        member this.UserSession = session :> _ 
                        member this.Environment = upcast Map.ofList [(RpcUtil.HttpContextKey, ctx :> obj)]
                        member this.Json = Shared.Json
                        member this.Metadata = Shared.Metadata
                        member this.Dependencies = Shared.Dependencies
                        member this.ApplicationPath = appPath
                        member this.ResourceContext = resCtx }
                let! response =
                    server.HandleRequest({ Headers = getHeader; Body = body }, ctx)
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
#endif

    static member CsrfTokenKey = RpcUtil.CsrfTokenKey

    static member MakeCsrfCookie() =
        use rng = new RNGCryptoServiceProvider()
        let bytes = Array.zeroCreate 32
        rng.GetBytes(bytes)
        Convert.ToBase64String bytes

#if NET461 // ASP.NET: RPC HttpModule
    static member SetCsrfCookie (resp: HttpResponseBase) =
        HttpCookie(RpcUtil.CsrfTokenKey, RpcHandler.MakeCsrfCookie(),
            Expires = System.DateTime.UtcNow.AddYears(1000))
        |> resp.SetCookie
#endif

    static member CorsAndCsrfCheck reqMethod reqUrl getCookie getHeader setInfiniteCookie =
        corsAndCsrfCheck reqMethod reqUrl getCookie getHeader setInfiniteCookie

#if NET461 // ASP.NET: RPC HttpModule
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
#endif
