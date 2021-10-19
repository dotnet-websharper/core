// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
module WebSharper.AspNetCore.Context

open System
open System.Collections.Generic
open System.Collections.Specialized
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication
open WebSharper.Sitelets
module Res = WebSharper.Core.Resources

let private stripFinalSlash (s: string) =
    if s.EndsWith "/" then s.[..s.Length-2] else s

let private (++) (a: string) (b: string) =
    let startsWithSlash (s: string) =
        s.Length > 0
        && s.[0] = '/'
    let endsWithSlash (s: string) =
        s.Length > 0
        && s.[s.Length - 1] = '/'
    match endsWithSlash a, startsWithSlash b with
    | true, true -> a + b.Substring(1)
    | false, false -> a + "/" + b
    | _ -> a + b

let RequestUri (req: HttpRequest) =
    System.UriBuilder(
        req.Scheme,
        req.Host.Host,
        req.Host.Port.GetValueOrDefault(-1),
        req.Path.ToString(),
        req.QueryString.ToString()
    ).Uri

type private AspNetCoreRequest(req: HttpRequest) =
    inherit Http.Request()

    let method = Http.Method.OfString req.Method
    let uri = RequestUri req
    let mutable post = null
    let mutable get = null
    let mutable cookies = null
    let mutable bodyText = null : Task<string>

    override this.Method = method
    override this.Uri = uri                                     
    override this.Headers =
        seq {
            for KeyValue(k, v) in req.Headers do
                for x in v do
                    yield Http.Header.Custom k x
        }
    override this.Body = req.Body
    override this.Post = 
        if isNull post then
            post <-
                if req.HasFormContentType then 
                    { new Http.ParameterCollection with
                        member this.Item(name:string) =
                            match req.Form.TryGetValue name with
                            | true, v -> Some (string v)
                            | _ -> None
                        member this.ToList() =
                            [
                                for KeyValue(k, v) in req.Form do
                                    yield (k, string v)
                            ]    
                    }
                else
                    Http.EmptyParameters
        post
    override this.Get = 
        if isNull get then
            get <-
                { new Http.ParameterCollection with
                    member this.Item(name:string) =
                        match req.Query.TryGetValue name with
                        | true, v -> Some (string v)
                        | _ -> None
                    member this.ToList() =
                        [
                            for KeyValue(k, v) in req.Query do
                                yield (k, string v)
                        ]    
                }
        get
    override this.ServerVariables = Http.EmptyParameters
    override this.Files =
        seq {
            for f in req.Form.Files do
                yield { new Http.IPostedFile with
                    member this.Key = f.Name
                    member this.ContentLength = int f.Length
                    member this.ContentType = f.ContentType
                    member this.FileName = f.FileName
                    member this.InputStream = f.OpenReadStream()
                    member this.SaveAs(n) =
                        use fileStream = System.IO.File.Create(n)
                        use s = f.OpenReadStream()
                        s.CopyTo(fileStream)
                }
        }
    override this.Cookies =
        if isNull cookies then
            cookies <-
                { new Http.ParameterCollection with
                    member this.Item(name:string) =
                        match req.Cookies.TryGetValue name with
                        | true, v -> Some (string v)
                        | _ -> None
                    member this.ToList() =
                        [
                            for KeyValue(k, v) in req.Cookies do
                                yield (k, string v)
                        ]    
                }
        cookies
    override this.BodyText =
        this.BodyTextAsync |> Async.RunSynchronously
    override this.BodyTextAsync =
        if isNull bodyText then
            let i = this.Body
            if isNull i then
                bodyText <- Task.FromResult ""    
            else
                let reader = new System.IO.StreamReader(i, System.Text.Encoding.UTF8, false, 1024, leaveOpen = true)
                bodyText <- reader.ReadToEndAsync()
        bodyText |> Async.AwaitTask

let private buildRequest (req: HttpRequest) =
    AspNetCoreRequest req :> Http.Request

type private UserSession(httpCtx: HttpContext, options: IWebSharperService) =
    let scheme = options.AuthenticationScheme

    let loginUser (username: string) (expiry: option<TimeSpan>) =
        let identity = System.Security.Principal.GenericIdentity(username)
        let principal = System.Security.Principal.GenericPrincipal(identity, [||])
        let props = AuthenticationProperties()
        props.IsPersistent <- expiry.IsSome
        props.ExpiresUtc <-
            match expiry with
            | None -> Nullable()
            | Some t -> Nullable(DateTimeOffset.UtcNow.Add(t))
        httpCtx.User <- principal
        httpCtx.SignInAsync(scheme, principal, props)
        |> Async.AwaitTask

    interface WebSharper.Web.IUserSession with
        member this.GetLoggedInUser() =
            match httpCtx.User with
            | null -> None
            | u ->
                match u.Identity.Name with
                | null -> None
                | i -> Some i
            |> async.Return

        member this.Logout() =
            httpCtx.User <- null
            httpCtx.SignOutAsync(scheme)
            |> Async.AwaitTask

        member this.LoginUser(username, ?persistent: bool) =
            if defaultArg persistent false
                then Some (TimeSpan.FromDays(1000.*365.))
                else None
            |> loginUser username 

        member this.LoginUser(username, expiry: TimeSpan) =
            loginUser username (Some expiry)

        member this.IsAvailable = true

let private makeEnv (httpCtx: HttpContext)  =
    dict [|
        "WebSharper.AspNetCore.HttpContext", box httpCtx
    |]

let Make (httpCtx: HttpContext) (wsService: IWebSharperService,  isDebug, rootFolder, sitelet: Sitelet<'T>) =
    let appPath = httpCtx.Request.PathBase.ToUriComponent() 
    // WebSharper is caching ResourceContext object based on appPath
    let getSetting x =
        if isNull wsService.Configuration then 
            None
        else
            wsService.Configuration.[x] |> Option.ofObj 
    let resCtx = WebSharper.Web.ResourceContext.ResourceContext appPath wsService.Metadata isDebug getSetting
    let link x =
        match sitelet.Router.Link x with
        | None -> failwithf "Failed to link to %O" (box x)
        | Some loc when loc.IsAbsoluteUri -> string loc
        | Some loc -> appPath ++ string loc
    let req = buildRequest httpCtx.Request
    new Context<'T>(
        ApplicationPath = appPath,
        Environment = makeEnv httpCtx,
        Link = link,
        Json = wsService.Json,
        Metadata = wsService.Metadata,
        Dependencies = wsService.Dependencies,
        ResourceContext = resCtx,
        Request = req,
        RootFolder = rootFolder,
        UserSession = UserSession(httpCtx, wsService)
    )

let MakeSimple (httpCtx: HttpContext) (wsService: IWebSharperService, isDebug, rootFolder) =
    let appPath = httpCtx.Request.PathBase.ToUriComponent() 
    // WebSharper is caching ResourceContext object based on appPath
    let getSetting x =
        if isNull wsService.Configuration then 
            None
        else
            wsService.Configuration.[x] |> Option.ofObj 
    let resCtx = WebSharper.Web.ResourceContext.ResourceContext appPath wsService.Metadata isDebug getSetting
    let uri = RequestUri httpCtx.Request
    { new WebSharper.Web.Context() with
        member this.ApplicationPath = appPath
        // TODO use httpCtx.Items? but it's <obj, obj>, not <string, obj>
        member this.Environment = makeEnv httpCtx
        member this.Json = wsService.Json
        member this.Metadata = wsService.Metadata
        member this.Dependencies = wsService.Dependencies
        member this.ResourceContext = resCtx
        member this.RequestUri = uri
        member this.RootFolder = rootFolder
        member this.UserSession = UserSession(httpCtx, wsService) :> _
    }

let private getOrMake<'T, 'A> make (httpCtx: HttpContext) (args: 'A) =
    match httpCtx.Items.TryGetValue(EnvKey.Context) with
    | true, x -> x :?> 'T
    | false, _ ->
        let ctx = make httpCtx args
        httpCtx.Items.[EnvKey.Context] <- ctx
        ctx

let GetOrMake httpCtx options isDebug rootFolder sitelet =
    getOrMake<Context<'T>, _> Make httpCtx (options, isDebug, rootFolder, sitelet)

let GetOrMakeSimple httpCtx options isDebug rootFolder =
    getOrMake<WebSharper.Web.Context, _> MakeSimple httpCtx (options, isDebug, rootFolder)
