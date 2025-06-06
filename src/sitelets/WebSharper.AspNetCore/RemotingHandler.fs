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
module WebSharper.AspNetCore.Remoting

open System
open System.IO
open System.Text
open System.Threading.Tasks
open System.Reflection
open Microsoft.AspNetCore.Http
open WebSharper.Web
open Microsoft.AspNetCore.Hosting

module Rem = WebSharper.Core.Remoting

let internal handleRemote (ctx: HttpContext) (server: Rem.Server) (options: WebSharperOptions) =

    let getReqHeader (k: string) =
        match ctx.Request.Headers.TryGetValue(k) with
        | true, s -> Seq.tryHead s
        | false, _ -> None

    let addRespHeaders headers =
        headers |> List.iter (fun (k: string, v: string) ->
            let v = Microsoft.Extensions.Primitives.StringValues(v)
            ctx.Response.Headers.Add(k, v)
        )

        options.RemotingHeaders
        |> Array.iter (fun (k, v) ->
            let v = Microsoft.Extensions.Primitives.StringValues(v)
            ctx.Response.Headers.Add(k, v)
        )

    if server.IsRemotingRequest ctx.Request.Path then
        let uri = Context.RequestUri ctx.Request
        let getCookie name =
            match ctx.Request.Cookies.TryGetValue(name) with
            | true, x -> Some x
            | false, _ -> None
        let setInfiniteCookie name value =
            ctx.Response.Cookies.Append(name, value,
                CookieOptions(Expires = Nullable(DateTimeOffset.UtcNow.AddYears(1000)))
            )
        match RpcHandler.CorsAndCsrfCheck ctx.Request.Method uri getCookie getReqHeader setInfiniteCookie with
        | CorsAndCsrfCheckResult.Error (code, msg, text) ->
            ctx.Response.StatusCode <- code
            let bytes = Encoding.UTF8.GetBytes(text)
            ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
        | CorsAndCsrfCheckResult.Ok headers ->
            task {
                addRespHeaders headers
                let wsctx = Context.GetOrMakeSimple ctx options
                use reader = new StreamReader(ctx.Request.Body)
                let! body = reader.ReadToEndAsync() 
                let! resp =
                    server.HandleRequest(
                        {
                            Path = ctx.Request.Path.ToString()
                            Body = body
                            Method = ctx.Request.Method
                            Headers = getReqHeader
                        }, wsctx)
                ctx.Response.StatusCode <- 200
                ctx.Response.ContentType <- resp.ContentType
                let bytes = Encoding.UTF8.GetBytes(resp.Content)
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            }
        | CorsAndCsrfCheckResult.Preflight headers ->
            addRespHeaders headers
            Task.CompletedTask
        |> Some
    else None

let Middleware (options: WebSharperOptions) =
    let getRemotingHandler (t: Type) =
        let service = options.Services.GetService(typedefof<IRemotingService<_>>.MakeGenericType([| t |])) 
        match service with
        | :? IRemotingService as s -> s.Handler
        | _ -> null
    let server = Rem.Server.Create options.Metadata WebSharper.Json.ServerSideProvider (Func<_,_> getRemotingHandler)
    Func<_,_,_>(fun (ctx: HttpContext) (next: Func<Task>) ->
        match handleRemote ctx server options with
        | Some rTask -> rTask
        | None -> next.Invoke()
    )

type RemotingHttpFuncResult = Task<HttpContext option>
type RemotingHttpFunc =  HttpContext -> RemotingHttpFuncResult
type RemotingHttpHandler = RemotingHttpFunc -> RemotingHttpFunc

let HttpHandler () : RemotingHttpHandler =
    fun (next: RemotingHttpFunc) ->
        let handleRPC (httpCtx: HttpContext) =
            let getReqHeader (k: string) =
                match httpCtx.Request.Headers.TryGetValue(k) with
                | true, s -> Seq.tryHead s
                | false, _ -> None
            let getRemotingHandler (t: Type) =
                let service = httpCtx.RequestServices.GetService(typedefof<IRemotingService<_>>.MakeGenericType([| t |])) 
                match service with
                | :? IRemotingService as s -> s.Handler
                | _ -> null
            
            let options =
                match httpCtx.RequestServices.GetService(typeof<IWebSharperService>) with
                | :? IWebSharperService as s -> s.WebSharperOptions
                | _ ->
                    WebSharperBuilder(httpCtx.RequestServices)
                        .UseSitelets(false)
                        .Build()
            let server = Rem.Server.Create options.Metadata WebSharper.Json.ServerSideProvider (Func<_,_> getRemotingHandler)
            if server.IsRemotingRequest httpCtx.Request.Path then
            
                match handleRemote httpCtx server options with
                | Some handle ->
                    task {
                        do! handle
                        return! next httpCtx
                    }
                | None -> 
                    Task.FromResult None

            else
                Task.FromResult None
                
        handleRPC