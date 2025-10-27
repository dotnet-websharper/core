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
namespace WebSharper.AspNetCore

open System
open System.IO
open System.Text
open System.Threading.Tasks
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Http
open WebSharper.Web
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open WebSharper.AspNetCore

module Rem = WebSharper.Core.Remoting

module Remoting =
    let internal handleRemote (ctx: HttpContext) (server: Rem.Server) (initService: IWebSharperInitializationService) (extraHeaders: (string * string)[]) =

        let getReqHeader (k: string) =
            match ctx.Request.Headers.TryGetValue(k) with
            | true, s -> Seq.tryHead s
            | false, _ -> None

        let addRespHeaders headers =
            headers |> List.iter (fun (k: string, v: string) ->
                let v = Microsoft.Extensions.Primitives.StringValues(v)
                ctx.Response.Headers.Add(k, v)
            )

            extraHeaders
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
                    let wsctx = Context.GetOrMakeSimple ctx initService
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

    // Giraffe/Saturn helpers

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
                    let service = httpCtx.RequestServices.GetService(typedefof<IWebSharperRemotingService<_>>.MakeGenericType([| t |])) 
                    match service with
                    | :? IWebSharperRemotingService as s -> s.Handler
                    | _ -> null
          
                let initService =
                    httpCtx.RequestServices.GetRequiredService<IWebSharperInitializationService>()

                let server =
                    match initService.RemotingServer with
                    | Some s -> s
                    | _ -> httpCtx.RequestServices.GetService<IWebSharperRemotingServerService>().RemotingServer
                  
                let extraHeaders = [||]

                if server.IsRemotingRequest httpCtx.Request.Path then
          
                    match handleRemote httpCtx server initService extraHeaders with
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
            
type WebSharperRemotingMiddleware(
    headers: (string * string)[],
    services: IServiceProvider
) =
    member this.MiddlewareFunc() =
        let initService = services.GetRequiredService<IWebSharperInitializationService>()

        match initService.RemotingServer with
        | Some server ->
            Func<_,_,_>(fun (ctx: HttpContext) (next: Func<Task>) ->
                match Remoting.handleRemote ctx server initService headers with
                | Some rTask -> rTask
                | None -> next.Invoke()
            )
        | None ->
            Func<_,_,_>(fun (ctx: HttpContext) (next: Func<Task>) ->
                match ctx.RequestServices.GetService<IWebSharperRemotingServerService>() with
                | null -> next.Invoke()
                | s ->
                    match Remoting.handleRemote ctx s.RemotingServer initService headers with
                    | Some rTask -> rTask
                    | None -> next.Invoke()
            )
