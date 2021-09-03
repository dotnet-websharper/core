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
open Microsoft.AspNetCore.Http
open WebSharper.Web
module Rem = WebSharper.Core.Remoting

let Middleware (options: WebSharperOptions) =
    let server = Rem.Server.Create options.Metadata options.Json
    Func<_,_,_>(fun (ctx: HttpContext) (next: Func<Task>) ->

        let getReqHeader (k: string) =
            match ctx.Request.Headers.TryGetValue(k) with
            | true, s -> Seq.tryHead s
            | false, _ -> None

        let addRespHeaders headers =
            headers |> List.iter (fun (k: string, v: string) ->
                let v = Microsoft.Extensions.Primitives.StringValues(v)
                ctx.Response.Headers.Add(k, v)
            )

        if Rem.IsRemotingRequest getReqHeader then
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
                async {
                    addRespHeaders headers
                    let wsctx = Context.GetOrMakeSimple ctx options
                    use reader = new StreamReader(ctx.Request.Body)
                    let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                    let! resp =
                        server.HandleRequest(
                            {
                                Body = body
                                Headers = getReqHeader
                            }, wsctx)
                    ctx.Response.StatusCode <- 200
                    ctx.Response.ContentType <- resp.ContentType
                    let bytes = Encoding.UTF8.GetBytes(resp.Content)
                    return! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length) |> Async.AwaitTask
                }
                |> Async.StartAsTask
                :> Task
            | CorsAndCsrfCheckResult.Preflight headers ->
                addRespHeaders headers
                Task.CompletedTask

        else next.Invoke()
    )
