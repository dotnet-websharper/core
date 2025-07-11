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
module WebSharper.AspNetCore.ScriptRedirect

open System
open System.IO
open System.Threading.Tasks
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Mvc.Abstractions
open Microsoft.Extensions.Options
open WebSharper.Sitelets
open Microsoft.Extensions.Logging
open System.Net.Http
open Microsoft.Extensions.Primitives

let Middleware (redirectUrlRoot: string) =
    let httpClient = new HttpClient()
    let disallowed = set [ "transfer-encoding"; "content-length"; "connection"; "keep-alive"; "proxy-connection"; "trailer"; "upgrade" ]
    Func<_, _, _>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
        if httpCtx.Request.Path.StartsWithSegments("/Scripts") || httpCtx.Request.Path.StartsWithSegments("/@vite") || httpCtx.Request.Path.StartsWithSegments("/@fs") then
            task {
                let relativePath = httpCtx.Request.Path + httpCtx.Request.QueryString.Value
                let targetUrl = redirectUrlRoot.TrimEnd('/') + relativePath

                use reqMsg = new HttpRequestMessage(HttpMethod.Get, targetUrl)

                // Optional: copy headers like User-Agent
                httpCtx.Request.Headers
                |> Seq.iter (fun kvp -> reqMsg.Headers.TryAddWithoutValidation(kvp.Key, kvp.Value.ToArray()) |> ignore)

                use! response = httpClient.SendAsync(reqMsg)

                httpCtx.Response.StatusCode <- int response.StatusCode

                // Copy response headers
                for header in response.Headers do
                    if not (disallowed.Contains(header.Key.ToLowerInvariant())) then
                        httpCtx.Response.Headers[header.Key] <- StringValues(header.Value |> Seq.toArray)
                for header in response.Content.Headers do
                    if not (disallowed.Contains(header.Key.ToLowerInvariant())) then
                        httpCtx.Response.Headers[header.Key] <- StringValues(header.Value |> Seq.toArray)

                use! content = response.Content.ReadAsStreamAsync()
                do! content.CopyToAsync(httpCtx.Response.Body)
                do! httpCtx.Response.Body.FlushAsync()
            } :> Task
        else
            next.Invoke()
    )
