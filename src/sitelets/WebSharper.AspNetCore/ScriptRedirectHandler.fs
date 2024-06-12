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

let Middleware (redirectUrlRoot: string) =
    Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
        if httpCtx.Request.Path.StartsWithSegments("/Scripts") || httpCtx.Request.Path.StartsWithSegments("/@vite") then
            let proxyRequest = httpCtx.Request.Path.Value
            httpCtx.Response.Redirect(redirectUrlRoot + proxyRequest)
            Task.CompletedTask
        else
            next.Invoke()
    )
