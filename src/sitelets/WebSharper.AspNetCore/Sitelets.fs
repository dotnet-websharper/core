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
module WebSharper.AspNetCore.Sitelets

open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Mvc.Abstractions
open WebSharper.Sitelets

let Middleware (options: WebSharperOptions) =
    let sitelet =
        match options.Sitelet with
        | Some s -> Some s
        | None -> Loading.DiscoverSitelet options.Assemblies
    match sitelet with
    | None ->
        Func<_,_,_>(fun (_: HttpContext) (next: Func<Task>) -> next.Invoke())
    | Some sitelet ->
        Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
            let ctx = Context.GetOrMake httpCtx options
            httpCtx.Items.Add("WebSharper.Sitelets.Context", ctx)
            
            let handleRouterResult r =
                match r with
                | Some endpoint ->
                    let actionCtx = ActionContext(httpCtx, RouteData(), ActionDescriptor())
                    let content = sitelet.Controller.Handle endpoint
                    (content:>IActionResult).ExecuteResultAsync actionCtx
                | None -> next.Invoke()

            let routeWithoutBody =
                try
                    Some (sitelet.Router.Route ctx.Request)
                with :? Router.BodyTextNeededForRoute ->
                    None 

            match routeWithoutBody with
            | Some r -> handleRouterResult r
            | None ->
                async {
                    do! ctx.Request.BodyTextAsync |> Async.Ignore
                    do! handleRouterResult (sitelet.Router.Route ctx.Request) |> Async.AwaitTask  
                }
                |> Async.StartAsTask :> Task
        )
