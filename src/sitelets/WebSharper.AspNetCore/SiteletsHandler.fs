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
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Mvc.Abstractions
open Microsoft.Extensions.Options
open WebSharper.Sitelets

let rec internal contentHelper (httpCtx: HttpContext) (content: obj) : Task =
    task {
        match content with
        | :? string as stringContent ->
            httpCtx.Response.StatusCode <- StatusCodes.Status200OK
            do! httpCtx.Response.WriteAsync(stringContent)
        | :? IActionResult as actionResult ->
            let actionCtx = ActionContext(httpCtx, RouteData(), ActionDescriptor())
            do! actionResult.ExecuteResultAsync(actionCtx)
        | _ ->
            let contentType = content.GetType()
            if contentType.IsGenericType && contentType.GetGenericTypeDefinition() = typedefof<Task<_>> then
                let contentTask = content :?> Task
                do! contentTask |> Async.AwaitTask
                let contentResult =
                    let resultGetter = contentType.GetProperty("Result")
                    resultGetter.GetMethod.Invoke(contentTask, [||])
                return! contentHelper httpCtx contentResult
            else
                httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                let jsonOptions = httpCtx.RequestServices.GetService(typeof<IOptions<JsonSerializerOptions>>) :?> IOptions<JsonSerializerOptions>;
                do! System.Text.Json.JsonSerializer.SerializeAsync(httpCtx.Response.Body, content, jsonOptions.Value)
    }

let Middleware (options: WebSharperOptions) =
    let sitelet =
        match options.Services.GetService(typeof<ISiteletService>) with
        | :? ISiteletService as s ->
            Some s.Sitelet
        | _ -> 
            failwith "ISiteletService not found. Use AddSitelet in your ConfigureServices."
    let wsService = 
        match options.Services.GetService(typeof<IWebSharperService>) with
        | :? IWebSharperService as s -> s
        | _ ->
            failwith "IWebSharperService not found. Use AddSitelet in your ConfigureServices."
    match sitelet with
    | None ->
        Func<_,_,_>(fun (_: HttpContext) (next: Func<Task>) -> next.Invoke())
    | Some sitelet ->
        Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
            let ctx = Context.GetOrMake httpCtx wsService options.ContentRootPath sitelet
            httpCtx.Items.Add("WebSharper.Sitelets.Context", ctx)
            
            let handleRouterResult r =
               match r with
               | Some endpoint ->
                   let content = sitelet.Controller.Handle endpoint
                   contentHelper httpCtx content
               | None -> next.Invoke()
           

            let routeWithoutBody =
                try
                    Some (sitelet.Router.Route ctx.Request)
                with :? Router.BodyTextNeededForRoute ->
                    None 

            match routeWithoutBody with
            | Some r ->
                handleRouterResult r
            | None -> 
                task {
                    let! _ = ctx.Request.BodyText
                    let routeWithBody = sitelet.Router.Route ctx.Request
                    return! handleRouterResult routeWithBody
                }
        )

// Giraffe/Saturn helpers

open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Mvc.Abstractions
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
    
type SiteletHttpFuncResult = Task<HttpContext option>
type SiteletHttpFunc =  HttpContext -> SiteletHttpFuncResult
type SiteletHttpHandler = SiteletHttpFunc -> SiteletHttpFunc

let HttpHandler (sitelet : Sitelet<'T>) : SiteletHttpHandler =
    fun (next: SiteletHttpFunc) ->
        let handleSitelet (httpCtx: HttpContext) =
            let wsService = httpCtx.RequestServices.GetService(typeof<IWebSharperService>) :?> IWebSharperService
            let hostingEnv = httpCtx.RequestServices.GetService(typeof<IHostingEnvironment>) :?> IHostingEnvironment 
            let ctx = Context.GetOrMake httpCtx wsService hostingEnv.ContentRootPath sitelet
            httpCtx.Items.Add("WebSharper.Sitelets.Context", ctx)

            let handleRouterResult r =
                match r with
                | Some endpoint ->
                    let content = sitelet.Controller.Handle endpoint
                    task {
                        do! contentHelper httpCtx content
                        return None
                    }
                | None ->
                    next httpCtx

            let routeWithoutBody =
                try
                    Some (sitelet.Router.Route ctx.Request)
                with :? Router.BodyTextNeededForRoute ->
                    None 

            match routeWithoutBody with
            | Some r ->
                handleRouterResult r
            | None -> 
                task {
                    let! _ = ctx.Request.BodyText
                    let routeWithBody = sitelet.Router.Route ctx.Request
                    return! handleRouterResult routeWithBody
                }

        handleSitelet