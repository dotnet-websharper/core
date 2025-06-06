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
open Microsoft.Extensions.Logging

let internal writeStatusCodeAndHeader (httpCtx: HttpContext) (rsp: Http.Response) =
    let httpResponse = httpCtx.Response
    httpResponse.StatusCode <- rsp.Status.Code
    for name, hs in rsp.Headers |> Seq.groupBy (fun h -> h.Name) do
        let values =
            [| for h in hs -> h.Value |]
            |> Microsoft.Extensions.Primitives.StringValues
        httpResponse.Headers.Append(name, values)

let rec internal mvcContentHelper (httpCtx: HttpContext) (context: Context<obj>) (content: obj) : Task =
    task {
        match content with
        | :? string as stringContent ->
            httpCtx.Response.StatusCode <- StatusCodes.Status200OK
            do! httpCtx.Response.WriteAsync(stringContent)
        | :? Content<obj> as wsContent ->
            return! contentHelper httpCtx context wsContent
        | :? CSharpContent as wsCSharpContent ->
            return! contentHelper httpCtx context wsCSharpContent.AsContent
        | :? IActionResult as actionResult ->
            let actionCtx = ActionContext(httpCtx, RouteData(), ActionDescriptor())
            do! actionResult.ExecuteResultAsync(actionCtx)
        | _ ->
            let contentType = content.GetType()
            if contentType.IsGenericType && contentType.GetGenericTypeDefinition() = typedefof<Task<_>> then
                let contentTask = content :?> Task
                do! contentTask
                let contentResult =
                    let resultGetter = contentType.GetProperty("Result")
                    resultGetter.GetMethod.Invoke(contentTask, [||])
                return! mvcContentHelper httpCtx context contentResult
            else
                httpCtx.Response.StatusCode <- StatusCodes.Status200OK
                let jsonOptions = httpCtx.RequestServices.GetService(typeof<IOptions<JsonSerializerOptions>>) :?> IOptions<JsonSerializerOptions>;
                do! System.Text.Json.JsonSerializer.SerializeAsync(httpCtx.Response.Body, content, jsonOptions.Value)
    }

and internal contentHelper (httpCtx: HttpContext) (context: Context<obj>) (content: Content<obj>) : Task =
    task {
        let! rsp = Content<obj>.ToResponse content context
        match rsp.WriteBody with
        | Http.EmptyBody ->
            writeStatusCodeAndHeader httpCtx rsp
        | Http.WriteBody write ->
            writeStatusCodeAndHeader httpCtx rsp
            // synchronous writes are not allowed to response stream, writing to memory stream first
            let memStr = new MemoryStream()
            write memStr
            memStr.Seek(0L, SeekOrigin.Begin) |> ignore
            do! memStr.CopyToAsync(httpCtx.Response.Body)
        | Http.WriteBodyAsync write ->
            writeStatusCodeAndHeader httpCtx rsp
            do! write httpCtx.Response.Body
        | Http.MvcBody mvcObj ->
            do! mvcContentHelper httpCtx context mvcObj
    }

let Middleware (options: WebSharperOptions) =
    match options.Sitelet with
    | None ->
        Func<_,_,_>(fun (_: HttpContext) (next: Func<Task>) -> next.Invoke())
    | Some sitelet ->
        if options.Logger.IsEnabled(LogLevel.Debug) then

            let timedDebug (message: string) action =
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let r = action()
                options.Logger.LogDebug("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                r

            let timedDebugAsync (message: string) (action: unit -> Task) =
                task {
                    let sw = System.Diagnostics.Stopwatch()
                    sw.Start()
                    do! action()
                    options.Logger.LogDebug("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                } :> Task

            Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
                let ctx = Context.GetOrMake httpCtx options sitelet
            
                let handleRouterResult r =
                   match r with
                   | Some endpoint ->
                        let content = timedDebug "Handling endpoint" (fun () -> sitelet.Controller.Handle endpoint)
                        timedDebugAsync "Writing response" (fun () -> contentHelper httpCtx ctx content)
                   | None -> next.Invoke()

                let routeWithoutBody =
                    try
                        Some (timedDebug "Routing request" (fun () -> sitelet.Router.Route ctx.Request))
                    with :? Router.BodyTextNeededForRoute ->
                        None 

                match routeWithoutBody with
                | Some r ->
                    handleRouterResult r
                | None -> 
                    task {
                        let! _ = ctx.Request.BodyText
                        let routeWithBody = timedDebug "Routing request using content body" (fun () -> sitelet.Router.Route ctx.Request)
                        return! handleRouterResult routeWithBody
                    }
            )
        else
            Func<_,_,_>(fun (httpCtx: HttpContext) (next: Func<Task>) ->
                let ctx = Context.GetOrMake httpCtx options sitelet
            
                let handleRouterResult r =
                   match r with
                   | Some endpoint ->
                       let content = sitelet.Controller.Handle endpoint
                       contentHelper httpCtx ctx content
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

open Microsoft.AspNetCore.Hosting
open System.Reflection
    
type SiteletHttpFuncResult = Task<HttpContext option>
type SiteletHttpFunc =  HttpContext -> SiteletHttpFuncResult
type SiteletHttpHandler = SiteletHttpFunc -> SiteletHttpFunc

let HttpHandler (sitelet : Sitelet<'T>) : SiteletHttpHandler =
    let sitelet = sitelet.Box()
    fun (next: SiteletHttpFunc) ->
        let handleSitelet (httpCtx: HttpContext) =
            let options =
                match httpCtx.RequestServices.GetService(typeof<IWebSharperService>) with
                | :? IWebSharperService as s -> s.WebSharperOptions
                | _ ->
                    WebSharperBuilder(httpCtx.RequestServices)
                        .Sitelet(sitelet)
                        .UseRemoting(false)
                        .Build()
            let ctx = Context.GetOrMake httpCtx options sitelet

            let handleRouterResult r =
                match r with
                | Some endpoint ->
                    let content = sitelet.Controller.Handle endpoint
                    task {
                        do! contentHelper httpCtx ctx content
                        return! next httpCtx
                    }
                | None ->
                    Task.FromResult None

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
