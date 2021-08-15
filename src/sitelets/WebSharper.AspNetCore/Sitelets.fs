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
open WebSharper.Sitelets

let private writeResponseAsync (resp: Http.Response) (out: HttpResponse) : Async<unit> =
    async {
        use memStr = new MemoryStream()
        do
            out.StatusCode <- resp.Status.Code
            for name, hs in resp.Headers |> Seq.groupBy (fun h -> h.Name) do
                let values =
                    [| for h in hs -> h.Value |]
                    |> Microsoft.Extensions.Primitives.StringValues
                out.Headers.Append(name, values)
            resp.WriteBody(memStr :> Stream)
            memStr.Seek(0L, SeekOrigin.Begin) |> ignore
        do! memStr.CopyToAsync(out.Body) |> Async.AwaitTask    
    }

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
            
            let handleRouterResult r =
                match r with
                | Some endpoint ->
                    async {
                        let content = sitelet.Controller.Handle endpoint
                        let! response = Content.ToResponse content ctx
                        do! writeResponseAsync response httpCtx.Response
                    }
                    |> Async.StartAsTask :> Task
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
