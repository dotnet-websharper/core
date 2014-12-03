// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Web

open System
open System.IO
open System.Web
module R = IntelliFactory.WebSharper.Core.Remoting

module private RpcUtil =
    let server = R.Server.Create None Shared.Metadata

[<Sealed>]
type RpcHandler() =
    let work (ctx: HttpContext) =
        let req = ctx.Request
        let resp = ctx.Response
        let getHeader (x: string) =
            match req.Headers.[x] with
            | null -> None
            | v -> Some v
        async {
            let body =
                use s = new StreamReader(req.InputStream)
                s.ReadToEnd()
            let! response =
                RpcUtil.server.HandleRequest { Headers = getHeader; Body = body }
            do
                resp.ContentType <- response.ContentType
                resp.Write response.Content
            return
                resp.End()
        }

    let (beginPR, endPR, cancelPR) = Async.AsBeginEnd(work)

    interface SessionState.IRequiresSessionState

    interface IHttpAsyncHandler with
        member this.BeginProcessRequest(ctx, cb, d) = beginPR (ctx, cb, d)
        member this.EndProcessRequest(res) = endPR res

    interface IHttpHandler with
        member this.IsReusable = true
        member this.ProcessRequest(ctx) = this.ProcessRequest(ctx)

    member this.ProcessRequest(ctx: HttpContext) =
        work ctx
        |> Async.RunSynchronously


/// The WebSharper RPC HttpModule. Handles RPC requests.
[<Sealed>]
type RpcModule() =
    interface IHttpModule with
        member this.Init(app: HttpApplication) =
            let handler =
                new EventHandler(fun x e ->
                    let app = (x :?> HttpApplication)
                    let ctx = app.Context
                    let req = app.Request
                    let getHeader (x: string) =
                        match req.Headers.[x] with
                        | null -> None
                        | v -> Some v
                    if R.IsRemotingRequest getHeader then
                        if HttpRuntime.UsingIntegratedPipeline then
                            ctx.RemapHandler(RpcHandler())
                        else
                            ctx.Handler <- RpcHandler())
            if HttpRuntime.UsingIntegratedPipeline then
                app.add_PostAuthorizeRequest(handler)
            else
                app.add_PostMapRequestHandler(handler)
        member this.Dispose() = ()
