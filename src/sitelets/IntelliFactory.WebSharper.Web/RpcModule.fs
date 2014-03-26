// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
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
