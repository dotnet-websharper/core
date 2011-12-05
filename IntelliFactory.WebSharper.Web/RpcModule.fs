// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module R = IntelliFactory.WebSharper.Core.Remoting

/// The WebSharper RPC HttpModule. Handles RPC requests.
[<Sealed>]
type RpcModule() =
    let server = R.Server.Create None Shared.Metadata

    interface System.Web.IHttpModule with

        member this.Init(app: System.Web.HttpApplication) =
            app.add_BeginRequest(System.EventHandler(fun obj args ->
                let req = app.Request
                let getHeader x =
                    match req.Headers.Item(x: string) with
                    | null  -> None
                    | v     -> Some v
                if R.IsRemotingRequest getHeader then
                    let body =
                        use s = new System.IO.StreamReader(req.InputStream)
                        s.ReadToEnd()
                    let response =
                        server.HandleRequest {Headers = getHeader; Body = body}
                    app.Response.ContentType <- response.ContentType
                    app.Response.Write response.Content
                    app.Response.End()))

        member this.Dispose() = ()

