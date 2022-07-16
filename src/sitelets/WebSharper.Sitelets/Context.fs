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

namespace WebSharper.Sitelets

open System.Collections.Generic

type Context<'Action>
    (
        ApplicationPath : string,
        Link : 'Action -> string,
        Json : WebSharper.Core.Json.Provider,
        Metadata : WebSharper.Core.Metadata.Info,
        Dependencies : WebSharper.Core.DependencyGraph.Graph,
        ResourceContext : WebSharper.Core.Resources.Context,
        Request : Http.Request,
        RootFolder : string,
        WebRootFolder : string,
        UserSession : WebSharper.Web.IUserSession,
        Environment : IDictionary<string, obj>
    ) =

    inherit WebSharper.Web.Context()
    member this.Link(e) = Link e
    override this.ApplicationPath = ApplicationPath
    override this.Json = Json
    override this.Metadata = Metadata
    override this.Dependencies = Dependencies
    override this.ResourceContext = ResourceContext
    member this.Request = Request
    override this.RequestUri = Request.Uri
    override this.RootFolder = RootFolder
    override this.WebRootFolder = WebRootFolder
    override this.UserSession = UserSession
    override this.Environment = Environment

type Context(ctx: Context<obj>) =
    inherit Context<obj>(ctx.ApplicationPath, ctx.Link, ctx.Json, ctx.Metadata, ctx.Dependencies,
        ctx.ResourceContext, ctx.Request, ctx.RootFolder, ctx.WebRootFolder, ctx.UserSession, ctx.Environment)

    static member Map (f: 'T2 -> 'T1) (ctx: Context<'T1>) : Context<'T2> =
        Context<'T2>(
            ApplicationPath = ctx.ApplicationPath,
            Link = (ctx.Link << f),
            Json = ctx.Json,
            Metadata = ctx.Metadata,
            Dependencies = ctx.Dependencies,
            ResourceContext = ctx.ResourceContext,
            Request = ctx.Request,
            RootFolder = ctx.RootFolder,
            WebRootFolder = ctx.WebRootFolder,
            UserSession = ctx.UserSession,
            Environment = ctx.Environment
        )

    static member WithSettings (settings: seq<string * string>) (ctx: Context<'T>) : Context<'T> =
        let settings = dict settings
        let getSetting k =
            match settings.TryGetValue(k) with
            | true, x -> Some x
            | false, _ -> ctx.ResourceContext.GetSetting k
        Context<'T>(
            ctx.ApplicationPath,
            ctx.Link,
            ctx.Json,
            ctx.Metadata,
            ctx.Dependencies,
            { ctx.ResourceContext with GetSetting = getSetting },
            ctx.Request,
            ctx.RootFolder,
            ctx.WebRootFolder,
            ctx.UserSession,
            ctx.Environment
        )
