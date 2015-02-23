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

namespace WebSharper.Sitelets

type Context<'Action> =
    {
        ApplicationPath : string
        Link : 'Action -> string
        Json : WebSharper.Core.Json.Provider
        Metadata : WebSharper.Core.Metadata.Info
        ResolveUrl : string -> string
        ResourceContext : WebSharper.Core.Resources.Context
        Request : Http.Request
        RootFolder : string
        UserSession : WebSharper.Web.IUserSession
    }

    interface WebSharper.Web.IContext with
        member this.RequestUri = this.Request.Uri
        member this.RootFolder = this.RootFolder
        member this.UserSession = this.UserSession

module Context =

    let Map (f: 'T2 -> 'T1) (ctx: Context<'T1>) : Context<'T2> =
        {
            ApplicationPath = ctx.ApplicationPath
            Link = ctx.Link << f
            Json = ctx.Json
            Metadata = ctx.Metadata
            ResolveUrl = ctx.ResolveUrl
            ResourceContext = ctx.ResourceContext
            Request = ctx.Request
            RootFolder = ctx.RootFolder
            UserSession = ctx.UserSession
        }
