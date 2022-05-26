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
namespace WebSharper.AspNetCore

open System.Runtime.CompilerServices
open System.Reflection
open WebSharper
open WebSharper.Sitelets
open Microsoft.AspNetCore.Mvc
open System.Threading.Tasks
open Microsoft.AspNetCore.Hosting

[<Extension>]
type ContentExtensions =

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Content<'T>) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) : Task =
                let httpCtx = context.HttpContext
                let options =
                    WebSharperBuilder(httpCtx.RequestServices)
                        .UseSitelets(false)
                        .UseRemoting(false)
                        .Build(Assembly.GetCallingAssembly())
                let hostingEnv = httpCtx.RequestServices.GetService(typeof<IHostingEnvironment>) :?> IHostingEnvironment 
                let ctx = Context.GetOrMake httpCtx options Sitelet.Empty
                task {
                    let! rsp = Content<'T>.ToResponse this ctx
                    do! Sitelets.writeResponse rsp context.HttpContext.Response
                }
        }

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: CSharpContent) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) : Task =
                let httpCtx = context.HttpContext
                let options =
                    WebSharperBuilder(httpCtx.RequestServices)
                        .UseSitelets(false)
                        .UseRemoting(false)
                        .Build(Assembly.GetCallingAssembly())
                let wsService = httpCtx.RequestServices.GetService(typeof<IWebSharperService>) :?> IWebSharperService
                let hostingEnv = httpCtx.RequestServices.GetService(typeof<IHostingEnvironment>) :?> IHostingEnvironment 
                let ctx = Context.GetOrMake httpCtx options Sitelet.Empty
                task {
                    let! rsp = Content<obj>.ToResponse this.AsContent ctx
                    do! Sitelets.writeResponse rsp context.HttpContext.Response
                }
        }
