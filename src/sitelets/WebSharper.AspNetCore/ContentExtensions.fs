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
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type ContentExtensions =

    /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: Content<'T>, httpCtx: HttpContext) : Task =
        task {
            let initService = httpCtx.RequestServices.GetRequiredService<IWebSharperInitializationService>()
            let ctx = Context.GetOrMake httpCtx initService Sitelet.Empty
            do! Sitelets.contentHelper httpCtx ctx (this.Box())
        }

    /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: Async<Content<'T>>, httpCtx: HttpContext) : Task =
        task {
            let! c = this
            do! c.HandleRequest(httpCtx)
        } 

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Content<'T>) : IActionResult =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                this.HandleRequest(context.HttpContext)
        }

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Async<Content<'T>>) : IActionResult =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                this.HandleRequest(context.HttpContext)
        }

     /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: CSharpContent, httpCtx: HttpContext) : Task =
        this.AsContent.HandleRequest(httpCtx)

     /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: Task<CSharpContent>, httpCtx: HttpContext) : Task =
        task {
            let! c = this
            do! c.AsContent.HandleRequest(httpCtx)
        } :> Task

   /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: CSharpContent) : IActionResult =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) : Task =
                this.HandleRequest(context.HttpContext)
        }

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]                                            
    static member ToIActionResult (this: Task<CSharpContent>) : IActionResult =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                this.HandleRequest(context.HttpContext)
        }
