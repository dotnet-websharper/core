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

[<AutoOpen>]
module internal ContentExtensionsHelpers =
    let createDefaultOptions services = 
        WebSharperBuilder(services)
            .UseSitelets(false)
            .UseRemoting(false)
            .Build()

[<Extension>]
type ContentExtensions =

    /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: Content<'T>, httpCtx: HttpContext) =
        task {
            let options = createDefaultOptions httpCtx.RequestServices
            let ctx = Context.GetOrMake httpCtx options Sitelet.Empty
            do! Sitelets.contentHelper httpCtx ctx (this.Box())
        } :> Task

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Content<'T>) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                ContentExtensions.HandleRequest(this, context.HttpContext)
        }

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Async<Content<'T>>) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                task {
                    let! c = this
                    do! ContentExtensions.HandleRequest(c, context.HttpContext)
                }
        }

     /// <summary>Use WebSharper content to handle an ASP.NET Core request.</summary>
    [<Extension>]
    static member HandleRequest (this: CSharpContent, httpCtx: HttpContext) =
        ContentExtensions.HandleRequest(this.AsContent, httpCtx)

   /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: CSharpContent) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) : Task =
                ContentExtensions.HandleRequest(this, context.HttpContext)
        }

    /// <summary>Convert WebSharper Content into ASP.NET Core IActionResult.</summary>
    [<Extension>]
    static member ToIActionResult (this: Task<CSharpContent>) =
        { new IActionResult with
            member x.ExecuteResultAsync (context: ActionContext) =
                task {
                    let! c = this
                    do! ContentExtensions.HandleRequest(c, context.HttpContext)
                }
        }
