// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper

open WebSharper.Sitelets

module SPA =
    type EndPoint =
        | [<EndPoint "/">] Home

namespace WebSharper

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open WebSharper.Sitelets

[<Class>]
type Application =

    static member MultiPage f = Sitelet.Infer f

    static member SinglePage (f: Context<SPA.EndPoint> -> Async<Content<SPA.EndPoint>>) =
        {
            Router = Router.Single SPA.EndPoint.Home "/"
            Controller =
                { Handle = fun SPA.EndPoint.Home ->
                    Content.CustomContentAsync <| fun ctx -> async {
                        let! x = f ctx
                        return! Content.ToResponse x ctx
                    }
                }
        }

    static member Text (t: Context<SPA.EndPoint> -> string) : Sitelet<SPA.EndPoint> =
        Application.SinglePage (fun context -> Content.Text (t context))

    static member SinglePage (f: Func<Context<SPA.EndPoint>, Task<Content<SPA.EndPoint>>>) : Sitelet<SPA.EndPoint> =
        Application.SinglePage (fun ctx -> f.Invoke ctx |> Async.AwaitTask)

    static member MultiPage (f: Func<Context<'EndPoint>, 'EndPoint, Task<Content<'EndPoint>>>) : Sitelet<'EndPoint> =
        Application.MultiPage (fun ctx ep -> f.Invoke(ctx, ep) |> Async.AwaitTask)

    static member Text (f: Func<Context<SPA.EndPoint>, string>) : Sitelet<SPA.EndPoint> =
        Application.SinglePage (fun context -> Content.Text (f.Invoke context))