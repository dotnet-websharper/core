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

/// Implements utilities for use by the current assembly.
namespace WebSharper.Sitelets

[<AutoOpen>]
module internal Extensions =
    open System.Diagnostics

    let private source =
        TraceSource("WebSharper", SourceLevels.All)

    let Timed message action =
        let sw = Stopwatch()
        sw.Start()
        let r = action()
        source.TraceInformation("{0} in {1} sec.",
            message, sw.Elapsed.TotalSeconds)
        r

    let joinWithSlash (a: string) (b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

module internal ResourceContext =
    open System
    open System.Collections.Generic
    open System.Configuration
    open System.Diagnostics
    open System.IO
    open System.Reflection
    open System.Web
    open System.Web.Hosting
    module M = WebSharper.Core.Metadata
    module R = WebSharper.Core.Remoting
    module Re = WebSharper.Core.Resources
    module P = WebSharper.PathConventions

    let MetaData () : M.Info =
        WebSharper.Web.Shared.Metadata

    let SharedJson () =
        WebSharper.Web.Shared.Json

    let ResourceContext (appPath: string) : Re.Context =
        let page = new UI.Page()
        let isDebug = HttpContext.Current.IsDebuggingEnabled
        let pu = P.PathUtility.VirtualPaths(appPath)
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match ConfigurationManager.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let aid = P.AssemblyId.Create(name.FullName)
                let url = if isDebug then pu.JavaScriptPath(aid) else pu.MinifiedJavaScriptPath(aid)
                Re.RenderLink url
            GetWebResourceRendering = fun ty resource ->
                let id = P.AssemblyId.Create(ty)
                let kind =
                    if resource.EndsWith(".js") || resource.EndsWith(".ts")
                        then P.ResourceKind.Script
                        else P.ResourceKind.Content
                P.EmbeddedResource.Create(kind, id, resource)
                |> pu.EmbeddedPath
                |> Re.RenderLink
        }
