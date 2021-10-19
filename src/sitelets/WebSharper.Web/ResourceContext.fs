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
namespace WebSharper.Web

module ResourceContext =
    open System
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Diagnostics
    open System.IO
    open System.Reflection
    module M = WebSharper.Core.Metadata
    module R = WebSharper.Core.Remoting
    module Re = WebSharper.Core.Resources
    module P = WebSharper.PathConventions

    let private contextCache = ConcurrentDictionary<string, Re.Context>()

    let ResourceContext (appPath: string) (meta: M.Info) isDebug getSetting : Re.Context =
        contextCache.GetOrAdd(appPath, fun appPath ->
            let pu = P.PathUtility.VirtualPaths(appPath)
            {
                DebuggingEnabled = isDebug
                DefaultToHttp = false
                ScriptBaseUrl = Some (pu.ScriptBasePath + "/")
                GetSetting = getSetting
                GetAssemblyRendering = fun name ->
                    let aid = P.AssemblyId.Create(name)
                    let url = if isDebug then pu.JavaScriptPath(aid) else pu.MinifiedJavaScriptPath(aid)
                    let version = 
                        let fileName = if isDebug then pu.JavaScriptFileName(aid) else pu.MinifiedJavaScriptFileName(aid)
                        match meta.ResourceHashes.TryGetValue(fileName) with
                        | true, h -> "?h=" + string h
                        | _ -> ""
                    Re.RenderLink (url + version)
                GetWebResourceRendering = fun ty resource ->
                    let id = P.AssemblyId.Create(ty)
                    let kind =
                        if resource.EndsWith(".js") || resource.EndsWith(".ts")
                            then P.ResourceKind.Script
                            else P.ResourceKind.Content
                    let r = P.EmbeddedResource.Create(kind, id, resource)
                    let url = pu.EmbeddedPath r
                    let version = 
                        match meta.ResourceHashes.TryGetValue(pu.EmbeddedResourceKey r) with
                        | true, h -> "?h=" + string h
                        | _ -> ""
                    Re.RenderLink (url + version)
                WebRoot = appendSlash appPath
                RenderingCache = System.Collections.Concurrent.ConcurrentDictionary()
                ResourceDependencyCache = System.Collections.Concurrent.ConcurrentDictionary()
            } : Re.Context
        )
