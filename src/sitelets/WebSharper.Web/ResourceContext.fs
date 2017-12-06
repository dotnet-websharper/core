namespace WebSharper.Web

module ResourceContext =
    open System
    open System.Collections.Generic
    open System.Collections.Concurrent
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

    let private contextCache = ConcurrentDictionary<string, Re.Context>()

    let ResourceContext (appPath: string) : Re.Context =
        contextCache.GetOrAdd(appPath, fun appPath ->
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
                    let aid = P.AssemblyId.Create(name)
                    let url = if isDebug then pu.JavaScriptPath(aid) else pu.MinifiedJavaScriptPath(aid)
                    let version = 
                        let fileName = if isDebug then pu.JavaScriptFileName(aid) else pu.MinifiedJavaScriptFileName(aid)
                        match Shared.Metadata.ResourceHashes.TryGetValue(fileName) with
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
                        match Shared.Metadata.ResourceHashes.TryGetValue(pu.EmbeddedResourceKey r) with
                        | true, h -> "?h=" + string h
                        | _ -> ""
                    Re.RenderLink (url + version)
                WebRoot = VirtualPathUtility.AppendTrailingSlash(appPath)
                RenderingCache = System.Collections.Concurrent.ConcurrentDictionary()
                ResourceDependencyCache = System.Collections.Concurrent.ConcurrentDictionary()
            } : Re.Context
        )
