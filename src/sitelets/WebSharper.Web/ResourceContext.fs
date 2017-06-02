namespace WebSharper.Web

module ResourceContext =
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

    let ResourceContext (appPath: string) : Re.Context =
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
            RenderingCache = System.Collections.Concurrent.ConcurrentDictionary()
            ResourceDependencyCache = System.Collections.Concurrent.ConcurrentDictionary()
        }
