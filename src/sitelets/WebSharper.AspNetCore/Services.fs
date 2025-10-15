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

open System
open System.Collections.Generic
open System.Configuration
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyInjection
open WebSharper.Sitelets
open WebSharper.Core
open WebSharper.Core.DependencyGraph
open WebSharper.Constants
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Html
open System.IO
open WebSharper.Core.Resources

module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

type WebSharperService(defaultAssembly) =

    let metaCache = Dictionary<Assembly, M.Info * Graph * Json.Provider>()
    let mutable options = Unchecked.defaultof<WebSharperOptions>

    interface IWebSharperService with
        member this.GetWebSharperMeta (siteletAssembly: Assembly, logger: ILogger) =

            match metaCache.TryGetValue(siteletAssembly) with
            | true, res -> res
            | false, _ ->
                let timedInfo (message: string) action =
                    if logger.IsEnabled(LogLevel.Information) then
                        let sw = System.Diagnostics.Stopwatch()
                        sw.Start()
                        let r = action()
                        logger.LogInformation("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                        r
                    else
                        action()

                let metadata, dependencies = 
                    timedInfo "Initialized WebSharper" <| fun () ->
                        let runtimeMeta =
                            siteletAssembly
                            |> M.IO.LoadRuntimeMetadata
                        match runtimeMeta with
                        | None ->
                            logger.LogWarning("Runtime WebSharper metadata not found.")
                            M.Info.Empty, Graph.Empty 
                        | Some meta ->
                            meta, Graph.FromData meta.Dependencies

                let res = metadata, dependencies, J.Provider.Create()
                metaCache.Add(siteletAssembly, res)
                res

        member this.DefaultAssembly = defaultAssembly

        member this.WebSharperOptions 
            with get() = options
            and set v = options <- v  
            
/// Define the default sitelet to serve by WebSharper.
[<AbstractClass>]
type SiteletService<'T when 'T : equality>() =
    abstract Sitelet : Sitelet<'T>

    interface ISiteletService with
        member this.Sitelet = Sitelet.Box this.Sitelet

type DefaultSiteletService<'T when 'T : equality>(sitelet: Sitelet<'T>) =
    inherit SiteletService<'T>()

    override this.Sitelet = sitelet

type SiteletRefService<'T when 'T : equality>(sitelet: ref<Sitelet<'T>>) =
    inherit SiteletService<'T>()

    override this.Sitelet = sitelet.Value

type RemotingService<'THandler, 'TInstance>(handler: 'TInstance) =
    interface IRemotingService<'THandler> with
        member this.Handler = (box handler)

type WebSharperContentService(httpCtx: IHttpContextAccessor, wsService: IWebSharperService) =
    let ctx = Context.GetOrMakeSimple httpCtx.HttpContext wsService.WebSharperOptions
    let requires = ResizeArray()
    let uidSource = UniqueIdSource()
    
    interface IWebSharperContentService with
        member this.Head() =
            HtmlString(ctx.GetResourcesAndScripts(requires))
        
        member this.Render(node) =
            requires.AddRange(node.Requires(ctx.Metadata, ctx.Json, uidSource))

            use sw = new StringWriter()
            use tw = new HtmlTextWriter(sw)
            node.Write(ctx, tw)
            HtmlString(sw.ToString())

[<Extension>]
type ServiceExtensions =

    /// <summary>
    /// Adds a required service to serve as metadata cache for WebSharper.
    /// Automatically added by any other AddWebSharper... methods.
    /// </summary>
    [<Extension>]
    static member AddWebSharper(this: IServiceCollection, [<Optional; DefaultParameterValue(null: Assembly)>] defaultAssembly: Assembly) =
        if this |> Seq.exists (fun s -> s.ServiceType = typeof<IWebSharperService>) |> not then
            let defaultAssembly = 
                if isNull defaultAssembly then Assembly.GetCallingAssembly() else defaultAssembly
            this.AddSingleton<IWebSharperService>(WebSharperService(defaultAssembly)) |> ignore
        this

    /// <summary>
    /// Add a sitelet service to be used with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'TImplementation
            when 'TImplementation :> ISiteletService
            and 'TImplementation : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<ISiteletService, 'TImplementation>()

    /// <summary>
    /// Add a sitelet reference to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    [<Obsolete "Use builder.Sitelet inside app.AddWebSharper instead for faster sitelet lookup.">]
    static member AddSitelet<'T when 'T : equality>
            (this: IServiceCollection, sitelet: Sitelet<'T>) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<ISiteletService>(DefaultSiteletService sitelet)

    /// <summary>
    /// Add a sitelet reference to be used with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'T when 'T : equality>
            (this: IServiceCollection, sitelet: ref<Sitelet<'T>>) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<ISiteletService>(SiteletRefService sitelet)

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<'THandler, 'THandler>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'THandler>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler, 'TInstance when 'TInstance : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<'TInstance, 'TInstance>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'TInstance>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection, handler: 'THandler) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddSingleton<'THandler>(handler)
            .AddSingleton<IRemotingService<'THandler>>(RemotingService handler)

    /// <summary>
    /// Adds a service allow embedding WebSharper content into Razor pages.
    /// </summary>
    [<Extension>]
    static member AddWebSharperContent(this: IServiceCollection) =
        this.AddWebSharper(Assembly.GetCallingAssembly())
            .AddScoped<IWebSharperContentService, WebSharperContentService>()
