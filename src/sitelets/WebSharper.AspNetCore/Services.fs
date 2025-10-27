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
open Microsoft.Extensions.Options
open Microsoft.AspNetCore.Hosting

module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

type SiteletRefService<'T when 'T: equality>(sitelet: ref<Sitelet<'T>>) =

    interface IWebSharperSiteletService with
        member this.Sitelet = Sitelet.Box sitelet.Value

type RuntimeRefService(runtime: ref<Sitelet<obj> * M.Info * Graph * Json.Provider * Remoting.Server>) =

    interface IWebSharperSiteletService with
        member this.Sitelet = 
            let s, _, _, _, _ = runtime.Value
            s

    interface IWebSharperMetadataService with
        member this.Metadata =
            let _, m, _, _, _ = runtime.Value
            m

        member this.Graph =
            let _, _, g, _, _ = runtime.Value
            g

    interface IWebSharperJsonProviderService with
        member this.JsonProvider =
            let _, _, _, j, _ = runtime.Value
            j

    interface IWebSharperRemotingServerService with
        member this.RemotingServer =
            let _, _, _, _, s = runtime.Value
            s

type RemotingService<'THandler, 'TInstance>(handler: 'TInstance) =
    interface IWebSharperRemotingService<'THandler> with
        member this.Handler = (box handler)

type WebSharperContentService(httpCtx: IHttpContextAccessor, cacheService: IWebSharperInitializationService) =
    let ctx = Context.GetOrMakeSimple httpCtx.HttpContext cacheService
    let requires = ResizeArray()
    let uidSource = UniqueIdSource()
    
    interface IWebSharperMvcService with
        member this.Head() =
            HtmlString(ctx.GetResourcesAndScripts(requires))
        
        member this.Render(node) =
            requires.AddRange(node.Requires(ctx.Metadata, ctx.Json, uidSource))

            use sw = new StringWriter()
            use tw = new HtmlTextWriter(sw)
            node.Write(ctx, tw)
            HtmlString(sw.ToString())

type WebSharperPostConfigureOptions(services: IServiceProvider) =

    interface IPostConfigureOptions<WebSharperOptions> with
        member this.PostConfigure (name, options): unit = 
            if isNull options.DefaultAssembly then
                options.DefaultAssembly <- Assembly.GetEntryAssembly()
            
            if isNull options.Configuration then
                options.Configuration <-
                    services.GetRequiredService<IConfiguration>().GetSection("websharper") :> IConfiguration 
            
            let hostingEnvironment =
                lazy services.GetRequiredService<IHostingEnvironment>()

            options.ContentRootPath <- hostingEnvironment.Value.ContentRootPath

            options.WebRootPath <- hostingEnvironment.Value.WebRootPath

type WebSharperInitializationService(
    options: IOptions<WebSharperOptions>, 
    services: IServiceProvider,
    logger: ILogger<WebSharperInitializationService>,
    [<Optional; DefaultParameterValue(null: IWebSharperMetadataService)>] metadataService: IWebSharperMetadataService,
    [<Optional; DefaultParameterValue(null: IWebSharperJsonProviderService)>] jsonService: IWebSharperJsonProviderService,
    [<Optional; DefaultParameterValue(null: IWebSharperRemotingServerService)>] remotingServerService: IWebSharperRemotingServerService
 ) =
    let wsOptions = options.Value 
    let mutable metaAndGraph = None
    let mutable json = None
    let mutable remotingServer = None
    
    member this.Initialize() = 
        let timedInfo (message: string) action =
            if logger.IsEnabled(LogLevel.Information) then
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let r = action()
                logger.LogInformation("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                r
            else
                action()

        if isNull metadataService then
            timedInfo "Initialized WebSharper" <| fun () ->
                let runtimeMeta =
                    wsOptions.DefaultAssembly
                    |> M.IO.LoadRuntimeMetadata
                match runtimeMeta with
                | None ->
                    logger.LogWarning("Runtime WebSharper metadata not found.")
                | Some meta ->
                    metaAndGraph <- Some (meta, Graph.FromData meta.Dependencies)

        if isNull jsonService then
            json <- Some WebSharper.Json.ServerSideProvider

        if isNull remotingServerService then
            match metaAndGraph with
            | Some (meta, _) ->
                remotingServer <- 
                    Some (Remoting.Server.Create meta WebSharper.Json.ServerSideProvider (Context.getRemotingHandler services))
            | _ -> ()

    interface IWebSharperInitializationService with
        member this.Options = wsOptions
        member this.MetadataAndGraph = metaAndGraph
        member this.JsonProvider = json
        member this.RemotingServer = remotingServer

type ConfigureWebSharper internal (services: IServiceCollection) =
    
    member this.Services = services

    /// Add a reference as the default sitelet.
    member this.AddSiteletRef<'T when 'T: equality>(siteletRef: ref<Sitelet<'T>>) =
        services.AddSingleton<IWebSharperSiteletService>(SiteletRefService siteletRef)

    /// Adds a reference for full runtime objects used by WebSharper.
    member this.AddRuntimeRef(runtime) =
        let s = RuntimeRefService runtime
        services
            .AddSingleton<IWebSharperSiteletService>(s)
            .AddSingleton<IWebSharperMetadataService>(s)
            .AddSingleton<IWebSharperRemotingServerService>(s)

    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    member this.AddRemotingHandler<'THandler when 'THandler : not struct>() =
        services
            .AddSingleton<'THandler, 'THandler>()
            .AddSingleton<IWebSharperRemotingService<'THandler>, RemotingService<'THandler, 'THandler>>()

    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    member this.AddRemotingHandler<'THandler, 'TInstance when 'TInstance : not struct>() =
        services
            .AddSingleton<'TInstance, 'TInstance>()
            .AddSingleton<IWebSharperRemotingService<'THandler>, RemotingService<'THandler, 'TInstance>>()

    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    member this.AddRemotingHandler<'THandler when 'THandler : not struct>(handler: 'THandler) =
        services
            .AddSingleton<'THandler>(handler)
            .AddSingleton<IWebSharperRemotingService<'THandler>>(RemotingService handler)

    /// Adds a service allow embedding WebSharper content into Razor pages.
    static member AddMvc(this: IServiceCollection) =
        this.AddScoped<IWebSharperMvcService, WebSharperContentService>()

[<Extension>]
type ServiceExtensions =

    /// <summary>
    /// Sets up WebSharper options and initialization.
    /// </summary>
    [<Extension>]
    static member AddWebSharper(this: IServiceCollection, [<Optional; DefaultParameterValue(null: Action<WebSharperOptions>)>] configureOptions: Action<WebSharperOptions>) =
        if not (isNull configureOptions) then
            this.Configure<WebSharperOptions>(configureOptions) |> ignore
        this
            .AddSingleton<IPostConfigureOptions<WebSharperOptions>, WebSharperPostConfigureOptions>()
            .AddSingleton<IWebSharperInitializationService, WebSharperInitializationService>() 
        |> ignore

        this

    /// <summary>
    /// Sets up WebSharper options and initialization.
    /// Returns a service builder for more WebSharper service helpers.
    /// </summary>
    [<Extension>]
    static member AddWebSharperServices(this: IServiceCollection, [<Optional; DefaultParameterValue(null: Action<WebSharperOptions>)>] configureOptions: Action<WebSharperOptions>) =
        this.AddWebSharperServices(configureOptions) |> ignore
        
        ConfigureWebSharper(this)
