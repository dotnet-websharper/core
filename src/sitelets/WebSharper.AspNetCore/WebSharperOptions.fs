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
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open WebSharper.Core
open WebSharper.Core.DependencyGraph
open WebSharper.Sitelets
open Microsoft.AspNetCore.Builder
open WebSharper.Constants

module Res = WebSharper.Core.Resources
module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

[<Sealed>]
type WebSharperOptions
    internal 
    (
        services: IServiceProvider,
        config: IConfiguration,
        logger: ILogger,
        contentRoot: string,
        webRoot: string,
        useMinifiedScripts: bool,
        sitelet: option<Sitelet<obj>>,
        metadata: M.Info,
        dependencies: Graph,
        json: Json.Provider,
        useSitelets: bool,
        useRemoting: bool,
        useExtension: IApplicationBuilder -> WebSharperOptions -> unit,
        remotingHeaders: (string * string) []
    ) =

    member val AuthenticationScheme = "WebSharper" with get, set

    member this.Services = services

    member this.Configuration = config

    member this.Logger = logger

    member this.UseSitelets = useSitelets

    member this.UseRemoting = useRemoting

    member this.Metadata = metadata

    member this.Dependencies = dependencies

    member this.Json = json

    member this.UseMinifiedScripts = useMinifiedScripts
    
    member this.ContentRootPath = contentRoot

    member this.WebRootPath = webRoot

    member this.Sitelet = sitelet

    member this.RemotingHeaders = remotingHeaders

    member internal this.UseExtension = useExtension

/// Defines settings for a WebSharper application.
type WebSharperBuilder(services: IServiceProvider) =
    let mutable _sitelet = None
    let mutable _siteletAssembly = None
    let mutable _contentRootPath = None
    let mutable _webRootPath = None
    let mutable _metadata = None
    let mutable _config = None
    let mutable _useMinifiedScripts = None
    let mutable _logger = None
    let mutable _authScheme = None
    let mutable _useSitelets = true
    let mutable _useRemoting = true
    let mutable _useExtension = fun _ _ -> ()
    let mutable _rpcHeaders : (string * string) array = [||]

    /// <summary>Defines the sitelet to serve.</summary>
    /// <remarks>
    /// Using <c>AddSitelet</c> in <c>ConfigureServices</c> is preferred.
    /// </remarks>
    member this.Sitelet<'T when 'T : equality>(sitelet: Sitelet<'T>) =
        if not _useSitelets then
            failwith "Do not call WebSharperBuilder.Sitelet with UseSitelets(false) or UseWebSharperRemoting"
        _sitelet <- Some (Sitelet.Box sitelet)
        this

    /// <summary>Specifies which assembly contains the runtime metadata for the sitelet (WebSharper project type: web).</summary>
    /// <remarks>Default: the assembly calling the UseWebSharper method or using ANC/Giraffe helpers.</remarks>
    member this.SiteletAssembly(siteletAssembly: Assembly) =
        if not _useSitelets then
            failwith "Do not call WebSharperBuilder.SiteletAssembly with UseSitelets(false) or UseWebSharperRemoting"
        _siteletAssembly <- Some siteletAssembly
        this

    /// <summary>Overrides the default runtime metadata used by WebSharper.</summary>
    /// <remarks>Default: loaded from SiteletAssembly or entry assembly with WebSharper.Core.Metadata.IO.LoadRuntimeMetadata.</remarks>
    member this.Metadata(meta: M.Info) =
        _metadata <- Some meta
        this

    /// <summary>Defines the configuration to be used by WebSharper.</summary>
    /// <remarks>Default: the host configuration's "websharper" subsection.</remarks>
    member this.Config(config: IConfiguration) =
        _config <- Some config
        this

    /// <summary>Decides if WebSharper writes .min.js script links.</summary>
    /// <remarks>Default: the value of WebSharper::UseMinifiedScripts configuration if found, otherwise false.</remarks>
    member this.UseMinifiedScripts(useMinifiedScripts: bool) =
        _useMinifiedScripts <- Some useMinifiedScripts
        this

    /// <summary>Defines the content root path to be used by WebSharper.</summary>
    /// <remarks>Default: IHostingEnvironment.ContentRootPath.</remarks>
    member this.ContentRootPath(contentRootPath: string) =
        _contentRootPath <- Some contentRootPath
        this

    /// <summary>Defines the web root path to be used by WebSharper.</summary>
    /// <remarks>Default: IHostingEnvironment.WebRootPath.</remarks>
    member this.WebRootPath(webRootPath: string) =
        _webRootPath <- Some webRootPath
        this

    /// <summary>Defines the logger for WebSharper internal messages.</summary>
    member this.Logger(logger: ILogger) =
        _logger <- Some logger
        this

    /// <summary>Defines the logger factory for WebSharper internal messages.</summary>
    member this.Logger(loggerFactory: ILoggerFactory) =
        _logger <- Some (loggerFactory.CreateLogger<IWebSharperService>() :> ILogger)
        this

    /// <summary>Defines the name of the authentication scheme to use for <c>Web.Context.UserSession</c>.</summary>
    /// <remarks>Default: "WebSharper".</remarks>
    member this.AuthenticationScheme(scheme: string) =
        _authScheme <- Some scheme
        this

    /// <summary>Defines whether to serve Sitelets.</summary>
    /// <remarks>
    /// <para>
    /// If true and the Sitelet is neither defined here nor in <c>ConfigureServices</c>,
    /// looks for a Sitelet marked with <c>WebsiteAttribute</c> in assembly calling UseWebSharper.
    /// </para>
    /// <para>Default: true.</para>
    /// </remarks>
    member this.UseSitelets([<Optional; DefaultParameterValue true>] useSitelets: bool) =
        _useSitelets <- useSitelets
        this

    /// <summary>Defines whether to serve Remote functions.</summary>
    /// <remarks>Default: true.</remarks>
    member this.UseRemoting([<Optional; DefaultParameterValue true>] useRemoting: bool) =
        _useRemoting <- useRemoting
        this

    member this.UseRemoting([<Optional; DefaultParameterValue true>] useRemoting: bool, [<Optional; DefaultParameterValue [||]>] headers: (string * string) []) =
        _useRemoting <- useRemoting
        _rpcHeaders <- headers
        this

    /// <summary>Adds an extra configuration step to execute that gets final the <c>WebSharperOptions</c> instance.</summary>
    member this.Use(useExtension: Func<IApplicationBuilder, WebSharperOptions, unit>) =
        let prev = _useExtension
        _useExtension <- 
            fun appBuilder options ->
                prev appBuilder options
                useExtension.Invoke(appBuilder, options)

    /// Builds WebSharper options.
    member internal this.Build() =

        let config =
            _config |> Option.defaultWith (fun () ->
                services.GetRequiredService<IConfiguration>().GetSection("websharper") :> IConfiguration 
            )

        let hostingEnvironment =
            lazy
            services.GetRequiredService<IHostingEnvironment>()

        let contentRootPath = 
            _contentRootPath |> Option.defaultWith (fun () ->
                hostingEnvironment.Value.ContentRootPath
            )

        let webRootPath = 
            _webRootPath |> Option.defaultWith (fun () ->
                hostingEnvironment.Value.WebRootPath
            )

        let useMinifiedScripts =
            _useMinifiedScripts |> Option.defaultWith (fun () ->
                match bool.TryParse(config.[RUNTIMESETTING_USEMINIFIEDSCRIPTS]) with
                | true, ums -> ums
                | _ -> false
            )

        let logger =
            _logger |> Option.defaultWith (fun () ->
                match services.GetService<ILogger<IWebSharperService>>() with
                | null -> Abstractions.NullLogger.Instance
                | l -> l :> ILogger
            )

        let wsService = 
            lazy
            match services.GetService(typeof<IWebSharperService>) with
            | :? IWebSharperService as s -> s
            | _ ->
                failwith "IWebSharperService not found. Use AddWebSharper in your ConfigureServices."
        
        let siteletAssembly = 
            lazy
            _siteletAssembly |> Option.defaultWith (fun () -> wsService.Value.DefaultAssembly)

        let metadata, dependencies, json =
            match _metadata with
            | Some m ->
                m, Graph.FromData m.Dependencies, J.Provider.Create()
            | None ->
                wsService.Value.GetWebSharperMeta(siteletAssembly.Value, logger)

        let timedInfo (message: string) action =
            if logger.IsEnabled(LogLevel.Information) then
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let r = action()
                logger.LogInformation("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                r
            else
                action()

        let sitelet =
            if not _useSitelets then None else
            _sitelet |> Option.orElseWith (fun () ->
                match services.GetService(typeof<ISiteletService>) with
                | :? ISiteletService as s -> 
                    Some s.Sitelet
                | _ ->
                    let s =
                        timedInfo "Sitelet discovered via reflection " (fun () -> 
                            WebSharper.Sitelets.Loading.DiscoverSitelet siteletAssembly.Value
                        )
                    match s with
                    | None ->
                        failwithf "Failed to discover sitelet in assembly %s. Mark a static property/value with the Website attribute or specify sitelet via WebSharperBuilder.Sitelet." siteletAssembly.Value.FullName
                    | res ->
                        logger.LogWarning("WebSharper sitelet loaded via reflection. It is recommended to pass sitelet object instead in WebSharperBuilder.UseSitelet.")
                        res
            )

        WebSharperOptions(
            services, 
            config, 
            logger,
            contentRootPath, 
            webRootPath, 
            useMinifiedScripts,
            sitelet, 
            metadata, 
            dependencies,
            json,
            _useSitelets, 
            _useRemoting, 
            _useExtension,
            _rpcHeaders
        )
