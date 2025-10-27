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

module Res = WebSharper.Core.Resources
module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

/// Defines settings for a WebSharper application.
type WebSharperBuilder(services: IServiceProvider) =
    let mutable _sitelet = None
    let mutable _useSitelets = true
    let mutable _discoverSitelet = false
    let mutable _useRemoting = true
    let mutable _useExtension = fun _ _ -> ()
    let mutable _rpcHeaders : (string * string) array = [||]

    /// <summary>Defines the sitelet to serve.</summary>
    member this.Sitelet<'T when 'T : equality>(sitelet: Sitelet<'T>) =
        if not _useSitelets then
            failwith "Do not call WebSharperBuilder.Sitelet with UseSitelets(false) or UseWebSharperRemoting"
        _sitelet <- Some (Sitelet.Box sitelet)
        this

    /// <summary>Serves the sitelet marked by Website attribute.</summary>
    member this.DiscoverSitelet() =
        if not _useSitelets then
            failwith "Do not call WebSharperBuilder.DiscoverSitelet with UseSitelets(false) or UseWebSharperRemoting"
        _discoverSitelet <- true
        this

    [<Obsolete "Configure DefaultAssembly in `AddWebSharper` instead.">]
    member this.SiteletAssembly(siteletAssembly: Assembly) =
        if not _useSitelets then
            failwith "Do not call WebSharperBuilder.SiteletAssembly with UseSitelets(false) or UseWebSharperRemoting"
        this

    [<Obsolete "Use `IWebSharperMetadataService` instead.">]
    member this.Metadata(meta: M.Info) =
        this

    [<Obsolete "Configure in `AddWebSharper` instead.">]
    member this.Config(config: IConfiguration) =
        this

    [<Obsolete "Configure in `AddWebSharper` instead.">]
    member this.ContentRootPath(contentRootPath: string) =
        this

    [<Obsolete "Configure in `AddWebSharper` instead.">]
    member this.WebRootPath(webRootPath: string) =
        this

    [<Obsolete "Configure `ILogger<WebSharperSiteletMiddleware>` instead.">]
    member this.Logger(logger: ILogger) =
        this

    [<Obsolete "Configure `ILogger<WebSharperSiteletMiddleware>` instead.">]
    member this.Logger(loggerFactory: ILoggerFactory) =
        this

    [<Obsolete "Configure in `AddWebSharper` instead.">]
    member this.AuthenticationScheme(scheme: string) =
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

    /// <summary>Adds an extra configuration step to execute that gets the <c>IWebSharperInitializationService</c> instance.</summary>
    member this.Use(useExtension: Action<IApplicationBuilder, IWebSharperInitializationService>) =
        let prev = _useExtension
        _useExtension <- 
            fun appBuilder initService ->
                prev appBuilder initService
                useExtension.Invoke(appBuilder, initService)
        this

    /// Builds WebSharper options.
    member internal this.Build() =


        let rpcOptions =
            if _useRemoting then
                Some _rpcHeaders
            else
                None

        let logger = 
            lazy 
            match services.GetService<ILogger<WebSharperSiteletMiddleware>>() with 
            | null -> Abstractions.NullLogger.Instance :> ILogger
            | logger -> logger :> ILogger


        let timedInfo (message: string) action =
            if logger.Value.IsEnabled(LogLevel.Information) then
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let r = action()
                logger.Value.LogInformation("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
                r
            else
                action()

        let initService = services.GetRequiredService<IWebSharperInitializationService>()
        
        let siteletOpt =
            if not _useSitelets then 
                None 
            else
                _sitelet |> Option.orElseWith (fun () ->
                    if _discoverSitelet then
                        let fromMeta =
                            match initService.MetadataAndGraph |> Option.bind (fun (meta, _) -> meta.SiteletDefinition) with
                            | Some td -> 
                                timedInfo "Sitelet discovered via metadata" (fun () ->
                                    let typ = WebSharper.Core.AST.Reflection.LoadTypeDefinition td
                                    let s, _ = Utils.GetSitelet typ ""
                                    Some s
                                )
                            | None -> None
                        fromMeta |> Option.orElseWith (fun () ->
                            let siteletAssembly = initService.Options.DefaultAssembly
                            let s =
                                timedInfo "Sitelet discovered via reflection " (fun () -> 
                                    WebSharper.Sitelets.Loading.DiscoverSitelet siteletAssembly
                                )
                            match s with
                            | None ->
                                failwithf "Failed to discover sitelet in assembly %s. Mark a static property/value with the Website attribute or specify sitelet via WebSharperBuilder.Sitelet." siteletAssembly.FullName
                            | res ->
                                logger.Value.LogWarning("WebSharper sitelet loaded via reflection. It is recommended to pass sitelet object instead in WebSharperBuilder.UseSitelet.")
                                res
                        )
                    else
                        None
                )
                |> Some

        let useExtension appBuilder = _useExtension appBuilder initService

        rpcOptions, siteletOpt, useExtension