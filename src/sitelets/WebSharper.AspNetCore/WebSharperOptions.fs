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

module Res = WebSharper.Core.Resources
module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

[<Sealed>]
type WebSharperOptions
    internal 
    (
        services: IServiceProvider,
        contentRoot: string,
        webRoot: string,
        isDebug: bool,
        useSitelets: bool,
        useRemoting: bool,
        useExtension: IApplicationBuilder -> WebSharperOptions -> unit
    ) =

    member this.Services = services

    member this.UseSitelets = useSitelets

    member this.UseRemoting = useRemoting

    member this.IsDebug = isDebug

    member this.WebRootPath = webRoot

    member this.ContentRootPath = contentRoot

    member internal this.UseExtension = useExtension

    static member Create
        (
            services: IServiceProvider,
            [<Optional>] logger: ILogger,
            [<Optional>] binDir: string,
            [<Optional; DefaultParameterValue true>] useSitelets: bool,
            [<Optional; DefaultParameterValue true>] useRemoting: bool
        ) =
        WebSharperOptions.Create(services, Option.ofObj logger, Option.ofObj binDir, useSitelets, useRemoting, fun _ _ -> ())

    static member internal Create
        (
            services: IServiceProvider,
            logger: option<ILogger>,
            binDir: option<string>,
            useSitelets: bool,
            useRemoting: bool,
            useExtension
        ) =
        let env = services.GetRequiredService<IHostingEnvironment>()
        //Context.IsDebug <- env.IsDevelopment
        //Context.GetSetting <- fun key -> Option.ofObj config.[key]
        WebSharperOptions(services, env.ContentRootPath, env.WebRootPath, env.IsDevelopment(), useSitelets, useRemoting, useExtension)

/// Defines settings for a WebSharper application.
type WebSharperBuilder(services: IServiceProvider) =
    let mutable _logger = None
    let mutable _binDir = None
    let mutable _useSitelets = true
    let mutable _useRemoting = true
    let mutable _useExtension = fun _ _ -> ()

    /// <summary>Defines the logger for WebSharper internal messages.</summary>
    member this.Logger(logger: ILogger) =
        _logger <- Some logger
        this

    /// <summary>Defines the logger factory for WebSharper internal messages.</summary>
    member this.Logger(loggerFactory: ILoggerFactory) =
        _logger <- Some (loggerFactory.CreateLogger<WebSharperOptions>() :> ILogger)
        this

    /// <summary>Defines the directory to look for assemblies with WebSharper metadata.</summary>
    /// <remarks>Default: the directory where WebSharper.AspNetCore.dll is located.</remarks>
    member this.BinDir(binDir: string) =
        _binDir <- Some binDir
        this

    /// <summary>Defines whether to serve Sitelets.</summary>
    /// <remarks>
    /// <para>
    /// If true and the Sitelet is neither defined here nor in <c>ConfigureServices</c>,
    /// looks for a Sitelet marked with <c>WebsiteAttribute</c> in the loaded assemblies.
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

    /// <summary>Adds an extra configuration step to execute that gets final the <c>WebSharperOptions</c> instance.</summary>
    member this.Use(useExtension: Func<IApplicationBuilder, WebSharperOptions, unit>) =
        let prev = _useExtension
        _useExtension <- 
            fun appBuilder options ->
                prev appBuilder options
                useExtension.Invoke(appBuilder, options)

    /// Builds WebSharper options.
    member internal this.Build() =
        WebSharperOptions.Create(services, _logger, _binDir, _useSitelets, _useRemoting, _useExtension)
