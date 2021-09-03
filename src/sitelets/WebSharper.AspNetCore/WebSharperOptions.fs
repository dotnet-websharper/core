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
        sitelet: option<Sitelet<obj>>,
        siteletAssembly: option<Assembly>,
        metadata: option<M.Info>,
        useSitelets: bool,
        useRemoting: bool,
        useExtension: IApplicationBuilder -> WebSharperOptions -> unit
    ) =

    static let tryLoad(name: AssemblyName) =
        try
            match Assembly.Load(name) with
            | null -> None
            | a -> Some a
        with _ -> None

    static let loadFileInfo(p: string) =
        let fn = Path.GetFullPath p
        let name = AssemblyName.GetAssemblyName(fn)
        match tryLoad(name) with
        | None -> Assembly.LoadFrom(fn)
        | Some a -> a

    static let discoverAssemblies (path: string) =
        let ls pat = Directory.GetFiles(path, pat)
        let files = Array.append (ls "*.dll") (ls "*.exe")
        files |> Array.choose (fun p ->
            try Some (loadFileInfo(p))
            with e -> None)

    static let loadReferencedAssemblies (logger: ILogger) (alreadyLoaded: Assembly[]) =
        let loaded = Dictionary()
        let rec load (asm: Assembly) =
            for asmName in asm.GetReferencedAssemblies() do
                let name = asmName.Name
                if not (loaded.ContainsKey name) then
                    try loaded.[name] <- Assembly.Load(asmName)
                    with _ ->
                        logger.LogWarning("Failed to load {0} referenced by {1}", name, asm.GetName().Name)
        for asm in alreadyLoaded do
            loaded.[asm.GetName().Name] <- asm
        Array.ofSeq loaded.Values

    static let autoBinDir() =
        Reflection.Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName
     
    let siteletAssembly =
        siteletAssembly
        |> Option.defaultWith System.Reflection.Assembly.GetEntryAssembly

    let metadata, dependencies = 
        match metadata with
        | Some meta ->
            meta, Graph.FromData meta.Dependencies
        | None ->

            let before = System.DateTime.UtcNow
            let metadataSetting =
                Context.GetSetting "WebSharperSharedMetadata"
                |> Option.map (fun x -> x.ToLower())
            match metadataSetting with
            | Some "none" ->
                M.Info.Empty, Graph.Empty
            | _ ->
                let trace =
                    System.Diagnostics.TraceSource("WebSharper",
                        System.Diagnostics.SourceLevels.All)
                let runtimeMeta =
                    siteletAssembly 
                    |> M.IO.LoadRuntimeMetadata
                match runtimeMeta with
                | None ->
                    trace.TraceInformation("Runtime WebSharper metadata not found.")
                    M.Info.Empty, Graph.Empty 
                | Some meta ->
                    let after = System.DateTime.UtcNow
                    let res =
                        meta, Graph.FromData meta.Dependencies
                    trace.TraceInformation("Initialized WebSharper in {0} seconds.",
                        (after-before).TotalSeconds)
                    res


    let json = J.Provider.CreateTyped metadata

    member val AuthenticationScheme = "WebSharper" with get, set

    member this.Services = services

    member this.UseSitelets = useSitelets

    member this.UseRemoting = useRemoting

    member this.Metadata = metadata

    member this.Dependencies = dependencies

    member this.Json = json

    member this.IsDebug = isDebug

    member this.WebRootPath = webRoot

    member this.ContentRootPath = contentRoot

    member this.Sitelet = sitelet

    member this.SiteletAssembly = siteletAssembly

    member internal this.UseExtension = useExtension

    static member Create
        (
            services: IServiceProvider,
            [<Optional>] sitelet: Sitelet<'T>,
            [<Optional>] config: IConfiguration,
            [<Optional>] logger: ILogger,
            [<Optional>] binDir: string,
            [<Optional; DefaultParameterValue true>] useSitelets: bool,
            [<Optional; DefaultParameterValue true>] useRemoting: bool,
            [<Optional>] siteletAssembly: Assembly,
            [<Optional>] metadata: M.Info
        ) =
        let siteletOpt =
            if obj.ReferenceEquals(sitelet, null)
            then None
            else Some (Sitelet.Box sitelet)
        let metadataOpt =
            if obj.ReferenceEquals(metadata, null)
            then None
            else Some metadata
        WebSharperOptions.Create(services, siteletOpt, Option.ofObj siteletAssembly, metadataOpt, Option.ofObj config, Option.ofObj logger, Option.ofObj binDir, useSitelets, useRemoting, fun _ _ -> ())

    static member internal Create
        (
            services: IServiceProvider,
            sitelet: option<Sitelet<obj>>,
            siteletAssembly: option<Assembly>,
            metadata: option<M.Info>,
            config: option<IConfiguration>,
            logger: option<ILogger>,
            binDir: option<string>,
            useSitelets: bool,
            useRemoting: bool,
            useExtension
        ) =
        let binDir =
            match binDir with
            | None -> autoBinDir()
            | Some d -> d
        let logger =
            match logger with
            | Some l -> l
            | None -> services.GetRequiredService<ILoggerFactory>().CreateLogger<WebSharperOptions>() :> _
        //// Note: must load assemblies and set Context.* before calling Shared.*
        //let assemblies =
        //    discoverAssemblies binDir
        //    |> loadReferencedAssemblies logger
        let env = services.GetRequiredService<IHostingEnvironment>()
        Context.IsDebug <- env.IsDevelopment
        let config =
            match config with
            | Some c -> c
            | None -> services.GetRequiredService<IConfiguration>().GetSection("websharper") :> _
        Context.GetSetting <- fun key -> Option.ofObj config.[key]
        let sitelet =
            if useSitelets then
                sitelet |> Option.orElseWith (fun () ->
                    match services.GetRequiredService<ISiteletService>() with
                    | null -> None
                    | service -> Some service.Sitelet
                )
            else None
        WebSharperOptions(services, env.ContentRootPath, env.WebRootPath, env.IsDevelopment(), sitelet, siteletAssembly, metadata, useSitelets, useRemoting, useExtension)

/// Defines settings for a WebSharper application.
type WebSharperBuilder(services: IServiceProvider) =
    let mutable _sitelet = None
    let mutable _siteletAssembly = None
    let mutable _metadata = None
    let mutable _config = None
    let mutable _logger = None
    let mutable _binDir = None
    let mutable _authScheme = None
    let mutable _useSitelets = true
    let mutable _useRemoting = true
    let mutable _useExtension = fun _ _ -> ()

    /// <summary>Defines the sitelet to serve.</summary>
    /// <remarks>
    /// Using <c>AddSitelet</c> in <c>ConfigureServices</c> is preferred.
    /// </remarks>
    member this.Sitelet<'T when 'T : equality>(sitelet: Sitelet<'T>) =
        _sitelet <- Some (Sitelet.Box sitelet)
        this

/// <summary>Specifies which assembly contains the runtime metadata for the sitelet (WebSharper project type: web).</summary>
/// <remarks>Default: entry assembly..</remarks>
    member this.SiteletAssembly(siteletAssembly: Assembly) =
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

    /// <summary>Defines the name of the authentication scheme to use for <c>Web.Context.UserSession</c>.</summary>
    /// <remarks>Default: "WebSharper".</remarks>
    member this.AuthenticationScheme(scheme: string) =
        _authScheme <- Some scheme
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
        let o = WebSharperOptions.Create(services, _sitelet, _siteletAssembly, _metadata, _config, _logger, _binDir, _useSitelets, _useRemoting, _useExtension)
        _authScheme |> Option.iter (fun s -> o.AuthenticationScheme <- s)
        o
