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

module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

[<AllowNullLiteral>]
type IWebSharperService =
    abstract SiteletAssembly : Assembly
    abstract Metadata : M.Info
    abstract Dependencies : Graph
    abstract Json : Json.Provider
    abstract AuthenticationScheme : string
    abstract Configuration : IConfiguration
    abstract IsLogging : bool
    abstract Timed: string * (unit -> 'T) ->  'T
    abstract TimedAsync: string * (unit -> Task) ->  Task

type IWebSharperServiceOptions =
    abstract SiteletAssembly : Assembly
    abstract Metadata : M.Info option
    abstract AuthenticationScheme : string
    abstract Configuration : IConfiguration

type WebSharperServiceOptions() =
    
    let mutable siteletAssembly: Assembly = null
    let mutable metadata: M.Info option = None
    let mutable authenticationScheme: string = null
    let mutable configuration: IConfiguration = null

    member x.SiteletAssembly with set a =
        if isNull a |> not then siteletAssembly <- a
    member x.Metadata with set m =
        if obj.ReferenceEquals(m, null) |> not then metadata <- Some m
    member x.AuthenticationScheme with set s =
        if isNull s |> not then authenticationScheme <- s
    member x.Configuration with set c =
        if isNull c |> not then configuration <- c

    interface IWebSharperServiceOptions with 
        member x.SiteletAssembly = siteletAssembly
        member x.Metadata = metadata
        member x.AuthenticationScheme = authenticationScheme
        member x.Configuration = configuration

type DefaultWebSharperService (serviceOptions: IWebSharperServiceOptions, logger: ILogger<IWebSharperService>, rootConfig: IConfiguration) =

    let siteletAssembly =
        serviceOptions.SiteletAssembly
        |> Option.ofObj
        |> Option.defaultWith System.Reflection.Assembly.GetEntryAssembly

    let configuration =
        match serviceOptions.Configuration with
        | null ->
            rootConfig.GetSection("websharper") :> IConfiguration 
        | c -> c 

    let isDebugLogging = logger.IsEnabled(LogLevel.Debug)
    
    let timedInfo (message: string) action =
        if logger.IsEnabled(LogLevel.Information) then
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()
            let r = action()
            logger.LogInformation("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
            r
        else
            action()

    let timedDebug (message: string) action =
        if isDebugLogging then
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()
            let r = action()
            logger.LogDebug("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
            r
        else
            action()

    let timedDebugAsync (message: string) (action: unit -> Task) =
        if isDebugLogging then
            task {
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                do! action()
                logger.LogDebug("{0} in {1} ms.", message, sw.Elapsed.TotalMilliseconds)
            } :> Task
        else
            action()

    let metadata, dependencies = 
        match serviceOptions.Metadata with
        | Some metadata ->
            metadata, Graph.FromData metadata.Dependencies
        | None ->
            let metadataSetting =
                if isNull configuration then
                    None
                else
                    configuration.[RUNTIMESETTING_SHARED_METADATA]
                    |> Option.ofObj
                    |> Option.map (fun x -> x.ToLower())
            match metadataSetting with
            | Some "none" ->
                M.Info.Empty, Graph.Empty
            | _ ->
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

    let json = J.Provider.CreateTyped metadata

    let authenticationScheme =
        match serviceOptions.AuthenticationScheme with
        | null -> "WebSharper"
        | s -> s

    interface IWebSharperService with
        member this.SiteletAssembly = siteletAssembly
        member this.Metadata = metadata
        member this.Dependencies = dependencies
        member this.Json = json
        member this.AuthenticationScheme = authenticationScheme
        member this.Configuration = configuration
        member this.IsLogging = isDebugLogging
        member this.Timed(message, action) = timedDebug message action
        member this.TimedAsync(message, action) = timedDebugAsync message action

/// Define the sitelet to serve by WebSharper.
[<AllowNullLiteral>]
type ISiteletService =
    abstract Sitelet : Sitelet<obj>

/// Define the sitelet to serve by WebSharper.
[<AbstractClass>]
type SiteletService<'T when 'T : equality>() =
    abstract Sitelet : Sitelet<'T>

    interface ISiteletService with
        member this.Sitelet = Sitelet.Box this.Sitelet

type DefaultSiteletService<'T when 'T : equality>(sitelet: Sitelet<'T>) =
    inherit SiteletService<'T>()

    override this.Sitelet = sitelet

/// Define a remoting handler to serve by WebSharper.
type IRemotingService =
    abstract Handler : obj

/// Define a remoting handler to serve by WebSharper.
type IRemotingService<'T> =
    inherit IRemotingService

type RemotingService<'THandler, 'TInstance>(handler: 'TInstance) =
    interface IRemotingService<'THandler> with
        member this.Handler = (box handler)

[<Extension>]
type ServiceExtensions =

    static member private AddWebSharperService (this: IServiceCollection) =
        if this |> Seq.exists (fun s -> s.ServiceType = typeof<IWebSharperService>) |> not then
            this.AddSingleton<IWebSharperService, DefaultWebSharperService>() |> ignore

        match this |> Seq.tryFind (fun s -> s.ServiceType = typeof<IWebSharperServiceOptions>) with
        | Some optsEntry -> 
            optsEntry.ImplementationInstance :?> WebSharperServiceOptions
        | None ->
            let o = WebSharperServiceOptions()
            this.AddSingleton<IWebSharperServiceOptions>(o) |> ignore
            o
    
    /// <summary>
    /// Add a sitelet service to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'TImplementation
            when 'TImplementation :> ISiteletService
            and 'TImplementation : not struct>
            (this: IServiceCollection) =
        ServiceExtensions.AddWebSharperService this |> ignore
        this.AddSingleton<ISiteletService, 'TImplementation>()

    /// <summary>
    /// Add a sitelet to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'T when 'T : equality>
            (this: IServiceCollection, sitelet: Sitelet<'T>) =
        ServiceExtensions.AddWebSharperService this |> ignore
        this.AddSingleton<ISiteletService>(DefaultSiteletService sitelet)

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection) =
        ServiceExtensions.AddWebSharperService this |> ignore
        this.AddSingleton<'THandler, 'THandler>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'THandler>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler, 'TInstance when 'TInstance : not struct>
            (this: IServiceCollection) =
        ServiceExtensions.AddWebSharperService this |> ignore
        this.AddSingleton<'TInstance, 'TInstance>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'TInstance>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection, handler: 'THandler) =
        ServiceExtensions.AddWebSharperService this |> ignore
        this.AddSingleton<'THandler>(handler)
            .AddSingleton<IRemotingService<'THandler>>(RemotingService handler)

    [<Extension>]
    static member AddWebSharper(this: IServiceCollection, 
            [<Optional>] siteletAssembly: Assembly, 
            [<Optional>] metadata: M.Info,
            [<Optional>] authenticationScheme: string,
            [<Optional>] configuration: IConfiguration) =
        let opts = ServiceExtensions.AddWebSharperService this
        opts.SiteletAssembly <- siteletAssembly
        opts.Metadata <- metadata
        opts.AuthenticationScheme <- authenticationScheme
        opts.Configuration <- configuration
        this
