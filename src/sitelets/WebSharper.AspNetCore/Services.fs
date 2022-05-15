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
    abstract GetWebSharperMeta : siteletAssembly: Assembly * logger: ILogger -> (M.Info * Graph * Json.Provider) 

type WebSharperService() =

    let metaCache = Dictionary<Assembly, M.Info * Graph * Json.Provider>()
    
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

                let res = metadata, dependencies, J.Provider.CreateTyped metadata
                metaCache.Add(siteletAssembly, res)
                res
            
/// Define the sitelet to serve by WebSharper.
[<AllowNullLiteral>]
type ISiteletService =
    abstract Sitelet : Sitelet<obj>

/// Define the default sitelet to serve by WebSharper.
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

    /// <summary>
    /// Adds a required service to serve as metadata cache for WebSharper.
    /// Automatically added by any other AddWebSharper... methods.
    /// </summary>
    [<Extension>]
    static member AddWebSharper(this: IServiceCollection) =
        if this |> Seq.exists (fun s -> s.ServiceType = typeof<IWebSharperService>) |> not then
            this.AddSingleton<IWebSharperService, WebSharperService>() |> ignore
        this

    /// <summary>
    /// Add a sitelet service to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'TImplementation
            when 'TImplementation :> ISiteletService
            and 'TImplementation : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper()
            .AddSingleton<ISiteletService, 'TImplementation>()

    /// <summary>
    /// Add a sitelet to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'T when 'T : equality>
            (this: IServiceCollection, sitelet: Sitelet<'T>) =
        this.AddWebSharper()
            .AddSingleton<ISiteletService>(DefaultSiteletService sitelet)

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper()
            .AddSingleton<'THandler, 'THandler>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'THandler>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler, 'TInstance when 'TInstance : not struct>
            (this: IServiceCollection) =
        this.AddWebSharper()
            .AddSingleton<'TInstance, 'TInstance>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'TInstance>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection, handler: 'THandler) =
        this.AddWebSharper()
            .AddSingleton<'THandler>(handler)
            .AddSingleton<IRemotingService<'THandler>>(RemotingService handler)
