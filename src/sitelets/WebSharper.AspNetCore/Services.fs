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
open Microsoft.Extensions.Configuration

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

type DefaultWebSharperService
    (
        [<Optional>] siteletAssembly: Assembly, 
        [<Optional>] metadata: M.Info, 
        [<Optional>] authenticationScheme: string,
        [<Optional>] configuration: IConfiguration) =

    let siteletAssembly =
        siteletAssembly
        |> Option.ofObj
        |> Option.defaultWith System.Reflection.Assembly.GetEntryAssembly

    let metadata, dependencies = 
        if not (obj.ReferenceEquals(metadata, null)) then
            metadata, Graph.FromData metadata.Dependencies
        else
            let before = System.DateTime.UtcNow
            let metadataSetting =
                configuration.["WebSharperSharedMetadata"]
                |> Option.ofObj
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

    let authenticationScheme =
        if isNull authenticationScheme then "WebSharper" else authenticationScheme

    let configuration =
        if isNull configuration then ConfigurationManager.GetSection("websharper") :?> IConfiguration else configuration

    interface IWebSharperService with
        member this.SiteletAssembly = siteletAssembly
        member this.Metadata = metadata
        member this.Dependencies = dependencies
        member this.Json = json
        member this.AuthenticationScheme = authenticationScheme
        member this.Configuration = configuration

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

    /// <summary>
    /// Add a sitelet service to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'TImplementation
            when 'TImplementation :> ISiteletService
            and 'TImplementation : not struct>
            (this: IServiceCollection) =
        this.AddSingleton<ISiteletService, 'TImplementation>()

    /// <summary>
    /// Add a sitelet to be loaded on startup with <c>UseWebSharper</c>.
    /// </summary>
    [<Extension>]
    static member AddSitelet<'T when 'T : equality>
            (this: IServiceCollection, sitelet: Sitelet<'T>) =
        this.AddSingleton<ISiteletService>(DefaultSiteletService sitelet)

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection) =
        this.AddSingleton<'THandler, 'THandler>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'THandler>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler, 'TInstance when 'TInstance : not struct>
            (this: IServiceCollection) =
        this.AddSingleton<'TInstance, 'TInstance>()
            .AddSingleton<IRemotingService<'THandler>, RemotingService<'THandler, 'TInstance>>()

    /// <summary>
    /// Add a remoting handler to be loaded on startup with <c>UseWebSharper</c>.
    /// The client can invoke it using <c>WebSharper.JavaScript.Pervasives.Remote&lt;THandler&gt;</c>.
    /// </summary>
    [<Extension>]
    static member AddWebSharperRemoting<'THandler when 'THandler : not struct>
            (this: IServiceCollection, handler: 'THandler) =
        this.AddSingleton<'THandler>(handler)
            .AddSingleton<IRemotingService<'THandler>>(RemotingService handler)

    [<Extension>]
    static member AddWebSharper(this: IServiceCollection, 
            [<Optional>] siteletAssembly: Assembly, 
            [<Optional>] metadata: M.Info) =
        this.AddSingleton<IWebSharperService>(DefaultWebSharperService(siteletAssembly, metadata))
        
