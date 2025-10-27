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
open Microsoft.AspNetCore.Html
open WebSharper.Web

module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json
            
/// Define the runtime metadata to use by WebSharper.
[<AllowNullLiteral>]
type IWebSharperMetadataService = 
    abstract Metadata : M.Info
    abstract Graph : Graph

/// Define the remoting server instance to use by WebSharper by default.
[<AllowNullLiteral>]
type IWebSharperRemotingServerService =
    abstract RemotingServer : Remoting.Server

/// Define the sitelet to serve by WebSharper by default.
[<AllowNullLiteral>]
type IWebSharperSiteletService =
    abstract Sitelet : Sitelet<obj>

/// Base non-generic interface. Do not use directly, register IWebSharperRemotingService<T> instead.
[<AllowNullLiteral>]
type IWebSharperRemotingService =
    abstract Handler : obj

/// Define a remoting handler to serve by WebSharper.
[<AllowNullLiteral>]
type IWebSharperRemotingService<'T> =
    inherit IWebSharperRemotingService

/// Define the json serializer provider instance to use by WebSharper.
[<AllowNullLiteral>]
type IWebSharperJsonProviderService =
    abstract JsonProvider : Json.Provider

/// Define a service for interop with Razor pages.
[<AllowNullLiteral>]
type IWebSharperMvcService =
    abstract Head : unit -> IHtmlContent
    abstract Render : INode -> IHtmlContent

/// Central service that initializes and holds cached WebSharper data.
[<AllowNullLiteral>]
type IWebSharperInitializationService =
    abstract Options : WebSharperOptions
    abstract MetadataAndGraph : option<M.Info * Graph>
    abstract RemotingServer : option<Remoting.Server>
    abstract JsonProvider : option<Json.Provider>