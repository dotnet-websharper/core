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

[<AllowNullLiteral>]
type IWebSharperService =
    abstract DefaultAssembly : Assembly 
    abstract WebSharperOptions : WebSharperOptions with get, set
            
/// Define the runtime metadata to use by WebSharper.
[<AllowNullLiteral>]
type IMetadataService =
    abstract Metadata : M.Info
    abstract Graph : Graph

/// Define the remoting server instance to use by WebSharper.
[<AllowNullLiteral>]
type IRemotingServerService =
    abstract RemotingServer : Remoting.Server

/// Define the sitelet to serve by WebSharper.
[<AllowNullLiteral>]
type ISiteletService =
    abstract Sitelet : Sitelet<obj>

/// Base non-generic interface. Do not use directly, register IRemotingService<T> instead.
type IRemotingService =
    abstract Handler : obj

/// Define a remoting handler to serve by WebSharper.
type IRemotingService<'T> =
    inherit IRemotingService

/// Define a service for interop with Razor pages.
type IWebSharperContentService =
    abstract Head : unit -> IHtmlContent
    abstract Render : INode -> IHtmlContent
