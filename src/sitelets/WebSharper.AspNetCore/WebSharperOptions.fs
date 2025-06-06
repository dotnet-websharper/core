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
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open WebSharper.Core
open WebSharper.Core.DependencyGraph
open WebSharper.Sitelets
open Microsoft.AspNetCore.Builder

module M = WebSharper.Core.Metadata

[<Sealed>]
type WebSharperOptions
    internal 
    (
        services: IServiceProvider,
        config: IConfiguration,
        logger: ILogger,
        contentRoot: string,
        webRoot: string,
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
    
    member this.ContentRootPath = contentRoot

    member this.WebRootPath = webRoot

    member this.Sitelet = sitelet

    member this.RemotingHeaders = remotingHeaders

    member internal this.UseExtension = useExtension
