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
type WebSharperOptions() =

    member val DefaultAssembly : System.Reflection.Assembly = null with get, set
    
    member val AuthenticationScheme = "WebSharper" with get, set // used by Usersession (sitelets+remoting)

    member val Configuration : IConfiguration = null with get, set

    member val ContentRootPath : string = null with get, set // used by Context

    member val WebRootPath : string = null with get, set // used by Context
