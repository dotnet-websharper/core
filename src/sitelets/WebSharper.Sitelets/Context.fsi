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

namespace WebSharper.Sitelets

open System.Collections.Generic

/// Provides services available to handlers at run-time.
[<Class>]
type Context<'Action> =
    inherit WebSharper.Web.Context

    /// Generates a (possibly relative) URL to a given action.
    member Link : 'Action -> string

    /// HTTP Request object
    member Request : Http.Request

    new : ApplicationPath : string
        * Link : ('Action -> string)
        * Json : WebSharper.Core.Json.Provider
        * Metadata : WebSharper.Core.Metadata.Info
        * Dependencies : WebSharper.Core.DependencyGraph.Graph
        * ResourceContext : WebSharper.Core.Resources.Context
        * Request : Http.Request
        * RootFolder : string
        * WebRootFolder : string
        * UserSession : WebSharper.Web.IUserSession
        * Environment : IDictionary<string, obj>
        -> Context<'Action>

[<Class>]
type Context =
    inherit Context<obj>

    new : Context<obj> -> Context

    /// Map a context to a wrapping type.
    static member Map : ('T2 -> 'T1) -> Context<'T1> -> Context<'T2>

    /// Define extra enviroment settings.
    static member WithSettings : (seq<string * string>) -> Context<'T> -> Context<'T>
