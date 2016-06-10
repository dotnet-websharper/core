// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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
[<Sealed>]
type Context<'Action> =
    interface WebSharper.Web.IContext

    /// Virtual application root path on the server.
    member ApplicationPath : string

    /// Generates a (possibly relative) URL to a given action.
    member Link : 'Action -> string

    /// The typed JSON provider for interacting with the client.
    member Json : WebSharper.Core.Json.Provider

    /// WebSharper metadata required for serializing controls.
    member Metadata : WebSharper.Core.Metadata.Info

    // Generates a URL respecting the application path.
    member ResolveUrl : string -> string

    /// WebSharper resource rendering context required for resources.
    member ResourceContext : WebSharper.Core.Resources.Context

    /// HTTP Request object
    member Request : Http.Request

    /// The full path to the application's root folder.
    member RootFolder : string

    /// Manage user login sessions.
    member UserSession : WebSharper.Web.IUserSession

    /// Environment-specific information (e.g. the ASP.NET or OWIN context)
    member Environment : IDictionary<string,obj>

    new : ApplicationPath : string
        * Link : ('Action -> string)
        * Json : WebSharper.Core.Json.Provider
        * Metadata : WebSharper.Core.Metadata.Info
        * ResolveUrl : (string -> string)
        * ResourceContext : WebSharper.Core.Resources.Context
        * Request : Http.Request
        * RootFolder : string
        * UserSession : WebSharper.Web.IUserSession
        * Environment : IDictionary<string, obj>
        -> Context<'Action>

module Context =

    /// Map a context to a wrapping type.
    val Map : ('T2 -> 'T1) -> Context<'T1> -> Context<'T2>
