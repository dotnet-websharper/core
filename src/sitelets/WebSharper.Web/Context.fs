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

namespace WebSharper.Web

open System.Collections.Generic

/// Provides context about the web request being replied to.
[<AbstractClass>]
type Context() =

    static member JoinWithSlash (a: string, b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

    /// Get or set runtime configuration settings.
    static member val GetSetting : (string -> option<string>) = (fun _ -> None)
        with get, set

    /// Get or set whether the application is running in debug mode.
    static member val IsDebug : (unit -> bool) = (fun () -> false)
        with get, set

    /// Virtual application root path on the server.
    abstract member ApplicationPath : string

    /// The full path to the application's root folder.
    abstract member RootFolder : string

    /// The URI of the web request.
    abstract member RequestUri : System.Uri

    /// Manage user login sessions.
    abstract member UserSession : IUserSession

    /// Environment-specific information (e.g. the ASP.NET or OWIN context)
    abstract member Environment : IDictionary<string,obj>

    /// The typed JSON provider for interacting with the client.
    abstract member Json : WebSharper.Core.Json.Provider

    /// WebSharper metadata required for serializing controls.
    abstract member Metadata : WebSharper.Core.Metadata.Info

    /// WebSharper code dependency graph required for looking up resources.
    abstract member Dependencies : WebSharper.Core.DependencyGraph.Graph

    /// WebSharper resource rendering context required for resources.
    abstract member ResourceContext : WebSharper.Core.Resources.Context

    /// Generates a URL respecting the application path.
    member this.ResolveUrl(url: string) =
        if url.StartsWith "~" then
            Context.JoinWithSlash(this.ApplicationPath, url.Substring(1))
        else url

module Remoting =

    let internal context =
        new System.Threading.ThreadLocal<option<Context>>(fun () -> None)

    let mutable internal allowedOrigins = Set.empty

    /// Set the HTTP origins that are allowed to perform RPC calls to this application.
    /// The format is: "http://mydomain.com"
    let SetAllowedOrigins (origins: seq<string>) =
        allowedOrigins <- Set.ofSeq (origins |> Seq.map (fun s -> s.ToLowerInvariant()))

    /// Add an HTTP origin that is allowed to perform RPC calls to this application.
    /// Does nothing if this origin was already allowed.
    /// The format is: "http://mydomain.com"
    let AddAllowedOrigin (origin: string) =
        allowedOrigins <- Set.add (origin.ToLowerInvariant()) allowedOrigins

    /// Remove an HTTP origin that is allowed to perform RPC calls to this application.
    /// Does nothing if this origin wasn't allowed.
    /// The format is: "http://mydomain.com"
    let RemoveAllowedOrigin (origin: string) =
        allowedOrigins <- Set.remove (origin.ToLowerInvariant()) allowedOrigins

    let mutable internal csrfProtect = true

    /// Enable Cross-Site Request Forgery protection on RPC calls.
    /// It is enabled by default.
    let EnableCsrfProtection() =
        csrfProtect <- true

    /// Disable Cross-Site Request Forgery protection on RPC calls.
    /// It is enabled by default.
    let DisableCsrfProtection() =
        csrfProtect <- false

    /// Retrieve the current web context in an Rpc function. This function must be called
    /// from the thread from which the Rpc function is originally called. The returned
    /// object can be used throughout the Rpc function.
    let GetContext() =
        match context.Value with
        | None -> failwith "No remoting context available."
        | Some c -> c

[<AutoOpen>]
module RemotingExtensions =

    type WebSharper.Core.Remoting.Server with

        /// Handle a request with the given web context.
        member this.HandleRequest(req, context) =
            Remoting.context.Value <- Some context
            let res = this.HandleRequest(req)
            Remoting.context.Value <- None
            res
