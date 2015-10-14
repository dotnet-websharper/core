// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Defines types that are too simple to deserve a dedicated file.
namespace WebSharper.Compiler

module CT = WebSharper.Core.ContentTypes
module Res = WebSharper.Core.Resources

/// Represents the priority of the error messages.
type Priority =
    | Critical
    | Error
    | Warning

    override this.ToString() =
        match this with
        | Critical -> "critical"
        | Error -> "error"
        | Warning -> "warning"

/// Represents locations in source code.
type SourceLocation =
    {
        File : string
        Line : int
        Column : int
    }

/// Represents locations with optional source locations.
type Location =
    {
        ReadableLocation : string
        SourceLocation : option<SourceLocation>
    }

    override this.ToString() =
        match this.SourceLocation with
        | None -> this.ReadableLocation
        | Some s -> String.Format("{0}({1})", s.File, s.Line)

/// Represents error and warning messages.
type Message =
    {
        Location : Location
        Priority : Priority
        Text : string
    }

    override this.ToString() =
        String.Format("{0}: {1}: {2}",
            this.Location, this.Priority, this.Text)

/// Thrown when the error limit is exceeded.
exception ErrorLimitExceeded

/// Represents a capability to log messages.
[<Sealed>]
type Logger private (log: Message -> unit, limit: int) =
    let mutable count = 0

    /// Sends a message to the logger.
    member this.Log(message: Message) =
        log message
        match message.Priority with
        | Critical -> count <- limit
        | Error -> count <- count + 1
        | Warning -> ()
        if count >= limit then
            raise ErrorLimitExceeded

    /// Creates a new logger.
    static member Create(log: Message -> unit)(errorLimit: int) =
        Logger(log, errorLimit)

/// Represents the kind of an inline definition. Instance definitions
/// accept a placeholder for the target object.
type MemberScope =
    | Instance
    | Static

/// Represents a resource content file.
type ResourceContent =
    {
        Content : string
        ContentType : CT.ContentType
        Name : string
    }

/// A reduced resource context for simplified dependency rendering.
type ResourceContext =
    {
        /// Whether to emit readable JavaScript.
        DebuggingEnabled : bool

        /// Wheter to switch `//` links to `http://` links.
        DefaultToHttp : bool

        /// Reads environment settings.
        GetSetting : string -> option<string>

        /// Decides how to render a resource.
        RenderResource : ResourceContent -> Res.Rendering
    }

//type internal Name = string

type internal Value =
    | Bool of bool
    | Double of double
    | Int of int
    | String of string
