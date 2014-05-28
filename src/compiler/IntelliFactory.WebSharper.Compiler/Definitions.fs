// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Defines types that are too simple to deserve a dedicated file.
namespace IntelliFactory.WebSharper.Compiler

module CT = IntelliFactory.WebSharper.Core.ContentTypes
module Res = IntelliFactory.WebSharper.Core.Resources

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

type internal Name = string

type internal Value =
    | Bool of bool
    | Double of double
    | Int of int
    | String of string
