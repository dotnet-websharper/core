// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

namespace IntelliFactory.WebSharper.Compiler

type Name = string

/// Represents the priority of the error messages.
type Priority =
    | Critical
    | Error
    | Warning

/// Represents locations in source code.
type SourceLocation =
    {
        File: string
        Line: int
        Column: int
    }

type ReadableLocation = string

/// Represents locations with optional source locations.
type Location =
    {
        ReadableLocation: ReadableLocation
        SourceLocation: option<SourceLocation>
    }

/// Represents error and warning messages.
type Message =
    {
        Location: Location
        Priority: Priority
        Text: string
    }

/// Sets the limit to the maximum number of errors.
type ErrorLimit = int

/// Thrown when the error limit is exceeded.
exception ErrorLimitExceeded

/// Represents a capability to log messages.
[<Sealed>]
type Logger =

    /// Creates a new logger.
    static member Create : (Message -> unit) -> ErrorLimit -> Logger

    /// Sends a message to the logger.
    member Log : Message -> unit

/// Represents the kind of an inline definition. Instance definitions
/// accept a placeholder for the target object.
type MemberScope =
    | Instance
    | Static

/// Represents values.
type Value =
    | Bool of bool
    | Double of double
    | Int of int
    | String of string
