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

/// Implements JSON encoding and decoding for client-server interaction.
module IntelliFactory.WebSharper.Json

module Re = IntelliFactory.WebSharper.Core.Resources

/// Represents the IE7-compatibility JSON resource.
type Resource =

    /// Constructs a new instance.
    new : unit -> Resource

    interface Re.IResource

val private As<'T>          : obj -> 'T
val private op_Dynamic<'T>  : obj -> string -> 'T
val private (?<-)           : obj -> string -> obj -> unit

/// Parses a JSON string.
val Parse : string -> obj

/// Converts JSON to a string.
val Stringify : obj -> string

/// Parses a JSON object returned by the server.
val Activate : obj -> 'T
