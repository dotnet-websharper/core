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

/// Implements JSON encoding and decoding for client-server interaction.
module WebSharper.Json

module Re = WebSharper.Core.Resources

/// Represents the IE7-compatibility JSON resource.
type Resource =

    /// Constructs a new instance.
    new : unit -> Resource

    interface Re.IResource

val private As<'T>          : obj -> 'T
val private op_Dynamic<'T>  : obj -> string -> 'T
val private (?<-)           : obj -> string -> obj -> unit

/// Reads a JSON string into a JavaScript object.
/// For type-aware deserialization compatible with Sitelets, see Json.Deserialize.
val Parse : string -> obj

/// Converts a JavaScript object to a JSON string.
/// For type-aware serialization compatible with Sitelets, see Json.Serialize.
val Stringify : obj -> string

/// Parses a JSON object returned by the server.
val Activate : obj -> Async<'T>
