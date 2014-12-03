// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Implements generic URL encoding and decoding for algebraic
/// datatypes. To be on the safe side, the encoded values use only
/// unreserved URL characters (including alphanumeric, tilde, dash,
/// underscore and dot characters). Tilde serves as a special character
/// starting escape sequences. The encoding also uses the forward slash
/// to separate logical components.
module IntelliFactory.WebSharper.Sitelets.UrlEncoding

/// Thrown when a formatter cannot be derived for a certain type.
exception NoFormatError of System.Type

/// Represents an URL encoding for a given type.
[<Sealed>]
type Format<'T> =

    /// Parses a string. Fails if the string cannot be parsed.
    member Read : string -> option<'T>

    /// Formats a value. Fails if it cannot be represented.
    member Show : 'T -> option<string>

/// Represents cached formatter collection.
[<Sealed>]
type Factory =

    /// Derives an encoding for the given type.
    member GetFormatFor : System.Type -> Format<obj>

    /// Derives an encoding for the given type passed as a type parameter.
    member GetFormat<'T> : unit -> Format<'T>

    /// Creates a new factory.
    static member Create : unit -> Factory

/// Derives a format using a new temporary factory.
val GetFormat<'T> : unit -> Format<'T>

/// Derives a format for a given type using a new temporary factory.
val GetFormatFor : System.Type -> Format<obj>

