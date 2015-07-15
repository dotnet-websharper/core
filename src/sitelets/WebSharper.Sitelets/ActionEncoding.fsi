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
module WebSharper.Sitelets.ActionEncoding

module A = WebSharper.Core.Attributes

/// Thrown when a formatter cannot be derived for a certain type.
exception NoFormatError of System.Type

/// The result of trying to decode a request.
[<A.NamedUnionCases "result">]
type DecodeResult<'Action> =
    /// The request was correct and an action was decoded.
    | [<CompiledName "success">]
      Success of action: 'Action
    /// An action was decoded, but the request used the given invalid HTTP method.
    | [<CompiledName "invalidMethod">]
      InvalidMethod of action: 'Action * ``method``: string
    /// An action failed to be decoded as JSON from the request body.
    /// The JSON part of the action is a default value.
    | [<CompiledName "invalidJson">]
      InvalidJson of action: 'Action
    /// A GET query parameter was missing to decode an action.
    /// The corresponding part of the action is a default value.
    | [<CompiledName "missingQueryParameter">]
      MissingQueryParameter of action: 'Action * queryParam: string
    /// A post body parameter of type application/x-www-urlencoded or multipart/form-data
    /// was missing to decode an action.
    /// The corresponding part of the action is a default value.
    | [<CompiledName "missingFormData">]
      MissingFormData of action: 'Action * formFieldName: string

/// Represents an URL encoding for a given type.
[<Sealed>]
type Format<'T> =

    /// Parses a URL path from the given request. Fails if the request cannot be parsed.
    member Read : path: string * Http.Request -> option<DecodeResult<'T>>

    /// Formats a value into a URL. Fails if it cannot be represented.
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

val internal JsonProvider : WebSharper.Core.Json.Provider
