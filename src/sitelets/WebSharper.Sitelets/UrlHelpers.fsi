// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

open System

/// Helpers to parse requests in a custom Sitelet router, for example in `Router.New`.
/// Request patterns compose with `&`:
/// `| GET & PATH ["blog"; INT id] & PARAM "page" (INT page) -> Some (Blog (id, page))`.
/// Number patterns parse a whole string (such as a path segment) with the invariant culture,
/// accepting no whitespace, and a leading sign only for signed types.
module UrlHelpers =

    /// Match the end of a string.
    val (|EOL|_|) : string -> unit option

    /// Parse a slash in a string.
    val (|SLASH|_|) : string -> string option

    /// Parse an integer from a string.
    val (|INT|_|) : string -> int option

    /// Parse an unsigned integer from a string.
    val (|UINT|_|) : string -> UInt32 option

    /// Parse a 64-bit integer from a string.
    val (|INT64|_|) : string -> int64 option

    /// Parse a 64-bit unsigned integer from a string.
    val (|UINT64|_|) : string -> UInt64 option

    /// Parse a 16-bit integer from a string.
    val (|INT16|_|) : string -> int16 option

    /// Parse a 16-bit unsigned integer from a string.
    val (|UINT16|_|) : string -> UInt16 option
    
    /// Parse a 8-bit integer from a string (same as SBYTE).
    val (|INT8|_|) : string -> int8 option

    /// Parse a 8-bit unsigned integer from a string (same as BYTE).
    val (|UINT8|_|) : string -> uint8 option
    
    /// Parse a sbyte from a string.
    val (|SBYTE|_|) : string -> sbyte option

    /// Parse a byte from a string.
    val (|BYTE|_|) : string -> byte option

    /// Parse a floating-point number from a string.
    val (|FLOAT|_|) : string -> float option

    /// Parse a guid in a string
    val (|GUID|_|) : string -> Guid option

    /// Parse a boolean in a string
    val (|BOOL|_|): string -> bool option

    /// Parse an alphabetic string in a string.
    /// Returns the parsed string and the remainder of the string.
    val (|ALPHA|_|) : string -> (string * string) option

    /// Parse an alphanumeric string in a string.
    /// Returns the parsed string and the remainder of the string.
    val (|ALPHA_NUM|_|) : string -> (string * string) option

    /// Skip the substring matching a regexp at the beginning of a string.
    val (|REGEX|_|) : regexp: string -> input: string -> string option

    /// Split the Path component of a URL by a given character.
    /// The path is unescaped before splitting, so an encoded separator also splits; use PATH to split requests by '/'.
    val (|SPLIT_BY|_|) : char -> Uri -> string list option

    /// Matches a GET request, including a custom method named "GET".
    val (|GET|_|) : Http.Request -> unit option

    /// Matches a POST request, including a custom method named "POST".
    val (|POST|_|) : Http.Request -> unit option

    /// Matches a PUT request, including a custom method named "PUT".
    val (|PUT|_|) : Http.Request -> unit option

    /// Matches a DELETE request, including a custom method named "DELETE".
    val (|DELETE|_|) : Http.Request -> unit option

    /// Matches a OPTIONS request, including a custom method named "OPTIONS".
    val (|OPTIONS|_|) : Http.Request -> unit option

    /// Matches a TRACE request, including a custom method named "TRACE".
    val (|TRACE|_|) : Http.Request -> unit option

    /// Matches a QUERY request, including a custom method named "QUERY".
    val (|QUERY|_|) : Http.Request -> unit option

    /// Matches a HEAD request, including a custom method named "HEAD".
    val (|HEAD|_|) : Http.Request -> unit option

    /// Matches a CONNECT request, including a custom method named "CONNECT".
    val (|CONNECT|_|) : Http.Request -> unit option

    /// Matches a request with a method not recognized by the method patterns above, returning the method name.
    val (|CUSTOM|_|) : Http.Request -> string option

    /// The non-empty path segments of the request URL, each one unescaped,
    /// so that an encoded slash ("%2F") stays inside its segment.
    val (|PATH|) : Http.Request -> string list

    /// Matches a request with the given query string parameter, returning its value.
    val (|PARAM|_|) : name: string -> Http.Request -> string option

    /// Matches a request with the given form field in its body, returning its value.
    val (|FORM|_|) : name: string -> Http.Request -> string option
