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

namespace WebSharper.Sitelets

open System

/// Helpers to parse requests in a custom Sitelet router.
module UrlHelpers =

    /// Match the end of a string.
    val (|EOL|_|) : string -> unit option

    /// Parse a slash in a string.
    val (|SLASH|_|) : string -> string option

    /// Parse an integer in a string.
    val (|INT|_|) : string -> int option

    /// Parse a floating-point number in a string.
    val (|FLOAT|_|) : string -> float option

    /// Parse an alphabetic string in a string.
    /// Returns the parsed string and the remainder of the string.
    val (|ALPHA|_|) : string -> (string * string) option

    /// Parse an alphanumeric string in a string.
    /// Returns the parsed string and the remainder of the string.
    val (|ALPHA_NUM|_|) : string -> (string * string) option

    /// Skip the substring matching a regexp at the beginning of a string.
    val (|REGEX|_|) : regexp: string -> string -> string option

    /// Split the Path component of a URL by a given character.
    val (|SPLIT_BY|_|) : char -> Uri -> string list option

    /// Extract the URL and parameters from an HTTP request
    /// that uses the given method.
    val (|DELETE|GET|OPTIONS|POST|PUT|TRACE|SPECIAL|) :
        Http.Request ->
        Choice< (string * string) list * Uri,
                (string * string) list * Uri,
                (string * string) list * Uri,
                (string * string) list * Uri,
                (string * string) list * Uri,
                (string * string) list * Uri,
                Http.Method * (unit -> (string * string) list) * Uri >
