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

namespace WebSharper

open WebSharper.JavaScript

[<CompiledName "TypedJson">]
type Json =

    /// An instance of Json.Provider, used for custom JSON serialization on the server.
    static member ServerSideProvider = WebSharper.Core.Json.Provider.Create ()

    /// Encodes an object in such a way that JSON stringification
    /// results in the same readable format as Sitelets.
    /// Client-side only.
    static member Encode<'T> (x: 'T) = X<obj>

    /// Serializes an object to JSON using the same readable format as Sitelets.
    /// For plain JSON stringification, see Json.Stringify.
    static member Serialize<'T> (x: 'T) =
        Json.ServerSideProvider.GetEncoder<'T>().Encode x
        |> Json.ServerSideProvider.Pack
        |> Core.Json.Stringify

    /// Decodes an object parsed from the same readable JSON format as Sitelets.
    /// Client-side only.
    static member Decode<'T> (x: obj) = X<'T>

    /// Deserializes a JSON string using the same readable format as Sitelets.
    /// For plain JSON parsing, see Json.Parse.
    static member Deserialize<'T> (x: string) =
        Core.Json.Parse x
        |> Json.ServerSideProvider.GetDecoder<'T>().Decode
