// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.VisualStudioTools

/// Utility module used by VisualStudioTools.
module Utils =
    open System
    open System.IO

    /// A simple representation for text and binary contents.
    [<Sealed>]
    type Content =

        /// Converts to a byte array.
        member GetBytes : unit -> byte []

        /// Writes to a stream.
        member Write : Stream -> unit

        /// Writes as a file.
        member WriteFile : path: string -> unit

        /// Lifts bytes to a `Content`.
        static member FromBytes : byte[] -> Content

        /// Lifts text to a `Content`.
        static member FromText : string -> Content

        /// Reads a binary file.
        static member ReadBinaryFile : path: string -> Content

        /// Reads a text file.
        static member ReadTextFile : path: string -> Content

        /// Creates a zip archive.
        static member Zip : seq<string * Content> -> Content
