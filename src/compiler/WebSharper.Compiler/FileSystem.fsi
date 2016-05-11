// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Provides utilities for accessing the file system and representing files.
module WebSharper.Compiler.FileSystem

open System.IO
open System.Text

/// The default encoding is UTF-8 without BOM.
val DefaultEncoding : Encoding

/// Represents binary content.
[<Sealed>]
type Binary =

    /// Writes to a given file if its contents are different,
    /// returns whether an actual write was performed.
    member EnsureFile : fullPath: string -> bool

    /// Reads content as a byte array.
    member GetBytes : unit -> byte []

    /// Opens a stream for reading.
    member Read : unit -> Stream

    /// Writes to a given stream.
    member Write : Stream -> unit

    /// Writes to a given file.
    member WriteFile : fullPath: string -> unit

    /// Constructs content from a byte array.
    static member FromBytes : bytes: byte [] -> Binary

    /// Reads a binary file.
    static member ReadFile : fullPath: string -> Binary

    /// Reads a stream.
    static member ReadStream : stream: Stream -> Binary

/// Defines text and binary content.
type Content =
    | BinaryContent of Binary
    | TextContent of string

    /// Writes to a given file if its contents are different,
    /// returns whether an actual write was performed.
    member EnsureFile : fullPath: string -> bool

    /// Writes to a given file.
    member WriteFile : fullPath: string -> unit

    /// Constructs a binary content.
    static member Binary : Binary -> Content

    /// Reads a binary file.
    static member ReadBinaryFile : fullPath: string -> Content

    /// Reads a text file.
    static member ReadTextFile : fullPath: string -> Content

    /// Constructs a text content.
    static member Text : string -> Content
