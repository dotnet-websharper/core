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

namespace WebSharper.Compiler

/// Represents assemblies.
[<Sealed>]
type Assembly =

    /// Loads embedded non-script resources.
    member GetContents : unit -> seq<EmbeddedFile>

    /// Loads embedded script resources with a WebResource attribute.
    member GetResScripts : unit -> seq<EmbeddedFile>

    /// Loads all embedded JavaScript resources.
    member GetScripts : WebSharper.Core.JavaScript.Output -> seq<EmbeddedFile>

    /// Loads all embedded map files.
    member GetMapFiles : WebSharper.Core.JavaScript.Output -> seq<EmbeddedFile>

    /// Loads all source files.
    member GetSources : unit -> seq<EmbeddedFile> 

    /// Returns the raw assembly data.
    member RawBytes : option<byte[]> -> byte[]

    /// Writes the assembly to the given path.
    member Write : option<byte[]> -> path: string -> unit

    /// The short name of the assembly.
    member Name : string

    /// The full name of the assembly.
    member FullName : string

    member LoadPath : option<string>
    member internal Raw : Mono.Cecil.AssemblyDefinition

    /// Returns the associated symbols, if any.
    member Symbols : option<Symbols>

    /// True if the assembly contains the `WebSharper.meta` embedded resource.
    member HasWebSharperMetadata : bool

    /// The source files embedded for source mapping, in path-contents pairs
    member EmbeddedSourceFiles : (string * string)[]

    static member internal Create :
        def: Mono.Cecil.AssemblyDefinition
        * ?loadPath: string
        * ?symbols: Symbols ->
        Assembly

    static member internal GetAllResources :
        def: Mono.Cecil.AssemblyDefinition ->
        seq<EmbeddedFile>
