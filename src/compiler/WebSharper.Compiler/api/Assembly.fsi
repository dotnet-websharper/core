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

namespace WebSharper.Compiler

/// Represents assemblies.
[<Sealed>]
type Assembly =

    /// Loads embedded non-script resources.
    member GetContents : unit -> seq<EmbeddedFile>

    /// Loads embedded script resources.
    member GetScripts : unit -> seq<EmbeddedFile>

    /// Returns the raw assembly data.
    member RawBytes : option<StrongNameKeyPair> -> byte []

    /// Writes the assembly to the given path.
    member Write : option<StrongNameKeyPair> -> path: string -> unit

    /// Reads the embedded JavaScript.
    member CompressedJavaScript : option<string>

    /// Reads the embedded map file.
    member MapFileForCompressed : option<string>

    /// Reads the embedded JavaScript.
    member ReadableJavaScript : option<string>

    /// Reads the embedded map file.
    member MapFileForReadable : option<string>

    /// The full name of the assembly.
    member FullName : string

    member internal LoadPath : option<string>
    member internal Raw : Mono.Cecil.AssemblyDefinition

    /// Returns the associated symbols, if any.
    member Symbols : option<Symbols>

    /// The TypeScript `.d.ts` declarations for the JavaScript.
    member TypeScriptDeclarations : option<string>

    static member internal Create :
        def: Mono.Cecil.AssemblyDefinition
        * ?loadPath: string
        * ?symbols: Symbols ->
        Assembly
