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

/// Compiles Code.Assembly values to assemblies.
namespace WebSharper.InterfaceGenerator

open System
open System.IO
open System.Reflection

type CompilerOptions =
    {
        AssemblyName : string
        AssemblyResolver : option<WebSharper.Compiler.AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        OutputPath : option<string>
        ReferencePaths : seq<string>
        StrongNameKeyPath : option<string>
    }

    static member Default : assemblyName: string -> CompilerOptions
    static member Parse : args: seq<string> -> CompilerOptions

[<Sealed>]
type CompiledAssembly =
    member GetBytes : unit -> byte []
    /// Writes the assembly and its sidecar file with the WebSharper metadata and web resources.
    member Save : path: string -> unit
    member Write : Stream -> unit
    member FileName : string
    /// Adds web resources (name, content type, contents), written to the sidecar file.
    /// The first one with a name is kept.
    member AddWebResources : seq<string * string * byte[]> -> unit
    /// WebSharper metadata of the written assembly image, with the hashes of its web resources.
    member GetWebSharperMetadata : WebSharper.Compiler.PEMetadata.PEAssembly -> byte[]

[<Sealed>]
type Compiler =
    member Compile : options: CompilerOptions * assembly: CodeModel.Assembly * filePath: string * ?original: Assembly -> CompiledAssembly
    member Start : args: seq<string> * assembly: CodeModel.Assembly * ?resolver: WebSharper.Compiler.AssemblyResolver -> int
    member Start : args: seq<string> * assembly: CodeModel.Assembly * original: Assembly * ?resolver: WebSharper.Compiler.AssemblyResolver -> int
    static member Create : WebSharper.Compiler.LoggerBase -> Compiler

/// Loads the interface generator assembly of a WIG project.
module ExtensionAssembly =

    /// Loads the assembly fsc wrote (keeping a copy as .Generator.dll) and gets the
    /// assembly definition from the IExtension named by its ExtensionAttribute.
    val Load : file: string -> logger: WebSharper.Compiler.LoggerBase -> AssemblyName * CodeModel.Assembly * Assembly
