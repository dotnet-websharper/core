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

[<Sealed>]
type CompilationKind =
    static member Console : CompilationKind
    static member Library : CompilationKind
    static member Windows : CompilationKind

type CompilerOptions =
    {
        AssemblyName : string
        AssemblyResolver : option<WebSharper.Compiler.AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        Kind : CompilationKind
        OutputPath : option<string>
        ProjectDir : string
        ReferencePaths : seq<string>
        StrongNameKeyPath : option<string>
    }

    static member Default : assemblyName: string -> CompilerOptions
    static member Parse : args: seq<string> -> CompilerOptions

[<Sealed>]
type CompiledAssembly =
    member GetBytes : unit -> byte []
    member Save : path: string -> unit
    member Write : Stream -> unit
    member FileName : string

[<Sealed>]
type Compiler =
    member Compile : options: CompilerOptions * assembly: CodeModel.Assembly * filePath: string * ?original: Assembly -> CompiledAssembly
    member Start : args: seq<string> * assembly: CodeModel.Assembly * ?resolver: WebSharper.Compiler.AssemblyResolver -> int
    member Start : args: seq<string> * assembly: CodeModel.Assembly * original: Assembly * ?resolver: WebSharper.Compiler.AssemblyResolver -> int
    static member Create : WebSharper.Compiler.LoggerBase -> Compiler
