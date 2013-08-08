// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Compiles Code.Assembly values to assemblies.
namespace IntelliFactory.WebSharper.InterfaceGenerator

open System
open System.IO
open System.Reflection
open IntelliFactory.Core

[<Sealed>]
type CompilationKind =
    static member Console : CompilationKind
    static member Library : CompilationKind
    static member Windows : CompilationKind

type CompilerOptions =
    {
        AssemblyName : string
        AssemblyResolver : option<AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        Kind : CompilationKind
        OutputPath : option<string>
        ReferencePaths : seq<string>
        StrongNameKeyPair : option<StrongNameKeyPair>
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
    member Compile : options: CompilerOptions * assembly: CodeModel.Assembly -> CompiledAssembly
    member Start : args: seq<string> * assembly: CodeModel.Assembly -> int
    static member Create : unit -> Compiler
