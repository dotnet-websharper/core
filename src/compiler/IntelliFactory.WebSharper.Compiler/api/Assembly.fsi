// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Compiler

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

    /// Reads the embedded JavaScript.
    member ReadableJavaScript : option<string>

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
