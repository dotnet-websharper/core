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

/// Exposes the compiler front-end for programmatic use.
module IntelliFactory.WebSharper.Compiler.FrontEnd

open System.IO
open System.Reflection
open System.Text
open System.Web.UI
open IntelliFactory.Core
module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Resources

/// Represents file-system paths.
type Path = string

/// Represents raw symbols data as found in .mdb and .pdb files.
type Symbols =
    | Mdb of byte []
    | Pdb of byte []

/// Represents assemblies.
[<Sealed>]
type Assembly =

    /// Returns the raw assembly data.
    member RawBytes : option<StrongNameKeyPair> -> byte []

    /// Writes the assembly to the given path.
    member Write : option<StrongNameKeyPair> -> Path -> unit

    /// Reads the embedded JavaScript.
    member CompressedJavaScript : option<string>

    /// Reads the embedded JavaScript.
    member ReadableJavaScript : option<string>

    /// Returns the associated symbols, if any.
    member Symbols : option<Symbols>

    /// The TypeScript `.d.ts` declarations for the JavaScript.
    member TypeScriptDeclarations : option<string>

/// Loads assemblies.
[<Sealed>]
type Loader =

    /// Creates a new loader. Accepts an assembly resolver.
    static member Create : resolver: AssemblyResolver -> log: (string -> unit) -> Loader

    /// Loads an assembly from raw data.
    member LoadRaw : byte [] -> option<Symbols> -> Assembly

    /// Loads an assembly from a given path.
    member LoadFile : Path -> Assembly

/// Represents compilation options.
type Options =
    {
        ErrorLimit : int
        KeyPair : option<StrongNameKeyPair>
        References : list<Assembly>
    }

    /// The defaults.
    static member Default : Options

/// Compiles an assembly and rewrites it on disk. Deprecated. Note
/// that `Compile opts log a = (Prepare opts log).CompileAndModify(a)`.
val Compile : Options -> log: (Message -> unit) -> (Assembly -> bool)

/// Represents a resource content file.
type ResourceContent =
    {
        Content : string
        ContentType : string
        Name : string
    }

/// A reduced resource context for simplified dependency rendering.
type ResourceContext =
    {
        /// Whether to emit readable JavaScript.
        DebuggingEnabled : bool

        /// Wheter to switch `//` links to `http://` links.
        DefaultToHttp : bool

        /// Reads environment settings.
        GetSetting : string -> option<string>

        /// Decides how to render a resource.
        RenderResource : ResourceContent -> R.Rendering
    }

/// Represents a compiled assembly.
[<Sealed>]
type CompiledAssembly =

    /// Renders the dependencies of the assembly.
    member RenderDependencies : R.Context * HtmlTextWriter -> unit

    /// Renders the dependencies of the assembly.
    member RenderDependencies : ResourceContext * HtmlTextWriter -> unit

    /// The metadata info record for the individual assembly.
    member AssemblyInfo : M.AssemblyInfo

    /// The compressed JS source for the assembly.
    member CompressedJavaScript : string

    /// The metadata info record for the assembly set.
    member Info : M.Info

    /// The readable JS source for the assembly.
    member ReadableJavaScript : string

    /// The TypeScript `.d.ts` declarations for the JavaScript.
    member TypeScriptDeclarations : string

/// Represents the compiler front-end object.
[<Sealed>]
type Compiler =

    /// Attempts to compile an expression potentially coming from a dynamic assembly.
    member Compile : quotation: Quotations.Expr * ?name: string -> option<CompiledAssembly>

    /// Attempts to compile an expression potentially coming from a dynamic assembly.
    member Compile : quotation: Quotations.Expr * context: System.Reflection.Assembly * ?name: string -> option<CompiledAssembly>

    /// Compiles an assembly and rewrites it on disk.
    member CompileAndModify : assembly: Assembly -> bool

/// Prepares a compiler.
val Prepare : Options -> log: (Message -> unit) -> Compiler

/// See `Bundle`.
[<Sealed>]
type Content =
    member WriteFile : name: string * ?encoding: Encoding -> unit
    member Write : TextWriter -> unit
    member Text : string

/// Experimental API for bundling WebSharper file sets into application packages.
[<Sealed>]
type Bundle =
    member CSS : Content
    member JavaScript : Content
    member MinifiedJavaScript : Content
    member TypeScript : Content
    member WithAssembly : assemblyFile: string -> Bundle
    member WithDefaultReferences : unit -> Bundle
    member WithTransitiveReferences : unit -> Bundle
    static member Empty : Bundle
    static member Create : unit -> Bundle
