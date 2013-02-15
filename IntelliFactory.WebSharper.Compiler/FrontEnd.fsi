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

module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Resources
open System.Reflection
open System.Web.UI

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

/// Loads assemblies.
[<Sealed>]
type Loader =

    /// Creates a new loader. Accepts a set of search paths.
    static member Create : searchPaths: Set<Path> -> log: (string -> unit) -> Loader

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
        /// Allocates a new resource, returns a URI to it.
        CreateUri : ResourceContent -> string

        /// Whether to emit readable JavaScript.
        DebuggingEnabled : bool

        /// Reads environment settings.
        GetSetting : string -> option<string>
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
