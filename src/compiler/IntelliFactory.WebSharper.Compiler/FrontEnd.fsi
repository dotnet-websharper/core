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

/// Exposes the compiler front-end for programmatic use.
module FrontEnd =
    type Assembly = IntelliFactory.WebSharper.Compiler.Assembly
    type Bundle = IntelliFactory.WebSharper.Compiler.Bundle
    type BundleCommand = IntelliFactory.WebSharper.Compiler.BundleCommand
    type CompiledAssembly = IntelliFactory.WebSharper.Compiler.CompiledAssembly
    type Content = IntelliFactory.WebSharper.Compiler.Content
    type EmbeddedFile = IntelliFactory.WebSharper.Compiler.EmbeddedFile
    type Loader = IntelliFactory.WebSharper.Compiler.Loader
    type ResourceContent = IntelliFactory.WebSharper.Compiler.ResourceContent
    type ResourceContext = IntelliFactory.WebSharper.Compiler.ResourceContext
    type Symbols = IntelliFactory.WebSharper.Compiler.Symbols

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

