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

module M = IntelliFactory.WebSharper.Core.Metadata
module P = IntelliFactory.JavaScript.Packager
module R = IntelliFactory.WebSharper.Compiler.ReflectionLayer
module Res = IntelliFactory.WebSharper.Core.Resources

/// Represents a compiled assembly.
[<Sealed>]
type CompiledAssembly =

    /// Renders the dependencies of the assembly.
    member RenderDependencies : Res.Context * HtmlTextWriter -> unit

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

    static member internal Create :
        context: Context
        * source: R.AssemblyDefinition
        * meta: Metadata.T
        * aInfo: M.AssemblyInfo
        * mInfo: M.Info
        * pkg: P.Module
        * typeScript: string ->
        CompiledAssembly

    member internal WriteToCecilAssembly : Mono.Cecil.AssemblyDefinition -> unit
