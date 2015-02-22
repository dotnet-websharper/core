// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module M = WebSharper.Core.Metadata
module P = WebSharper.Core.JavaScript.Packager
module R = WebSharper.Compiler.ReflectionLayer
module Res = WebSharper.Core.Resources

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
        * typeScript: string 
        * sourceMap : bool ->
        CompiledAssembly

    member internal WriteToCecilAssembly : Mono.Cecil.AssemblyDefinition -> unit
