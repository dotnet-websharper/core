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

namespace IntelliFactory.WebSharper

/// Makes resource path and naming conventions explicit to avoid code duplication.
module PathConventions =
    open System
    open System.Reflection

    /// Identifies assemblies by name.
    [<Sealed>]
    type AssemblyId =

        /// Creates an assembly identifier from a string representation.
        static member Create : string -> AssemblyId

        /// Creates an assembly identifier for the given assembly object.
        static member Create : Assembly -> AssemblyId

        /// Creates an assembly identifier from a `System.Reflection` name.
        static member Create : AssemblyName -> AssemblyId

        /// Creates an assembly identifier from a marker type.
        static member Create : Type -> AssemblyId

    /// Represents kinds of resources.
    [<Sealed>]
    type ResourceKind =

        /// Content resources (such as CSS stylesheet).
        static member Content : ResourceKind

        /// Script resources (primarily JavaScript).
        static member Script : ResourceKind

    /// Represents embedded resources.
    [<Sealed>]
    type EmbeddedResource =

        /// Constructs a new embedded resource identifier.
        static member Create : ResourceKind * AssemblyId * resourceName: string -> EmbeddedResource

    /// Utility object for computing paths on the filesystem or relative URLs.
    [<Sealed>]
    type PathUtility =

        /// Path or URL for the embedded resource.
        member EmbeddedPath : EmbeddedResource -> string

        /// Path or URL to the `.js` JavaScript file corresponding to an assembly.
        member JavaScriptPath : AssemblyId -> string

        /// Filename for the `.map` source mapping file corresponding to an assembly.
        member MapFileName : AssemblyId -> string

        /// Path or URL to the `.map` source mapping file corresponding to an assembly.
        member MapFilePath : AssemblyId -> string

        /// Filename for the `.js` JavaScript file corresponding to an assembly.
        member JavaScriptFileName : AssemblyId -> string

        /// Path or URL to the `.min.js` JavaScript file corresponding to an assembly.
        member MinifiedJavaScriptPath : AssemblyId -> string

        /// Filename for the `.min.js` JavaScript file corresponding to an assembly.
        member MinifiedJavaScriptFileName : AssemblyId -> string

        /// Path or URL to the `.min.map` source mapping file corresponding to an assembly.
        member MinifiedMapFileName : AssemblyId -> string

        /// Path or URL to the `.min.map` source mapping file corresponding to an assembly.
        member MinifiedMapFilePath : AssemblyId -> string

        /// Path or URL to the `.d.ts` TypeScript file corresponding to an assembly.
        member TypeScriptDefinitionsPath : AssemblyId -> string

        /// Filename for the `.d.ts` TypeScript file corresponding to an assembly.
        member TypeScriptDefinitionsFileName : AssemblyId -> string

        /// Constructs a utiltiy object based on the physical path to the
        /// web root folder, which can be obtained by `Server.MapPath("~")`.
        static member FileSystem : rootDirectory: string -> PathUtility

        /// Constructs a utiltiy object for computing URLs based on a given
        /// virtual path root (such as '/').
        static member VirtualPaths : virtualPathRoot: string -> PathUtility
