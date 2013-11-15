// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.VisualStudioTools

/// Provides a facility to generate VisualStudio extension `.vsix` files
/// using the 2010 VSIX format compatible with VisualStudio 2010 and VisualStudio 2012,
/// see <http://msdn.microsoft.com/en-us/library/vstudio/dd393754(v=vs.100).aspx>
/// Quickstart: use the static methods on `VsixFile` to construct
/// an in-memory `.vsix` representation you can then manipulate.
module Extensions =
    open System
    open System.Globalization

    /// Represents VisualStudio editions.
    [<Sealed>]
    type VSEdition =
        static member IntegratedShell : VSEdition
        static member ExpressAll : VSEdition
        static member Premium : VSEdition
        static member Pro : VSEdition
        static member Ultimate : VSEdition
        static member VBExpress : VSEdition
        static member VCExpress : VSEdition
        static member VCSExpress : VSEdition
        static member VWDExpress : VSEdition

    /// Unifies different kinds of supported product declarations.
    [<Sealed>]
    type SupportedProduct

    /// Represents a VisualStudio product entry.
    [<Sealed>]
    type VSProduct =

        /// A `VSProduct` is a `SupportedProduct`.
        member AsSupportedProduct : unit -> SupportedProduct

        /// Constructs a new VisualStudio product entry.
        static member Create : version: string * editions: seq<VSEdition> -> VSProduct

    /// Represents an Isolated Shell application product entry.
    [<Sealed>]
    type IsolatedShellProduct =

        /// An `IsolatedShellProduct` is a `SupportedProduct`.
        member AsSupportedProduct : unit -> SupportedProduct

        /// Constructs the `IsolatedShellProduct`.
        static member Create : name: string * ?version: string -> IsolatedShellProduct

    /// More methods on `SupportedProduct`.
    type SupportedProduct with

        /// An `IsolatedShellProduct` is a `SupportedProduct`.
        static member IsolatedShell : IsolatedShellProduct -> SupportedProduct

        /// A `VSProduct` is a `SupportedProduct`.
        static member VSProduct : VSProduct -> SupportedProduct

    /// Represents extension identification.
    [<Sealed>]
    type Identifier =

        /// Sets the set of supported products.
        member WithProducts : seq<SupportedProduct> -> Identifier

        /// Sets the version.
        member WithVersion  : Version -> Identifier

        /// Creates a bare-bones identification section.
        static member Create :
            author: string *
            id: Templates.ExtensionIdentity *
            name: string *
            description: string -> Identifier

    /// Unifies different types of content declarations.
    [<Sealed>]
    type VsixContent =

        /// Helper for quick definition of template contents.
        static member ProjectTemplate : category: seq<string> * archive: Templates.ProjectTemplate -> VsixContent

    /// Respresents the top-level configuration element.
    [<Sealed>]
    type Vsix =

        /// Constructs a new `Vsix` element.
        static member Create : Identifier * contents: seq<VsixContent> -> Vsix

    /// Represents an in-memory `.vsix` package.
    [<Sealed>]
    type VsixFile =

        /// The filename name of the file.
        member FileName : string

        /// Writes the `.vsix` file to a directory.
        member WriteToDirectory : directoryPath: string -> unit

        /// Creates a `VsixFile`.
        static member Create : fileName: string * vsix: Vsix -> VsixFile

        /// Reads a `.vsix` file.
        static member FromFile : fullPath: string -> VsixFile
