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

/// Provides a facility to generate VisualStudio template archive files.
/// These typically have a `.zip` extension and contain `.vstemplate` XML manifests.
/// Quickstart: use the static methods on `Archive` to construct
/// an in-memory `.zip` representation you can then manipulate.
module Templates =
    open System
    open System.IO
    module NG = IntelliFactory.VisualStudioTools.NuGet
    type Content = IntelliFactory.VisualStudioTools.Utils.Content

    /// Defines a project item corresponding to the `ProjectItem` XML element
    /// within VisualStudio project templates.
    /// See <http://msdn.microsoft.com/en-us/library/ys81cc94.aspx>
    [<Sealed>]
    type ProjectItem =

        /// Sets the flag triggering parameter replacement in the text of the item.
        member ReplaceParameters : ?value: bool -> ProjectItem

        /// Creates a new `ProjectItem` from explicit components.
        static member Create : fileName: string * content: Content -> ProjectItem

        /// Creates a new `ProjectItem` by reading a binary file.
        static member FromBinaryFile : fullPath: string -> ProjectItem

        /// Creates a new `ProjectItem` by reading a text file.
        static member FromTextFile : fullPath: string -> ProjectItem

    /// Subtypes for `Item`.
    /// See <http://msdn.microsoft.com/en-us/library/ms171408(v=vs.80).aspx>
    [<Sealed>]
    type ItemSubType

    /// Corresponds to the `ProjectItem` XML element
    /// describing a VisualStudio item template.
    /// See <http://msdn.microsoft.com/en-us/library/ms171408.aspx>
    [<Sealed>]
    type Item =

        /// Creates a new `ProjectItem` from explicit components.
        static member Create : fileName: string * content: Content -> Item

        /// Creates a new `ProjectItem` by reading a binary file.
        static member FromBinaryFile : fullPath: string -> Item

        /// Creates a new `ProjectItem` by reading a text file.
        static member FromTextFile : fullPath: string -> Item

    /// Represents contents of a folder.
    [<Sealed>]
    type FolderElement =

        /// A project item is a folder element.
        static member Nested : ProjectItem -> FolderElement

    /// Defines a folder of project items corresponding to the `Folder` XML element.
    /// See <http://msdn.microsoft.com/en-US/library/ahkztdcb(v=vs.110).aspx>
    [<Sealed>]
    type Folder =

        /// Creates a new `Folder` explicitly.
        static member Create : name: string * elements: seq<FolderElement> -> Folder

    /// More methods for `FolderElement`.
    type FolderElement with

        /// A folder is also a folder element.
        static member Folder : Folder -> FolderElement

    /// Represents a project corresponding to the `Project` XML element.
    /// See <http://msdn.microsoft.com/en-US/library/ms171401.aspx>
    [<Sealed>]
    type Project =

        /// Sets the flag triggering parameter replacement in the text of the item.
        member ReplaceParameters : ?value: bool -> Project

        /// Creates a new `Project` from explicit components.
        static member Create :
            fileName: string
            * content: string
            * elements: seq<FolderElement> ->
            Project

        /// Creates a new `Project` by reading a file.
        static member FromFile :
            fullPath: string
            * elements: seq<FolderElement> ->
            Project

    /// Represents project types.
    type ProjectType =
        | CSharp
        | FSharp
        | VisualBasic
        | Web

    /// Represents icons.
    [<Sealed>]
    type Icon =

        /// Creates explicitly.
        static member Create : fileName: string * content: Content -> Icon

        /// Creates from a file.
        static member FromFile : fullPath: string -> Icon

    /// Describes templates, corresponds to the `TemplateData` XML element.
    [<Sealed>]
    type TemplateData =

        /// Sets the default name for projects generated with this template.
        member WithDefaultProjectName : ?name: string -> TemplateData

        /// Creates with given required parameters.
        static member Create : ty: ProjectType * name: string * description: string * icon: Icon -> TemplateData

    /// Represents VisualStudio versions.
    [<Sealed>]
    type VisualStudioVersion =
        static member VS2008 : VisualStudioVersion
        static member VS2010 : VisualStudioVersion
        static member VS2012 : VisualStudioVersion

    /// The template kind used for installation.
    [<Sealed>]
    type TemplateKind

    /// Describes template installation options.
    [<Sealed>]
    type InstallConfig =

        /// Creates a new `InstallConfig`.
        static member Create : category: seq<string> * studio: VisualStudioVersion -> InstallConfig

    /// A globally unique VS extension identifier.
    [<Sealed>]
    type ExtensionIdentity =

        /// The combined name.
        member FullName : string

        /// The unique identifier
        member Guid : Guid

        /// The friendly name.
        member Name : string

        /// Creates a new identifier.
        static member Create : name: string * guid: Guid -> ExtensionIdentity

    /// A collection of NuGet packages required by the project templates
    /// together with a VS extension identity.s
    [<Sealed>]
    type NuGetPackages =

        /// The associated identity.
        member ExtensionIdentity : ExtensionIdentity

        /// The packages.
        member Packages : list<NG.Package>

        /// Creates a new set.
        static member Create : ExtensionIdentity * seq<NG.Package> -> NuGetPackages

    [<Sealed>]
    /// Corresponds to the `VSTemplate` element of type `Project`.
    type ProjectTemplate =

        /// The associated NuGet package collection.
        member NuGetPackages : option<NuGetPackages>

        /// Sets the associated NuGet package collection.
        member WithNuGetPackages : ?packages: NuGetPackages -> ProjectTemplate

        /// Creates a new ProjectTemplate.
        static member Create : TemplateData * Project -> ProjectTemplate

    /// An in-memory representation of a VisualStudio `.vstemplate` file.
    [<Sealed>]
    type Archive =

        /// Attempts to locally install the template.
        member Install : InstallConfig -> bool

        /// Attempts to locally uninstall the template.
        member Uninstall : InstallConfig -> bool

        /// Writes the zip file to a given directory.
        member WriteToDirectory : directoryPath : string -> unit

        /// File name of the associated zip arhive.
        member ZipContent : Content

        /// File name of the associated zip arhive.
        member ZipFileName : string

        /// Reads a specific zip file.
        static member FromFile : kind: TemplateKind * fullPath: string -> Archive

        /// Constructs a project template.
        static member Project : project: ProjectTemplate -> Archive


        (* TODO: item templates, multi-project (project group) templates *)
