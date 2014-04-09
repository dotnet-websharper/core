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

namespace IntelliFactory.WebSharper.Templates

/// Options for resolving WebSharper with NuGet.
type NuGetOptions =
    {
        /// Path to a directory where packages should be installed.
        PackagesDirectory : string
    }

    /// Creates default options.
    static member Create : unit -> NuGetOptions

/// Configures local WebSharper installation.
type LocalWebSharperSource =
    {
        /// Path to WebSharper.targets
        TargetsFile : string

        /// Path to templates directory.
        TemplatesDirectory : string
    }

    /// Creates default source using standard layout under root.
    static member Create : root: string -> LocalWebSharperSource

/// Defines where to obtain WebSharper for the templates.
[<Sealed>]
type WebSharperSource =

    /// Query NuGet repository to obtain the latest WebSharper.
    static member FromNuGet : ?version: string * ?opts: NuGetOptions -> WebSharperSource

    /// Specify a local installation.
    static member Local : LocalWebSharperSource -> WebSharperSource

/// Options for initializing templates.
type InitOptions =
    {
        /// Directory that gets created and/or populated.
        Directory : string

        /// The name of the project to create.
        ProjectName : string

        /// Defines how to obtain WebSharper.
        WebSharperSource : WebSharperSource
    }

    /// Creates default options.
    static member Create : unit -> InitOptions

/// Represents a WebSharper MSBuild/XBuild template.
[<Sealed>]
type Template =

    /// Initializes a new instance of the template.
    member Init : InitOptions -> unit

    /// All templates.
    static member All : list<Template>

    /// The bundle-website template.
    static member BundleWebsite : Template

    /// The extension template.
    static member Extension : Template

    /// The library template.
    static member Library : Template

    /// The sitelets-host template.
    static member SiteletsHost : Template

    /// The sitelets-html template.
    static member SiteletsHtml : Template

    /// The sitelets-website template.
    static member SiteletsWebsite : Template

