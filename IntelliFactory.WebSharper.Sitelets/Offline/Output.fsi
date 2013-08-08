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

/// Implements offline sitelet HTML generation.
module internal IntelliFactory.WebSharper.Sitelets.Offline.Output

open System
open System.IO
open IntelliFactory.Core
open IntelliFactory.WebSharper.Sitelets

/// The output mode, Debug or Release.
type Mode =
    | Debug
    | Release

/// Configuration options.
type Config =
    {
        Actions : list<obj>
        Mode : Mode
        Sitelet : Sitelet<obj>
        SourceDirs : list<DirectoryInfo>
        TargetDir : DirectoryInfo
    }

/// Writes a site given the configuration options.
val WriteSite : AssemblyResolver -> config: Config -> unit
