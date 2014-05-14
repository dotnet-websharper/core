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

open System
open System.IO

/// Represents a set of (template) files.
[<Sealed>]
type FileSet =

    /// Writes to a given directory.
    member Populate : targetDir: string -> unit

    /// "cd" operation on a FileSet.
    member Item : string -> FileSet with get

    /// Reads all files in a given directory.
    static member FromDirectory : path: string -> FileSet

    /// Reads all files in a given zip stream.
    static member FromZip : Stream -> FileSet

    /// Reads all files in a given zip file.
    static member FromZipFile : path: string -> FileSet

