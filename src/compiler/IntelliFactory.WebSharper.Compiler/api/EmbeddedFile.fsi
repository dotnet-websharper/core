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

/// Represents embedded resource files.
[<Sealed>]
type EmbeddedFile =

    /// Reads the content.
    member GetContentData : unit -> byte []

    /// Reads the content.
    member Content : string

    /// The mime content type.
    member ContentType : string

    /// The file name.
    member FileName : string

    /// True for Script resources.
    member IsScript : bool

    static member internal Create :
        assemblyFullName: string
        * resourceName: string
        * bytes: byte[]
        * contentType: string ->
        EmbeddedFile
