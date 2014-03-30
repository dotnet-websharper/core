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

[<AutoOpen>]
module internal Abbreviations =
    open System
    open System.Collections

    type Activator = System.Activator
    type AssemblyName = Reflection.AssemblyName
    type AssemblyResolver = IntelliFactory.Core.AssemblyResolution.AssemblyResolver
    type CustomAttributeData = Reflection.CustomAttributeData
    type IDictionary<'T1,'T2> = Generic.IDictionary<'T1,'T2>
    type Dictionary<'T1,'T2> = Generic.Dictionary<'T1,'T2>
    type DirectoryInfo = IO.DirectoryInfo
    type Encoding = Text.Encoding
    type File = IO.File
    type FileInfo = IO.FileInfo
    type HtmlTextWriter = Web.UI.HtmlTextWriter
    type InvalidOperationException = System.InvalidOperationException
    type MemoryStream = IO.MemoryStream
    type Path = IO.Path
    type Queue<'T> = Generic.Queue<'T>
    type Stream = IO.Stream
    type StreamReader = IO.StreamReader
    type StreamWriter = IO.StreamWriter
    type String = System.String
    type StringWriter = IO.StringWriter
    type StrongNameKeyPair = Reflection.StrongNameKeyPair
    type TextWriter = IO.TextWriter
    type Type = System.Type
    type UTF8Encoding = Text.UTF8Encoding
