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
