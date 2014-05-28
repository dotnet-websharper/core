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

module CT = IntelliFactory.WebSharper.Core.ContentTypes

type EmbeddedFile =
    {
        ResAssembly : string
        mutable ResContent : string
        ResContentBytes : byte []
        ResContentType : CT.ContentType
        ResName : string
    }

    static member Create(assemblyFullName, resourceName, bytes, contentType) =
        {
            ResAssembly = assemblyFullName
            ResContent = null
            ResContentBytes = bytes
            ResContentType = contentType
            ResName = resourceName
        }

    member ri.GetContentData() =
        Array.copy ri.ResContentBytes

    member ri.Content =
        match ri.ResContent with
        | null ->
            try
                let s = UTF8Encoding(false, true).GetString(ri.ResContentBytes)
                ri.ResContent <- s
                s
            with e ->
                failwithf "Encoding problem in resource [%s] in assembly [%s]: %O"
                    ri.ResAssembly ri.ResName e
        | s -> s

    member ri.ContentType = ri.ResContentType
    member ri.FileName = ri.ResName

    member ri.IsScript =
        match ri.ResContentType with
        | CT.JavaScript -> true
        | _ -> false
