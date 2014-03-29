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

[<Sealed>]
type Content(t: Lazy<string>) =
    static let utf8 = UTF8Encoding(false, true) :> Encoding

    member c.Map(f) =
        Content(lazy f t.Value)

    member c.Write(output: TextWriter) =
        output.Write(t.Value)

    member c.WriteFile(fileName: string, ?encoding: Encoding) =
        let enc = defaultArg encoding utf8
        let dir = DirectoryInfo(Path.GetDirectoryName(fileName))
        if not dir.Exists then
            dir.Create()
        File.WriteAllText(fileName, t.Value, enc)

    member c.Text = t.Value

    static member Create(t) =
        Content(t)
