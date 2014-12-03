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
