// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Build.Minify

open System
open System.IO
open Microsoft.Ajax.Utilities

let Run () =

    let minify (path: string) =
        let min = Minifier()
        let out = Path.ChangeExtension(path, ".min.js")
        if NeedsBuilding path out then
            let raw = File.ReadAllText(path)
            let mjs = min.MinifyJavaScript(raw)
            File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
            stdout.WriteLine("Written {0}", out)

    minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"
    minify "src/stdlib/WebSharper.Main/Json.js"
    minify "src/stdlib/WebSharper.Main/AnimFrame.js"
