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
module WebSharper.SPA.Tests.Main

open WebSharper
open WebSharper.JavaScript

let private addOne (c: IControlBody) =
    let e = JS.Document.CreateElement("div")
    JS.Document.QuerySelector("#main").AppendChild(e) |> ignore
    c.ReplaceInDom(e)

[<SPAEntryPoint>]
[<Require(typeof<WebSharper.Tests.TwitterBootstrap>)>]
let Main() =
    WebSharper.CSharp.Tests.Remoting.ShouldRun <- false
    Seq.iter addOne [
        WebSharper.Tests.Main.RunTests false true
        WebSharper.Collections.Tests.Main.RunTests()
        WebSharper.CSharp.Tests.Tests.RunTests()
        WebSharper.Html5.Tests.Main.RunTests()
        WebSharper.Web.Tests.Main.RunTests "" false
    ]

[<assembly: JavaScript>]
[<assembly: JavaScriptExport("File1.fs")>]
[<assembly: JavaScriptExport("WebSharper.SPA.Tests.JavaScriptExportTest2+IncludedFromAssemblyLevel1")>]
[<assembly: JavaScriptExport(typeof<JavaScriptExportTest2.IncludedFromAssemblyLevel2>)>]
do ()
