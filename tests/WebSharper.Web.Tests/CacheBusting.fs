// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Web.Tests.CacheBusting

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let private attributeValues selector attr =
    let nodes = JS.Document.QuerySelectorAll(selector)
    [| for i in 0 .. nodes.Length - 1 -> (As<Dom.Element> (nodes.Item i)).GetAttribute(attr) |]

[<JavaScript>]
let Tests =
    TestCategory "Cache busting" {

        Test "Bundle script links have content hash" {
            let srcs = attributeValues "script[src*='/Scripts/WebSharper/tests.js']" "src"
            equal srcs.Length 1
            isTrue (srcs.[0].Contains("tests.js?h="))
        }

        Test "Bundle css links have content hash" {
            let hrefs = attributeValues "link[href*='/Scripts/WebSharper/tests.css']" "href"
            equal hrefs.Length 1
            isTrue (hrefs.[0].Contains("tests.css?h="))
        }

        Test "Worker script paths have content hash" {
            let version = JS.Global?WebSharperRuntime?ScriptVersion : string
            isTrue (version.StartsWith("?h="))
        }
    }
