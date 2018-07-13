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

/// Test Runner.
module WebSharper.Testing.Runner

open WebSharper
open WebSharper.JavaScript
module M = WebSharper.Core.Metadata
//module R = WebSharper.Core.Reflection

[<JavaScript>]
type private RunnerControlBody(run) =
    interface IControlBody with
        member this.ReplaceInDom(e) =
            match JS.Document.QuerySelector "#qunit" with
            | null ->
                let fixture = JS.Document.CreateElement("div")
                fixture.SetAttribute("id", "qunit-fixture")
                let qunit = JS.Document.CreateElement("div")
                qunit.SetAttribute("id", "qunit")
                let parent = e.ParentNode
                parent.ReplaceChild(fixture, e) |> ignore
                parent.InsertBefore(qunit, fixture) |> ignore
            | _ -> ()
            run()

[<JavaScript>]
let RunTests (tests: seq<TestCategory>) =
    new RunnerControlBody(fun () ->
        for t in tests do
            QUnit.Module t.name
            t.run()
    ) :> IControlBody
