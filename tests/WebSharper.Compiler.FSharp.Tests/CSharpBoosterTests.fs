// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// The C# booster's agent extensions (wscscservice).
module Compiler.CSharpBoosterTests

open NUnit.Framework
open FsUnit
open WebSharper.CSharp.Service

/// An extension of the C# booster, found by its loadExtensions in this test assembly (the F# booster's
/// loadExtensions skips it, its interface is another one).
type TestCSharpExtension() =
    interface AgentTools.IAgentExtension with
        member _.Name = "test-csharp"
        member _.Tools =
            [ { Name = "echo_cs"; Description = "Echoes text."; Parameters = [ "text", "string", "The text.", true ]
                Run = fun args -> defaultArg (args.String "text") "" } ]
        member _.Instructions = "Use echo_cs."
        member _.Decorate name _ text = if name = "get_diagnostics" then text + "\n  more in echo_cs" else text
        member _.LocalTools = [ "echo_cs" ]

let private report (extensions: string[]) : AgentTools.CompileReport =
    {
        Project = "/p/App.csproj"; Assembly = "/p/obj/App.dll"; References = [||]; ProjectType = "Library"
        ExitCode = 0; DurationMs = 1.; Skipped = false; SourceFiles = 1; Log = [||]; Extensions = extensions
    }

[<Test>]
let ``The C# booster loads the extensions reported with a compilation and offers their tools`` () =
    AgentTools.loadReportedExtensions (report [| typeof<TestCSharpExtension>.Assembly.Location |])
    AgentTools.loadedExtensions () |> List.map (fun e -> e.Name) |> should contain "test-csharp"
    let edition = AgentTools.edition ()
    let names = edition.Tools |> List.map (fun t -> t.Name)
    for t in AgentTools.tools do names |> should contain t.Name
    names |> should contain "echo_cs"
    edition.Instructions |> should haveSubstring "Use echo_cs."
    edition.LocalTools |> should equal [ "echo_cs" ]
    edition.Decorate "get_diagnostics" (AgentTools.ToolArgs(null)) "x" |> should equal "x\n  more in echo_cs"
    AgentTools.callTool "echo_cs" """{"text":"hi"}""" |> should equal (Ok "hi" : Result<string, string>)
    AgentTools.toolSource "echo_cs" |> should equal "test-csharp"
    AgentTools.loadExtensions "/no/such/extension.dll" |> Result.isError |> should equal true

[<Test>]
let ``Reports of an older wscsc, without extensions, are still recorded`` () =
    let json = """{"Project":"/p/Old.csproj","Assembly":"/p/obj/Old.dll","References":[],"ProjectType":"Library","ExitCode":0,"DurationMs":1,"Skipped":false,"SourceFiles":1,"Log":[]}"""
    let r = System.Text.Json.JsonSerializer.Deserialize<AgentTools.CompileReport>(json, System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true))
    r.Extensions |> should be Null
    AgentTools.loadReportedExtensions r
    AgentTools.record r
    AgentTools.callTool "list_projects" "{}" |> Result.isOk |> should equal true
