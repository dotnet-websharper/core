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

/// Reports a compilation to the C# booster (wscscservice) of this installation, for its agent tools: what was
/// printed and where the outputs are. The booster does not compile, wscsc compiles before reporting. Best effort:
/// a booster that can't be started or reached changes nothing for the build.
module WebSharper.CSharp.BoosterReport

open System
open System.IO
open WebSharper.Compiler
open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.WsFscServiceCommon
open WebSharper.CSharp.Service

/// A logger printing like ConsoleLogger that keeps the printed lines for the report.
type RecordingLogger() =
    inherit LoggerBase()
    let lines = ResizeArray<AgentTools.LogLine>()
    member _.Lines = lock lines (fun () -> lines.ToArray())
    override x.Error s =
        let s = x.Indent s
        lock lines (fun () -> lines.Add { IsError = true; Text = s })
        Console.Error.WriteLine s
    override x.Out s =
        let s = x.Indent s
        lock lines (fun () -> lines.Add { IsError = false; Text = s })
        Console.Out.WriteLine s

/// The stage Compile reports when the WebSharper outputs are up to date.
let skippedStage = "WebSharper resources already exist, skipping"

let private disabled (config: WsConfig) =
    config.Standalone
    || isNull config.ProjectFile
    || String.Equals(Environment.GetEnvironmentVariable "WebSharperBuildService", "false", StringComparison.OrdinalIgnoreCase)

/// Sends the report, starting the booster when none runs for this folder.
let send (config: WsConfig) (exitCode: int) (duration: TimeSpan) (skipped: bool) (lines: AgentTools.LogLine[]) =
    if not (disabled config) then
        try
            let location = Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly().Location)
            let pipe = cscBoosterPipeName location
            if not (BoosterPipe.exists pipe) then
                BoosterProcess.start location "wscscservice" []
                let sw = Diagnostics.Stopwatch.StartNew()
                while not (BoosterPipe.exists pipe) && sw.ElapsedMilliseconds < 5000L do
                    Threading.Thread.Sleep 50
            let report : AgentTools.CompileReport =
                {
                    Project = Path.GetFullPath config.ProjectFile
                    Assembly = (if isNull config.AssemblyFile then "" else Path.GetFullPath config.AssemblyFile)
                    References = config.References |> Array.map Path.GetFullPath
                    ProjectType = match config.ProjectType with Some t -> string t | None -> "Library"
                    ExitCode = exitCode
                    DurationMs = duration.TotalMilliseconds
                    Skipped = skipped
                    SourceFiles = config.CompilerArgs |> Array.filter (fun a -> a.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) && not (a.StartsWith "/") && not (a.StartsWith "-")) |> Array.length
                    Log = lines
                }
            let json = Text.Json.JsonSerializer.Serialize report
            BoosterPipe.send pipe [| "record"; json |] 2000 |> ignore
        with _ -> ()
