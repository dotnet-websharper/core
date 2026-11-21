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

module WebSharper.CSharp.Program

open System.IO
open WebSharper.Compiler

open WebSharper.Compiler.CommandTools
module C = WebSharper.Compiler.Commands
open WebSharper.Compiler.CSharp.Compile
open WebSharper.Compiler.CSharp.ErrorPrinting

let formatArgv (argv: string[]) =
    match argv with
    | [| a |] when a.StartsWith "@" -> File.ReadAllLines a.[1..]
    | _ -> argv

[<EntryPoint>]
let main argv =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    let tryGetMetadata = WebSharper.Compiler.FrontEnd.TryReadFromAssemblyWith
    let logger = BoosterReport.RecordingLogger()
    let mutable skipped = false
    logger.StageObserver <- System.Action<string, System.TimeSpan>(fun stage _ -> if stage = BoosterReport.skippedStage then skipped <- true)
    let mutable config = None
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let exitCode =
        try
            compileMainWith (formatArgv argv) tryGetMetadata logger (fun c -> config <- Some c)
        with
        | ArgumentError "" ->
            1
        | ArgumentError msg ->
            PrintGlobalError logger msg
            1
        | e ->
            PrintGlobalError logger (sprintf "Global error: %A" e)
            1
    // the C# booster's agent tools answer from this compilation
    config |> Option.iter (fun c -> BoosterReport.send c exitCode stopwatch.Elapsed skipped logger.Lines)
    exitCode
