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

module WebSharper.FSharp.Program

open System.IO
open WebSharper.Compiler
module C = WebSharper.Compiler.Commands

open FSharp.Compiler.SourceCodeServices
open WebSharper.Compiler.FSharp.Compile
open WebSharper.FSharp.NamedPipeClient
open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FSharp.ErrorPrinting

let formatArgv (argv: string[]) =
    match argv with
    | [| a |] when a.StartsWith "@" ->
        File.ReadAllLines a.[1..]
    | [| f; a |] when (f.EndsWith "fsc.exe" || f.EndsWith "fsc.dll") && a.StartsWith "@" ->
        Array.append [| f |] (File.ReadAllLines a.[1..])
    | _ -> argv

[<EntryPoint>]
let main(argv) =
    let standaloneMode = argv |> Array.exists (fun x -> System.String.Equals(x, "--standalone", System.StringComparison.CurrentCultureIgnoreCase))
    // --ws:extension and --ws:interfaceGenerator they are aliases
    let extension = argv |> Array.exists (fun x -> System.String.Equals(x, "--ws:extension", System.StringComparison.CurrentCultureIgnoreCase))
    let interfaceGenerator = argv |> Array.exists (fun x -> System.String.Equals(x, "--ws:interfaceGenerator", System.StringComparison.CurrentCultureIgnoreCase))

    let argv = argv |> Array.filter (fun x -> x <> "--standalone")
    if standaloneMode || extension || interfaceGenerator then
        System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
        let createChecker() = FSharpChecker.Create(keepAssemblyContents = true)
        let tryGetMetadata = WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
        let logger = ConsoleLogger()   
#if DEBUG
        compileMain (formatArgv argv) createChecker tryGetMetadata logger 
#else
        try compileMain (formatArgv argv) createChecker tryGetMetadata logger
        with 
        | ArgumentError msg -> 
            PrintGlobalError logger (msg + " - args: " + (formatArgv argv |> String.concat " "))
            1
        | e -> 
            PrintGlobalError logger (sprintf "Global error: %A" e)
            1
#endif
    else
        sendCompileCommand (formatArgv argv)
