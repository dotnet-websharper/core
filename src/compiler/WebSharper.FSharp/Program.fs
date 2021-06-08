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
open WebSharper.Compiler.LoggerBase

let formatArgv (argv: string[]) =
    match argv with
    | [| a |] when a.StartsWith "@" ->
        File.ReadAllLines a.[1..]
    | [| f; a |] when (f.EndsWith "fsc.exe" || f.EndsWith "fsc.dll") && a.StartsWith "@" ->
        Array.append [| f |] (File.ReadAllLines a.[1..])
    | _ -> argv

[<EntryPoint>]
let main(argv) =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(printfn "%s", checker)
    let tryGetMetadata = (WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata)
    let logger = {
        new LoggerBase() with
            override _.Error s = 
                eprintfn "%s" s
            override _.Out s =
                printfn "%s" s
        }
    
#if DEBUG
    compileMain (formatArgv argv) checker compiler tryGetMetadata logger 
#else
    try compileMain (formatArgv argv) checker compiler tryGetMetadata logger
    with 
    | ArgumentError msg -> 
        PrintGlobalError (msg + " - args: " + (formatArgv argv |> String.concat " "))
        1    
    | e -> 
        PrintGlobalError (sprintf "Global error: %A" e)
        1
#endif

