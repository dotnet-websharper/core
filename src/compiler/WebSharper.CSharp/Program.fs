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
    let tryGetMetadata = WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
    let logger = ConsoleLogger()
    try
        compileMain (formatArgv argv) tryGetMetadata logger 
    with
    | ArgumentError "" -> 
        1    
    | ArgumentError msg -> 
        PrintGlobalError logger msg
        1    
    | e -> 
        PrintGlobalError logger (sprintf "Global error: %A" e)
        1
