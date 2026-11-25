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
module WebSharper.FSharp.NamedPipeClient
#nowarn "44"
open WebSharper.Compiler

open System.Text


open System.Diagnostics
open System.IO.Pipes
open System.Runtime.InteropServices
open WebSharper.Compiler.WsFscServiceCommon
open System.IO

let (|StdOut|_|) (str: string) =
    if str.StartsWith("n: ") then
        str.Substring(3) |> Some
    else
        None

let (|StdErr|_|) (str: string) =
    if str.StartsWith("e: ") then
        str.Substring(3) |> Some
    else
        None

let (|Finish|_|) (str: string) =
    if str.StartsWith("x: ") then
        let trimmed = 
            str.Substring(3)
        trimmed
        |> System.Int32.Parse
        |> Some
    else
        None

let retryErrorCode = -12212

/// Starts a detached booster from the folder of this compiler.
let startBooster (location: string) (logArgs: string list) =
    WebSharper.Compiler.BoosterProcess.start location "wsfscservice" logArgs

let sendCompileCommand args (logPath: string) useLogs (cLogger: ConsoleLogger) =
    let serverName = "." // local machine server name
    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeNameRaw = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    let pipeName =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            pipeNameRaw  // Windows uses simple pipe names
        else
            Path.Combine(Path.GetTempPath(), pipeNameRaw) // Linux/macOS require 

    let newClientPipe () =
        new NamedPipeClientStream( serverName, //server name, local machine is .
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // direction of the pipe 
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)

    // A running booster of this folder listens on its pipe. Probing the pipe finds it whether it runs
    // as an apphost or as dotnet wsfscservice.dll, which looks like any other dotnet process.
    let tryConnect (timeout: int) =
        let pipe = newClientPipe ()
        try
            pipe.Connect(timeout)
            Some pipe
        with _ ->
            pipe.Dispose()
            None

    // the singleton wsfscservice collects cache about metadata, and have the compiler in memory.
    // Call that with the compilation args for compilation.
    use clientPipe =
        match tryConnect 300 with
        | Some pipe -> pipe
        | None ->
            // start a detached booster. If another client started one meanwhile, the new one exits
            // (one booster per folder, guarded by a named mutex) and we connect to the other.
            let logArgs = if useLogs then [ logPath ] else []
            try
                startBooster location logArgs
            with e ->
                cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build. Could not start the booster: %s" e.Message)
                raise <| PipeException()
            match tryConnect 10000 with
            | Some pipe -> pipe
            | None ->
                cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check %s for more information." logPath)
                raise <| PipeException()
    let Write (ms: byte []) =
        if clientPipe.IsConnected && clientPipe.CanWrite then
            let unexpectedFinishErrorCode = -12211
            let write = async {
                let printResponse (message: obj) = 
                    async {
                        // messages on the service have n: e: or x: prefix for stdout stderr or error code kind of output
                        let jE = message :?> Json.JsonElement
                        match jE.GetString() with
                        | StdOut n ->
                            printfn "%s" n
                            return None
                        | StdErr e ->
                            eprintfn "%s" e
                            return None
                        | Finish i -> 
                            return i |> Some
                        | x -> 
                            let unrecognizedMessageErrorCode = -13311
                            return unrecognizedMessageErrorCode |> Some
                    }
                clientPipe.Write(System.BitConverter.GetBytes(ms.Length), 0, 4) // prepend with message length
                clientPipe.Write(ms, 0, ms.Length)
                clientPipe.Flush()
                if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
                    clientPipe.WaitForPipeDrain()

                let clientError () =
                    if clientPipe.IsConnected |> not then
                        raise <| PipeException ()
                
                let! errorCode = readingMessages clientPipe printResponse clientError
                match errorCode with
                | Some -12211 ->
                    return -12211
                | Some x -> return x
                | None ->
                    return unexpectedFinishErrorCode
                }
            try
                Async.RunSynchronously(write)
            with
            | PipeException _ as ex ->
                cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check %s for more information." logPath)
                retryErrorCode
            | _ -> 
                unexpectedFinishErrorCode
        else
            let cannotConnectErrorCode = -14411
            eprintfn "ClientPipe cannot connect (%i)" cannotConnectErrorCode
            cannotConnectErrorCode
            
 


    // args going binary serialized to the service.
    let startCompileMessage: ArgsType = {args = args}
    try
        if clientPipe.IsConnected then
            clientPipe.ReadMode <- PipeTransmissionMode.Byte
            let options = System.Text.Json.JsonSerializerOptions()
            options.Encoder <- Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
            let res = System.Text.Json.JsonSerializer.Serialize(startCompileMessage, options)
            let bytes = System.Text.Encoding.UTF8.GetBytes res
            let returnCode = Write bytes
            returnCode
        else
            cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check %s for more information." logPath)
            raise <| PipeException()
    with
    | _ ->
        cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check %s for more information." logPath)
        raise <| PipeException()
