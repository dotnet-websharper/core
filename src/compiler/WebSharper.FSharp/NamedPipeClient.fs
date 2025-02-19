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
module WebSharper.FSharp.NamedPipeClient
#nowarn "44"
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

let sendCompileCommand args projectDir =
    let serverName = "." // local machine server name
    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeNameRaw = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    let pipeName =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            pipeNameRaw  // Windows uses simple pipe names
        else
            Path.Combine(Path.GetTempPath(), pipeNameRaw) // Linux/macOS require 
    let exeName =
        if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
            "wsfscservice.exe"
        else 
            "wsfscservice"
    let fileNameOfService = (location, exeName) |> System.IO.Path.Combine
    let runningServers =
        try
            Process.GetProcessesByName("wsfscservice")
            |> Array.filter (fun x -> System.String.Equals(x.MainModule.FileName, fileNameOfService, System.StringComparison.OrdinalIgnoreCase))
        with
        | e ->
            // If the processes cannot be queried, because of insufficient rights, the Mutex in service will handle
            // not running 2 instances
            [||]

    let isServerNeeded =
        runningServers |> Array.isEmpty

    let mutable proc: Process = null
    if isServerNeeded then
        // start a detached wsfscservice.exe. Platform specific.
        let cmdName = if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
                        "wsfscservice_start.cmd" else "wsfscservice_start.sh"
        let cmdFullPath = (location, cmdName) |> System.IO.Path.Combine
        let startInfo = ProcessStartInfo(cmdFullPath)
        startInfo.CreateNoWindow <- true
        startInfo.UseShellExecute <- false
        startInfo.WindowStyle <- ProcessWindowStyle.Hidden
        proc <- Process.Start(startInfo)

    // the singleton wsfscservice.exe collects cache about metadata, and have the compiler in memory.
    // Call that with the compilation args for compilation.
    use clientPipe = new NamedPipeClientStream( serverName, //server name, local machine is .
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // direction of the pipe 
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
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
                clientPipe.WaitForPipeDrain()
                let! errorCode = readingMessages clientPipe printResponse
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
            | _ -> 
                unexpectedFinishErrorCode
        else
            let cannotConnectErrorCode = -14411
            eprintfn "ClientPipe cannot connect (%i)" cannotConnectErrorCode
            cannotConnectErrorCode
            
 


    // args going binary serialized to the service.
    let startCompileMessage: ArgsType = {args = args}
    clientPipe.Connect()
    clientPipe.ReadMode <- PipeTransmissionMode.Byte
    let options = System.Text.Json.JsonSerializerOptions()
    options.Encoder <- Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    let res = System.Text.Json.JsonSerializer.Serialize(startCompileMessage, options)
    let bytes = System.Text.Encoding.UTF8.GetBytes res
    let returnCode = Write bytes
    returnCode
