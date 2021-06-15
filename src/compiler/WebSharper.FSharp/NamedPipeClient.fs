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

open System.Diagnostics
open System.IO.Pipes
open System.Threading
open System.Runtime.Serialization.Formatters.Binary
open WebSharper.Compiler.WsFscServiceCommon
open System.IO
open NLog.FSharp

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


let sendCompileCommand args =
    let nLogger = Logger()
    let serverName = "." // local machine server name
    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    nLogger.Debug "location of wsfsc.exe and wsfscservice.exe: %s" location
    let pipeName = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    nLogger.Debug "pipeName is : %s" pipeName
#if DEBUG
    printfn "pipeName is : %s" pipeName
#endif
    let fileNameOfService = (location, "wsfscservice.exe") |> System.IO.Path.Combine
    let runningServers =
        try
        Process.GetProcessesByName("wsfscservice")
            |> Array.filter (fun x -> System.String.Equals(x.MainModule.FileName, fileNameOfService, System.StringComparison.OrdinalIgnoreCase))
        with
        | e ->
            // If the processes cannot be queried, because of insufficient rights, the Mutex in service will handle
            // not running 2 instances
            nLogger.ErrorException e "Could not read running processes of wsfscservice."
            [||]

    nLogger.Debug "number of running wsfscservices (> 0 means server is not needed): %i" runningServers.Length
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
        nLogger.Debug "Started service PID=%d" proc.Id

    // the singleton wsfscservice.exe collects cache about metadata, and have the compiler in memory.
    // Call that with the compilation args for compilation.
    use clientPipe = new NamedPipeClientStream( serverName, //server name, local machine is .
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // direction of the pipe 
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
    let Write (bytes: byte array) =
        if clientPipe.IsConnected && clientPipe.CanWrite then
            let unexpectedFinishErrorCode = -12211
            let write = async {
                let printResponse (bytes: byte array) = 
                    async {
                    let message = System.Text.Encoding.UTF8.GetString(bytes)
                        // messages on the service have n: e: or x: prefix for stdout stderr or error code kind of output
                    match message with
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
                            nLogger.Error "Unrecognizable message from server (%i): %s" unrecognizedMessageErrorCode x
                            return unrecognizedMessageErrorCode |> Some
                    }
                do! clientPipe.AsyncWrite(bytes, 0, bytes.Length)
                clientPipe.Flush()
                let! errorCode = readingMessages clientPipe printResponse
                match errorCode with
                | Some x -> return x
                | None -> 
                    nLogger.Error "Listening for server finished abruptly (%i)" unexpectedFinishErrorCode
                    return unexpectedFinishErrorCode
                }
            try
                Async.RunSynchronously(write)
            with
            | _ -> 
                nLogger.Error "Listening for server finished abruptly (%i)" unexpectedFinishErrorCode
                unexpectedFinishErrorCode
        else
            let cannotConnectErrorCode = -14411
            eprintfn "ClientPipe cannot connect (%i)" cannotConnectErrorCode
            cannotConnectErrorCode
            
 


    let bf = new BinaryFormatter();
    use ms = new MemoryStream()

    nLogger.Debug "WebSharper compilation arguments:"
    args |> Array.iter (nLogger.Debug "    %s")
    // args going binary serialized to the service.
    let startCompileMessage: ArgsType = {args = args}
    bf.Serialize(ms, startCompileMessage);
    ms.Flush();
    let data = ms.ToArray();
    clientPipe.Connect()
    clientPipe.ReadMode <- PipeTransmissionMode.Message
    let returnCode = Write data
    nLogger.Debug "wsfscservice.exe compiled in %s with error code: %i" location returnCode
    returnCode
