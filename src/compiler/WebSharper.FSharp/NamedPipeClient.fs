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
open System.Runtime.Serialization.Formatters.Binary
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

let sendCompileCommand args =
    printfn "Using WebSharper compiler service"
    let serverName = "." // local machine server name
    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
#if DEBUG
    printfn "location of wsfsc.exe and wsfscservice.exe: %s" location
#endif
    let pipeName = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPipeName
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
#if DEBUG
            eprintfn "Could not read running processes of wsfscservice. Error : %s" e.Message
#endif
            [||]


#if DEBUG
    printfn "number of running wsfscservices (> 0 means server is not needed): %i" runningServers.Length
#endif
    let isServerNeeded =
        runningServers |> Array.isEmpty

    let mutable returnCode = 0
    let mutable proc: Process = null
    if isServerNeeded then
        if (System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows)) then
            let cmdName = (location, "wsfscservice_start.cmd") |> System.IO.Path.Combine
            let startInfo = ProcessStartInfo(cmdName) // ProcessStartInfo(fileNameOfService)
            startInfo.CreateNoWindow <- true
            startInfo.UseShellExecute <- false
            startInfo.WindowStyle <- ProcessWindowStyle.Hidden
            proc <- Process.Start(startInfo)
#if DEBUG
            printfn "Started service PID=%d" proc.Id
#endif

// TODO: decide what is best, this starts proc directly:
//#if DEBUG
//        printfn "Starting service at %s" fileNameOfService
//#endif
//        let startInfo = ProcessStartInfo(fileNameOfService)
//        startInfo.CreateNoWindow <- true
//        startInfo.UseShellExecute <- true
//        startInfo.WindowStyle <- ProcessWindowStyle.Hidden
//        proc <- Process.Start(startInfo)
//        proc.Exited.AddHandler exitedEventHandler
//#if DEBUG
//        printfn "Started service PID=%d" proc.Id
//#endif

    use clientPipe = new NamedPipeClientStream( serverName, //server name, local machine is .
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // direction of the pipe 
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
    let Write (bytes: byte array) =
        if clientPipe.IsConnected && clientPipe.CanWrite then
            let write = async {
                let printResponse (bytes: byte array) = 
                    async {
                        let message = System.Text.Encoding.UTF8.GetString(bytes)
                        match message with
                        | StdOut n ->
                            printfn "%s" n
                            return false
                        | StdErr e ->
                            eprintfn "%s" e
                            return false
                        | Finish i -> 
                            returnCode <- i
#if DEBUG
                            printfn "wsfscservice.exe compiled in %s with error code: %i" location i
#endif
                            return true
                        | x -> 
#if DEBUG
                            eprintfn "Unrecognizable message from server: %s" x
#endif
                            return true
                    }
                do! clientPipe.AsyncWrite(bytes, 0, bytes.Length)
                clientPipe.Flush()
                let! ok = readingMessages clientPipe printResponse
                if not ok then 
                    eprintfn "Listening for server finished abruptly"
                    returnCode <- -12211
                }
            try
                Async.RunSynchronously(write)
            with
            | _ -> () // Pokemon

    let bf = new BinaryFormatter();
    use ms = new MemoryStream()

#if DEBUG
    printfn "WebSharper compilation arguments:"
    args |> Array.iter (printfn "    %s")
#endif
    let startCompileMessage: ArgsType = {args = args}
    bf.Serialize(ms, startCompileMessage);
    ms.Flush();
    let data = ms.ToArray();
    clientPipe.Connect()
    clientPipe.ReadMode <- PipeTransmissionMode.Message
    Write data
    returnCode
