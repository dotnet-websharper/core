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

/// Starts the booster on Windows without inheriting any of this process's handles.
/// Process.Start with UseShellExecute=false always passes inheritable handles to the child,
/// so a booster started that way keeps the stdout/stderr pipes of this client open. When the
/// client runs inside an MSBuild ToolTask, MSBuild waits for those pipes to close: forever
/// before MSBuild 18.6 (the build hangs), 30 seconds after that.
module private DetachedProcess =

    [<Struct; StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type StartupInfo =
        val mutable cb: int
        val mutable lpReserved: nativeint
        val mutable lpDesktop: nativeint
        val mutable lpTitle: nativeint
        val mutable dwX: int
        val mutable dwY: int
        val mutable dwXSize: int
        val mutable dwYSize: int
        val mutable dwXCountChars: int
        val mutable dwYCountChars: int
        val mutable dwFillAttribute: int
        val mutable dwFlags: int
        val mutable wShowWindow: int16
        val mutable cbReserved2: int16
        val mutable lpReserved2: nativeint
        val mutable hStdInput: nativeint
        val mutable hStdOutput: nativeint
        val mutable hStdError: nativeint

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type ProcessInformation =
        val mutable hProcess: nativeint
        val mutable hThread: nativeint
        val mutable dwProcessId: int
        val mutable dwThreadId: int

    [<DllImport("kernel32.dll", EntryPoint = "CreateProcessW", CharSet = CharSet.Unicode, SetLastError = true)>]
    extern bool CreateProcess(
        string lpApplicationName, StringBuilder lpCommandLine,
        nativeint lpProcessAttributes, nativeint lpThreadAttributes,
        bool bInheritHandles, uint32 dwCreationFlags, nativeint lpEnvironment,
        string lpCurrentDirectory, StartupInfo& lpStartupInfo, ProcessInformation& lpProcessInformation)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool CloseHandle(nativeint hObject)

    let CREATE_NO_WINDOW = 0x08000000u

    let start (exePath: string) (args: string list) (workingDir: string) =
        let quote (s: string) = "\"" + s + "\""
        let commandLine = StringBuilder(exePath :: args |> List.map quote |> String.concat " ")
        let mutable startupInfo = StartupInfo()
        startupInfo.cb <- Marshal.SizeOf<StartupInfo>()
        let mutable processInfo = ProcessInformation()
        // a bare name (dotnet) is looked up on PATH from the command line
        let appName = if Path.IsPathRooted exePath then exePath else null
        if not (CreateProcess(appName, commandLine, 0n, 0n, false, CREATE_NO_WINDOW, 0n, workingDir, &startupInfo, &processInfo)) then
            raise (System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error()))
        CloseHandle processInfo.hThread |> ignore
        CloseHandle processInfo.hProcess |> ignore

/// The dotnet host to start the booster with when it has no runnable apphost: the host running
/// this process (the portable compiler runs as dotnet wsfsc.dll), then $(DOTNET_HOST_PATH) set by
/// MSBuild, then dotnet from PATH.
let dotnetHost () =
    let isDotnet (p: string) =
        not (isNull p) && Path.GetFileNameWithoutExtension(p).Equals("dotnet", System.StringComparison.OrdinalIgnoreCase) && File.Exists p
    let processPath = System.Environment.ProcessPath
    let hostPath = System.Environment.GetEnvironmentVariable "DOTNET_HOST_PATH"
    if isDotnet processPath then processPath
    elif isDotnet hostPath then hostPath
    else "dotnet"

/// The booster's apphost, if it can be started directly. Packages extracted on Linux and macOS
/// may not have the executable bit set, those boosters run with the dotnet host.
let tryBoosterAppHost (location: string) =
    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let path = Path.Combine(location, if isWindows then "wsfscservice.exe" else "wsfscservice")
    if not (File.Exists path) then None
    elif isWindows then Some path
    else
        try
            if File.GetUnixFileMode(path).HasFlag(UnixFileMode.UserExecute) then Some path else None
        with _ -> None

/// Starts a detached booster from the folder of this compiler.
let startBooster (location: string) (logArgs: string list) =
    let fileName, args =
        match tryBoosterAppHost location with
        | Some appHost -> appHost, logArgs
        | None -> dotnetHost (), Path.Combine(location, "wsfscservice.dll") :: logArgs
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        DetachedProcess.start fileName args location
    else
        // the booster's output goes to /dev/null, so it doesn't keep our pipes open
        let startInfo = ProcessStartInfo("/bin/sh")
        startInfo.ArgumentList.Add "-c"
        startInfo.ArgumentList.Add "nohup \"$0\" \"$@\" </dev/null >/dev/null 2>&1 &"
        startInfo.ArgumentList.Add fileName
        for a in args do startInfo.ArgumentList.Add a
        startInfo.WorkingDirectory <- location
        startInfo.CreateNoWindow <- true
        startInfo.UseShellExecute <- false
        use p = Process.Start(startInfo)
        p.WaitForExit()

let sendCompileCommand args projectDir useLogs (cLogger: ConsoleLogger) =
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
            let logArgs = if useLogs then [ Path.Combine(projectDir, "websharper.log") ] else []
            try
                startBooster location logArgs
            with e ->
                cLogger.Error (sprintf "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build. Could not start the booster: %s" e.Message)
                raise <| PipeException()
            match tryConnect 10000 with
            | Some pipe -> pipe
            | None ->
                cLogger.Error "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check websharper.log for more information."
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
                cLogger.Error "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check websharper.log for more information."
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
            cLogger.Error "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check websharper.log for more information."
            raise <| PipeException()
    with
    | _ ->
        cLogger.Error "WebSharperBooster warning WSB9002: There was an issue with WebSharper Booster. Retrying with Standalone build, check websharper.log for more information."
        raise <| PipeException()
