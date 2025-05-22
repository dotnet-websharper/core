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
#nowarn "44"

open System
open System.IO.Pipes
open System.Threading
open System.IO
open System.Runtime.InteropServices
open FSharp.Compiler.CodeAnalysis
open System.Runtime.Caching
open WebSharper.Compiler.WsFscServiceCommon
open WebSharper.Compiler
open WebSharper.Compiler.FSharp.Compile
open WebSharper.Compiler.FSharp.ErrorPrinting
open WebSharper.Compiler.CommandTools
open System.Collections.Generic

type ErrorCode =
    | ProjectNotCached          = -33212
    | ProjectOutdated           = -11234
    | UnexpectedFinish          = -12211
    | ProjectTypeNotPermitted   = -21233

type FileTimestamp = { Path: string; Timestamp: DateTime }
type CachedProjInfo = { Timestamps: FileTimestamp list; Args: string [] }
let argsDict = Dictionary<string, CachedProjInfo>(StringComparer.InvariantCultureIgnoreCase)

let (|Exit|FullCompile|PostCompile|) (args: ArgsType) = 
    if args = {args= [|"exit"|]} then
        Exit
    elif args.args.Length = 1 && args.args.[0].StartsWith("compile:") then
        let projectOption = args.args.[0].Substring(8) 
        PostCompile (projectOption)
    else
        let joinTailIfSome (array: string array) =
            match array with
            | [||] -> None
            | x -> System.String.Join(":", x) |> Some

        // read the project file
        let projectOption = 
            args.args
            |> Array.tryFind (fun x -> x.IndexOf("--project", System.StringComparison.OrdinalIgnoreCase) >= 0)
            |> Option.bind (fun x -> x.Split(':') |> Array.tail |> joinTailIfSome)
        FullCompile (args.args, projectOption)
        
        
let startListening() =
    let nLogger = 
        let callerType = 
            System.Diagnostics.StackTrace(0, false)
                .GetFrames().[0]
                .GetMethod()
                .DeclaringType
        NLog.LogManager.GetLogger(callerType.Name)
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let checkerFactory() = checker

    let memCache = MemoryCache.Default
    let tryGetMetadata (r: WebSharper.Compiler.FrontEnd.Assembly) =
        match r.LoadPath with
        // memCache.[<non-existent key>] won't error. It's returning null, if the key is not present.
        | Some x when memCache.[x] = null -> 
            match WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Core.Metadata.MetadataOptions.FullMetadata r with
            | None ->
                None
            | result ->
                let policy = CacheItemPolicy()
                let monitor = new HostFileChangeMonitor([| x |])
                policy.ChangeMonitors.Add monitor
                memCache.Set(x, result, policy)
                memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | Some x ->
            memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | None ->
            // in-memory assembly may have no path. nLogger. 
            // nLogger.Trace "Reading assembly: %s" x makes this compilation fail. No Tracing here.
            WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Core.Metadata.MetadataOptions.FullMetadata r

    // possible sends for pipeStream
    let send (serverPipe: NamedPipeServerStream) paramPrint str = async {
        let newMessage = paramPrint str
        nLogger.Trace(sprintf "Server sends: %s" newMessage)
        let options = System.Text.Json.JsonSerializerOptions()
        options.Encoder <-System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let res = System.Text.Json.JsonSerializer.Serialize(newMessage, options)
        let bytes = System.Text.Encoding.UTF8.GetBytes res
        serverPipe.Write(System.BitConverter.GetBytes(bytes.Length), 0, 4) // prepend with message length
        serverPipe.Write(bytes, 0, bytes.Length)
        serverPipe.Flush()
    }

    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeNameRaw = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    let pipeName =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            pipeNameRaw  // Windows uses simple pipe names
        else
            Path.Combine(Path.GetTempPath(), pipeNameRaw) // Linux/macOS require 

    let sendFinished (serverPipe: NamedPipeServerStream) = sprintf "x: %i" |> send serverPipe

    // client starts the service without window. You have to shut down the service from Task Manager/ kill command.
    // or use dotnet-ws tool, and send a [|"exit"|] message.
    use locker = new AutoResetEvent(false)
    let mutable exiting = false
    let agent = MailboxProcessor.Start(fun inbox ->

        // the message processing function
        // compilations are serialed by a MailboxProcessor
        let rec messageLoop () = async {
            // a compilation failing because a client disconnects can still process the next compilation
            try
                // read a message
                let! (deserializedMessage: ArgsType, serverPipe: NamedPipeServerStream, token) = inbox.Receive()
                let send = send serverPipe
                let sendFinished = sendFinished serverPipe
                // all compilation output goes through a logger. This in standalone mode goes to stdout and stderr
                // in service compilation it's proxied through a NamedPipeStream. prefixed with n: or e: or x: (error code)
                let logger = { new LoggerBase() with
                        override x.Out s =
                            let sendOut = sprintf "n: %s" |> send
                            let asyncValue = 
                                x.Indent s
                                |> sendOut
                            Async.RunSynchronously(asyncValue, cancellationToken = token)
                        override x.Error s =
                            let sendErr = sprintf "e: %s" |> send
                            let asyncValue = 
                                x.Indent s
                                |> sendErr
                            Async.RunSynchronously(asyncValue, cancellationToken = token)
                    }

                let processCompileMessage (projectOption: string option) (wsConfig: WsConfig) warnSettings args = async {
                    let nLogger = nLogger.WithProperty("wsdir", wsConfig.ProjectDir)
                    nLogger.Debug(sprintf "location of wsfscservice is: %s (server side)" location)
                    nLogger.Debug(sprintf "pipename is: %s (server side)" pipeName)
                    nLogger.Debug(sprintf "Compiling %s" projectOption.Value)
                    let compilationResultForDebugOrRelease() =
                        try
                            Compile wsConfig warnSettings logger checkerFactory tryGetMetadata
                        with 
                        | ArgumentError msg -> 
                            PrintGlobalError logger (msg + " - args: " + (args |> String.concat " "))
                            1
                        | e ->
                            nLogger.Debug(e.Message)
                            PrintGlobalError logger (sprintf "Global error: %A" e)
                            1
                    let returnValue = compilationResultForDebugOrRelease()
                    match projectOption with
                    | Some project ->
                        let projectTimestamp = { Path = project; Timestamp = File.GetLastWriteTime project }
                        let referenceTimestamps = 
                            wsConfig.References
                            |> Array.map (fun x -> { Path = x; Timestamp = File.GetLastWriteTime x })
                            |> Array.toList
                        argsDict.[project] <- { Timestamps = projectTimestamp :: referenceTimestamps; Args = args }
                    | None -> ()
                    do! sendFinished returnValue
                    }
                let tryGetDirectoryName (path: string) =
                    try
                        let path = System.IO.Path.GetDirectoryName path
                        if System.IO.Directory.Exists path then
                            Some path
                        else
                            None
                    with
                    | _ -> None
                match deserializedMessage with
                | Exit -> 
                    exiting <- true
                | PostCompile project ->
                    let projectDirOption = tryGetDirectoryName project 
                    match projectDirOption with
                    | Some project -> System.Environment.CurrentDirectory <- project
                    | None -> ()
                    let mutable parsedOptions = ParseOptionsResult.HelpOrCommand 0
                    try
                        parsedOptions <- ParseOptions argsDict.[project].Args logger
                    with
                    | _ ->
                        do! sendFinished (int ErrorCode.UnexpectedFinish)
                    // use the same differentiation as --standalone
                    match parsedOptions with
                    | HelpOrCommand r ->
                        do! sendFinished r // unexpected, wsfsc.exe should handle this
                    | ParsedOptions (wsConfig, warnSettings) ->
                        // https://developers.websharper.com/docs/v4.x/fs/project-variables
                        // If Project is empty but OutputDir is specified then this setting will implicitly have the value Site, which means a Client-Server Application project type.
                        match defaultArg wsConfig.ProjectType Website with
                        // TODO: this needs to be revisited with .NET 6
                        | WIG | Proxy ->
                            do! sendFinished (int ErrorCode.ProjectTypeNotPermitted)
                        | Bundle | BundleOnly | Html | Website | Service ->
                            do! processCompileMessage (project |> Some) wsConfig warnSettings argsDict.[project].Args
                | FullCompile (args, projectOption) ->
                    let projectDirOption = projectOption |> Option.bind tryGetDirectoryName
                    match projectDirOption with
                    | Some project -> System.Environment.CurrentDirectory <- project
                    | None -> ()
                    let mutable parsedOptions = ParseOptionsResult.HelpOrCommand 0
                    try
                        parsedOptions <- ParseOptions args logger
                    with
                    | _ ->
                        do! sendFinished (int ErrorCode.UnexpectedFinish)
                    // use the same differentiation like --standalone
                    match parsedOptions with
                    | HelpOrCommand r ->
                        do! sendFinished r // unexpected, wsfsc.exe should handle this
                    | ParsedOptions (wsConfig, warnSettings) ->
                        do! processCompileMessage projectOption wsConfig warnSettings args
            with 
            | ex -> 
                nLogger.Error(ex, "Error in MailBoxProcessor loop")
            if exiting && inbox.CurrentQueueLength = 0 then
                locker.Set() |> ignore
            else 
                // loop to top
                return! messageLoop ()
            }

        // start the loop
        messageLoop ()
        )


    // start the reading/processing loop from that NamedPipeStream
    let handOverPipe (serverPipe: NamedPipeServerStream) (token: CancellationToken) =
        async {
            try
                let handleMessage (message: obj) = 
                    async {
                        let message = System.Text.Json.JsonSerializer.Deserialize<ArgsType>(string message)
                        if message.args.Length = 1 && message.args.[0].StartsWith "compile:" then
                            let project = message.args.[0].Substring(8) 
                            match argsDict.TryGetValue project with
                            | true, projCache ->
                                if projCache.Timestamps |> List.exists (fun timestamp -> (File.Exists timestamp.Path |> not) || File.GetLastWriteTime(timestamp.Path) <> (timestamp.Timestamp)) then
                                    do! sendFinished serverPipe (int ErrorCode.ProjectOutdated)
                                else
                                    agent.Post (message, serverPipe, token)
                            | false, _ ->
                                do! sendFinished serverPipe (int ErrorCode.ProjectNotCached)
                        else
                            agent.Post (message, serverPipe, token)
                        return None
                    }
                // collecting a full message in a ResizableBuffer. When it arrives do the "handleMessage" function on that.
                let! _ = readingMessages serverPipe handleMessage ignore
                serverPipe.Close()
            with
            | ex ->
                nLogger.Error(ex, "Error in handleMessage loop")
            }

    // start listening. When Client connects, spawn a message processor and start another listen
    let rec pipeListener token = async {
        let serverPipe = new NamedPipeServerStream( 
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // diretcion of the pipe 
                          NamedPipeServerStream.MaxAllowedServerInstances, // max number of server instances
                          PipeTransmissionMode.Byte, // using Byte for Linux support
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)

        if not <| RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            let currentPermissions = System.IO.File.GetUnixFileMode pipeName
            System.IO.File.SetUnixFileMode(
                pipeName, currentPermissions ||| System.IO.UnixFileMode.OtherWrite ||| System.IO.UnixFileMode.OtherRead)
        
        do! serverPipe.WaitForConnectionAsync(token) |> Async.AwaitTask
        serverPipe.ReadMode <- PipeTransmissionMode.Byte
        Async.Start (handOverPipe serverPipe token, token)
        do! pipeListener token
        }

    let tokenSource = new CancellationTokenSource()
    Async.Start (pipeListener tokenSource.Token)
    locker.WaitOne() |> ignore

    tokenSource.Cancel()

[<assembly: System.Reflection.AssemblyTitleAttribute("WebSharper Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
[<assembly: System.Reflection.AssemblyDescriptionAttribute("WebSharper Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
do ()

[<EntryPoint>]
let main args =
    let initialLogLocation = if args.Length > 1 then Some (args[0]) else None
    try
        // One service should serve all compilations in a folder. Protect it with a global Mutex.
        let location = System.Reflection.Assembly.GetEntryAssembly().Location
        let mutexName =
            (location, "WsFscServiceMutex")
            |> System.IO.Path.Combine
            |> hashPath
            |> fun x -> "Global\\" + x
        let mutable mutex = null
        let mutexExists = Mutex.TryOpenExisting(mutexName, &mutex)
        if mutexExists then
            exit(1)
        mutex <- new Mutex(false, mutexName)
        mutex.WaitOne() |> ignore // should always instantly continue
        System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
        try
            startListening()
            try
                mutex.ReleaseMutex()
            finally
                try
                    mutex.Close()
                finally
                    mutex.Dispose()
        // killing the task from Task Manager on Windows 10 will dispose the Mutex
        with
        | ex ->
            initialLogLocation |> Option.iter (fun loc -> System.IO.File.AppendAllLines(loc, [ex.ToString()]))
            try
                mutex.ReleaseMutex()
            finally
                try
                    mutex.Close()
                finally
                    mutex.Dispose()
    with
        | ex ->
            initialLogLocation |> Option.iter (fun loc -> System.IO.File.AppendAllLines(loc, [ex.ToString()]))
    0 // exit code