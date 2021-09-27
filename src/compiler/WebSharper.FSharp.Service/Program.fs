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
open System.Runtime.Serialization.Formatters.Binary
open FSharp.Compiler.CodeAnalysis
open System.Runtime.Caching
open WebSharper.Compiler.WsFscServiceCommon
open WebSharper.Compiler
open WebSharper.Compiler.FSharp.Compile
open WebSharper.Compiler.FSharp.ErrorPrinting
open WebSharper.Compiler.CommandTools
open System.Collections.Generic

type FileTimestamp = { Path: string; Timestamp: DateTime }
type CachedProjInfo = { Timestamps: FileTimestamp list; Args: string [] }
let argsDict = Dictionary<string, CachedProjInfo>(StringComparer.InvariantCultureIgnoreCase)

let (|Exit|FullCompile|PostCompile|) (args: ArgsType) = 
    let tryGetDirectoryName (path: string) =
        try
            let path = System.IO.Path.GetDirectoryName path
            if System.IO.Directory.Exists path then
                Some path
            else
                None
        with
        | _ -> None

    if args = {args= [|"exit"|]} then
        Exit
    elif args.args.Length = 1 && args.args.[0].StartsWith("compile:") then
        let projectOption = args.args.[0].Substring(8) 
        let projectDirOption = projectOption |> Some |> Option.bind tryGetDirectoryName
        match projectDirOption with
        | Some project -> System.Environment.CurrentDirectory <- project
        | None -> ()
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
        let projectDirOption = projectOption |> Option.bind tryGetDirectoryName
        match projectDirOption with
        | Some project -> System.Environment.CurrentDirectory <- project
        | None -> ()
        FullCompile (args.args, projectOption)
        
        
let startListening() =
    let nLogger = 
        let callerType = 
            System.Diagnostics.StackTrace(0, false)
                .GetFrames().[0]
                .GetMethod()
                .DeclaringType
        NLog.LogManager.GetLogger(callerType.Name)
    nLogger.Trace "Trace level is on"
    nLogger.Debug "Debug level is on"
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let checkerFactory() = checker

    nLogger.Debug "Initializing memory cache"
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
                nLogger.Trace(sprintf "Storing assembly: %s" x)
                memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | Some x ->
            nLogger.Trace(sprintf "Reading assembly: %s" x)
            memCache.[x] :?> Result<WebSharper.Core.Metadata.Info, string> option
        | None ->
            // in-memory assembly may have no path. nLogger. 
            // nLogger.Trace "Reading assembly: %s" x makes this compilation fail. No Tracing here.
            WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Core.Metadata.MetadataOptions.FullMetadata r

    // possible sends for pipeStream
    let send (serverPipe: NamedPipeServerStream) paramPrint str = async {
        let newMessage = paramPrint str
        nLogger.Trace(sprintf "Server sends: %s" newMessage)
        let bf = new BinaryFormatter()
        use ms = new MemoryStream()
        bf.Serialize(ms, newMessage)
        ms.Flush()
        ms.Position <- 0L
        do! ms.CopyToAsync(serverPipe) |> Async.AwaitTask
        serverPipe.Flush()
    }
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
                        override _.Out s =
                            let sendOut = sprintf "n: %s" |> send
                            let asyncValue = 
                                s
                                |> sendOut
                            Async.RunSynchronously(asyncValue, cancellationToken = token)
                        override _.Error s =
                            let sendErr = sprintf "e: %s" |> send
                            let asyncValue = 
                                s
                                |> sendErr
                            Async.RunSynchronously(asyncValue, cancellationToken = token)
                    }

                let processCompileMessage (projectOption: string option) wsConfig warnSettings args = async {
                    nLogger.Debug(sprintf "Compiling %s" projectOption.Value)
                    let compilationResultForDebugOrRelease() =
#if DEBUG
                        Compile wsConfig warnSettings logger checkerFactory tryGetMetadata
#else
                        try Compile wsConfig warnSettings logger checkerFactory tryGetMetadata
                        with 
                        | ArgumentError msg -> 
                            PrintGlobalError logger (msg + " - args: " + (args |> String.concat " "))
                            1
                        | e -> 
                            PrintGlobalError logger (sprintf "Global error: %A" e)
                            1
#endif
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
                match deserializedMessage with
                | Exit -> 
                    exiting <- true
                | PostCompile project ->
                    if argsDict.ContainsKey project |> not then
                        let projectNotInCacheErrorCode = -33212
                        do! sendFinished projectNotInCacheErrorCode
                    else
                        let mutable parsedOptions = ParseOptionsResult.HelpOrCommand 0
                        try
                            parsedOptions <- ParseOptions argsDict.[project].Args logger
                        with
                        | _ -> 
                            let unexpectedFinishErrorCode = -12211
                            do! sendFinished unexpectedFinishErrorCode
                        // use the same differentiation like --standalone
                        match parsedOptions with
                        | HelpOrCommand r ->
                            do! sendFinished r // unexpected, wsfsc.exe should handle this
                        | ParsedOptions (wsConfig, warnSettings) ->
                            // https://developers.websharper.com/docs/v4.x/fs/project-variables
                            // If Project is empty but OutputDir is specified then this setting will implicitly have the value Site, which means a Client-Server Application project type.
                            match defaultArg wsConfig.ProjectType Website with
                            | WIG | Proxy -> 
                                let permittedProjectTypeErrorCode = -21233
                                do! sendFinished permittedProjectTypeErrorCode
                            | Bundle | BundleOnly | Html | Website ->
                                do! processCompileMessage (project |> Some) wsConfig warnSettings argsDict.[project].Args
                | FullCompile (args, projectOption) ->
                    let mutable parsedOptions = ParseOptionsResult.HelpOrCommand 0
                    try
                        parsedOptions <- ParseOptions args logger
                    with
                    | _ -> 
                        let unexpectedFinishErrorCode = -12211
                        do! sendFinished unexpectedFinishErrorCode
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
                        let message = message :?> ArgsType
                        if message.args.Length = 1 && message.args.[0].StartsWith "compile:" then
                            let project = message.args.[0].Substring(8) 
                            let projCache = argsDict.[project]
                            if projCache.Timestamps |> List.exists (fun timestamp -> (File.Exists timestamp.Path |> not) || File.GetLastWriteTime(timestamp.Path) <> (timestamp.Timestamp)) then
                                let projectOutdatedErrorCode = -11234
                                do! sendFinished serverPipe projectOutdatedErrorCode
                            else
                                agent.Post (message, serverPipe, token)
                        else
                            agent.Post (message, serverPipe, token)
                        return None
                    }
                // collecting a full message in a ResizableBuffer. When it arrives do the "handleMessage" function on that.
                let! _ = readingMessages serverPipe handleMessage
                nLogger.Debug "Client has disconnected"
                serverPipe.Close()
            with
            | ex ->
                nLogger.Error(ex, "Error in handleMessage loop")
            }

    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    nLogger.Debug(sprintf "location of wsfscservice is: %s (server side)" location)
    let pipeName = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    nLogger.Debug(sprintf "pipename is: %s (server side)" location)
    // start listening. When Client connects, spawn a message processor and start another listen
    let rec pipeListener token = async {
        let serverPipe = new NamedPipeServerStream( 
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // diretcion of the pipe 
                          -1, // max number of server instances
                          PipeTransmissionMode.Byte, // Transmissione Mode
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
        do! serverPipe.WaitForConnectionAsync(token) |> Async.AwaitTask
        nLogger.Debug(sprintf "Client connected on %s pipeName" pipeName)
        Async.Start (handOverPipe serverPipe token, token)
        do! pipeListener token
        }

    let tokenSource = new CancellationTokenSource()
    nLogger.Debug(sprintf "Server listening started on %s pipeName" pipeName)
    Async.Start (pipeListener tokenSource.Token)
    locker.WaitOne() |> ignore

    tokenSource.Cancel()

[<assembly: System.Reflection.AssemblyTitleAttribute("WebSharper Booster " + AssemblyVersionInformation.AssemblyFileVersion)>]
[<assembly: System.Reflection.AssemblyDescriptionAttribute("WebSharper Booster " + AssemblyVersionInformation.AssemblyFileVersion)>]
do ()

[<EntryPoint>]
let main _ =
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
    // killing the task from Task Manager on Windows 10 will dispose the Mutex
    finally 
        try
            mutex.ReleaseMutex()
        finally
            try
                mutex.Close()
            finally
                mutex.Dispose()
    0 // exit code