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
module WebSharper.FSharp.Service.Program
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

/// The booster's pipe, derived from the location of this executable: one booster per installation.
let pipeName () =
    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeNameRaw = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPath
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        pipeNameRaw  // Windows uses simple pipe names
    else
        Path.Combine(Path.GetTempPath(), pipeNameRaw) // Linux/macOS require 

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

/// Parses the options of a compile request. A failure is printed like in a standalone compilation,
/// and the caller sends the returned exit code: the client reads exactly one per request.
let parseCompileOptions (args: string[]) (logger: LoggerBase) =
    try
        Ok (ParseOptions args logger)
    with
    | ArgumentError msg ->
        PrintGlobalError logger msg
        Error 1
    | e ->
        PrintGlobalError logger (sprintf "Global error: %A" e)
        Error 1

let startListening() =
    let nLogger = 
        let callerType = 
            System.Diagnostics.StackTrace(0, false)
                .GetFrames().[0]
                .GetMethod()
                .DeclaringType
        NLog.LogManager.GetLogger(callerType.Name)
    // one shared checker per mode, so a project opting out of the TransparentCompiler
    // still benefits from the IncrementalBuilder's caching across compilations
    let transparentChecker = lazy FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = true)
    let incrementalChecker = lazy FSharpChecker.Create(keepAssemblyContents = true, useTransparentCompiler = false)
    let checkerFactory (wsConfig: WsConfig) () =
        if wsConfig.TransparentCompiler then transparentChecker.Value else incrementalChecker.Value
    // reads of unchanged source files are replayed, see ReadCache (standalone compilations have none)
    WebSharper.Compiler.FSharp.ReadCache.Cache.Shared <- WebSharper.Compiler.FSharp.ReadCache.Cache.FromEnvironment()
    // what each compilation leaves for the agent tools, compilations are serialized by the agent below
    let lastSnapshot : WebSharper.Compiler.FSharp.AgentSnapshot.CompileSnapshot option ref = ref None
    if AgentTools.maxProjects > 0 then
        WebSharper.Compiler.FSharp.AgentSnapshot.Sink <- Some (fun s -> lastSnapshot.Value <- Some s)

    let memCache = MemoryCache.Default
    let tryGetMetadata (readOptions: WebSharper.Core.MetadataIO.ReadOptions) (r: WebSharper.Compiler.FrontEnd.Assembly) =
        match r.LoadPath with
        | Some path ->
            // library and full reads are cached separately
            let key =
                if readOptions = WebSharper.Core.MetadataIO.ReadOptions.Full then path
                else sprintf "%s|%b|%b|%b" path readOptions.InlineExpressions readOptions.OtherExpressions readOptions.Graph
            // memCache.[<non-existent key>] won't error. It's returning null, if the key is not present.
            match memCache.[key] with
            | null ->
                match WebSharper.Compiler.FrontEnd.TryReadFromAssemblyWith readOptions r with
                | None ->
                    None
                // errors are not cached: a missing sidecar can appear later without the dll changing,
                // and only existing files are watched
                | Some (Error _) as result ->
                    result
                | result ->
                    let policy = CacheItemPolicy()
                    // metadata comes from the sidecar, its module id is checked against the dll
                    let watched = [| path; WebSharper.Compiler.Sidecar.PathFor path |] |> Array.filter System.IO.File.Exists
                    let monitor = new HostFileChangeMonitor(watched)
                    policy.ChangeMonitors.Add monitor
                    memCache.Set(key, result, policy)
                    result
            | cached ->
                cached :?> Result<WebSharper.Core.Metadata.Info, string> option
        | None ->
            // in-memory assembly may have no path. nLogger.
            // nLogger.Trace "Reading assembly: %s" x makes this compilation fail. No Tracing here.
            WebSharper.Compiler.FrontEnd.TryReadFromAssemblyWith readOptions r

    // writes one message to a client, whole: compilation and F# output can log at the same time
    let writeMessage (serverPipe: NamedPipeServerStream) (newMessage: string) =
        nLogger.Trace(sprintf "Server sends: %s" newMessage)
        let options = System.Text.Json.JsonSerializerOptions()
        options.Encoder <-System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let res = System.Text.Json.JsonSerializer.Serialize(newMessage, options)
        let bytes = System.Text.Encoding.UTF8.GetBytes res
        let frame = Array.zeroCreate<byte> (4 + bytes.Length)
        System.BitConverter.GetBytes(bytes.Length).CopyTo(frame, 0) // prepend with message length
        bytes.CopyTo(frame, 4)
        lock serverPipe (fun () ->
            serverPipe.Write(frame, 0, frame.Length)
            serverPipe.Flush()
        )

    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeName = pipeName ()

    let sendFinished (serverPipe: NamedPipeServerStream) code = async { writeMessage serverPipe (sprintf "x: %i" code) }

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
                // read a message, reply sends one message to its client (a pipe, or the agent tools' build_project)
                let! (deserializedMessage: ArgsType, reply: string -> unit, token: CancellationToken) = inbox.Receive()
                let sendFinished (code: int) = async { reply (sprintf "x: %i" code) }
                // the printed output, kept for the agent tools' diagnostics
                let log = ResizeArray<bool * string>()
                let addLog isError (s: string) = lock log (fun () -> log.Add((isError, s)))
                // all compilation output goes through a logger. This in standalone mode goes to stdout and stderr
                // in service compilation it's proxied through a NamedPipeStream. prefixed with n: or e: or x: (error code)
                let logger = { new LoggerBase() with
                        override x.Out s =
                            let s = x.Indent s
                            addLog false s
                            token.ThrowIfCancellationRequested()
                            reply ("n: " + s)
                        override x.Error s =
                            let s = x.Indent s
                            addLog true s
                            token.ThrowIfCancellationRequested()
                            reply ("e: " + s)
                    }

                let processCompileMessage (projectOption: string option) (wsConfig: WsConfig) warnSettings args = async {
                    let nLogger = nLogger.WithProperty("wsdir", wsConfig.ProjectDir)
                    nLogger.Debug(sprintf "location of wsfscservice is: %s (server side)" location)
                    nLogger.Debug(sprintf "pipename is: %s (server side)" pipeName)
                    nLogger.Debug(sprintf "Compiling %s" (defaultArg projectOption wsConfig.ProjectFile))
                    // agent tools of extension packages the project references (--agentextension)
                    for path in wsConfig.AgentExtensions do
                        match AgentTools.loadExtensions path with
                        | Ok _ -> ()
                        | Error msg -> logger.Out msg
                    lastSnapshot.Value <- None
                    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                    let compilationResultForDebugOrRelease() =
                        try
                            CompileOnWorker wsConfig warnSettings logger (checkerFactory wsConfig) tryGetMetadata
                        with 
                        | ArgumentError msg -> 
                            PrintGlobalError logger (msg + " - args: " + (args |> String.concat " "))
                            1
                        | e ->
                            nLogger.Debug(e.Message)
                            PrintGlobalError logger (sprintf "Global error: %A" e)
                            1
                    let returnValue = compilationResultForDebugOrRelease()
                    match projectOption |> Option.orElse (Option.ofObj wsConfig.ProjectFile) with
                    | Some project ->
                        let logLines = lock log (fun () -> log.ToArray())
                        AgentTools.record (AgentTools.createState project lastSnapshot.Value returnValue stopwatch.Elapsed logLines)
                    | None -> ()
                    lastSnapshot.Value <- None
                    match projectOption with
                    | Some project ->
                        let projectTimestamp = { Path = project; Timestamp = File.GetLastWriteTime project }
                        let referenceTimestamps = 
                            wsConfig.References
                            |> Array.map (fun x -> { Path = x; Timestamp = File.GetLastWriteTime x })
                            |> Array.toList
                        lock argsDict (fun () -> argsDict.[project] <- { Timestamps = projectTimestamp :: referenceTimestamps; Args = args })
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
                    match lock argsDict (fun () -> argsDict.TryGetValue project) with
                    | false, _ ->
                        do! sendFinished (int ErrorCode.ProjectNotCached)
                    | true, cached ->
                        // use the same differentiation as --standalone
                        match parseCompileOptions cached.Args logger with
                        | Error exitCode ->
                            do! sendFinished exitCode
                        | Ok (HelpOrCommand r) ->
                            do! sendFinished r // unexpected, wsfsc.exe should handle this
                        | Ok (ParsedOptions (wsConfig, warnSettings)) ->
                            // https://developers.websharper.com/docs/v4.x/fs/project-variables
                            // If Project is empty but OutputDir is specified then this setting will implicitly have the value Site, which means a Client-Server Application project type.
                            match defaultArg wsConfig.ProjectType Website with
                            // TODO: this needs to be revisited with .NET 6
                            | WIG | Proxy ->
                                do! sendFinished (int ErrorCode.ProjectTypeNotPermitted)
                            | Bundle | BundleOnly | Html | Website | Service ->
                                do! processCompileMessage (project |> Some) wsConfig warnSettings cached.Args
                | FullCompile (args, projectOption) ->
                    let projectDirOption = projectOption |> Option.bind tryGetDirectoryName
                    match projectDirOption with
                    | Some project -> System.Environment.CurrentDirectory <- project
                    | None -> ()
                    // use the same differentiation like --standalone
                    match parseCompileOptions args logger with
                    | Error exitCode ->
                        do! sendFinished exitCode
                    | Ok (HelpOrCommand r) ->
                        do! sendFinished r // unexpected, wsfsc.exe should handle this
                    | Ok (ParsedOptions (wsConfig, warnSettings)) ->
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


    let isOutdated (project: string) =
        match lock argsDict (fun () -> match argsDict.TryGetValue project with | true, c -> Some c | _ -> None) with
        | Some projCache ->
            projCache.Timestamps |> List.exists (fun timestamp -> (File.Exists timestamp.Path |> not) || File.GetLastWriteTime(timestamp.Path) <> (timestamp.Timestamp))
            |> Some
        | None -> None

    // the agent tools' build_project: a compilation like `dotnet ws build`, its output is recorded by the agent
    let boosterContext : AgentTools.BoosterContext =
        {
            IsOutdated = isOutdated
            Build = fun project ->
                let finished = System.Threading.Tasks.TaskCompletionSource<int>()
                let reply (message: string) =
                    if message.StartsWith "x: " then
                        finished.TrySetResult(int (message.Substring 3)) |> ignore
                agent.Post ({ args = [| "compile:" + project |] }, reply, CancellationToken.None)
                if finished.Task.Wait(TimeSpan.FromMinutes 30.) then finished.Task.Result
                else int ErrorCode.UnexpectedFinish
        }

    // start the reading/processing loop from that NamedPipeStream
    let handOverPipe (serverPipe: NamedPipeServerStream) (token: CancellationToken) =
        async {
            try
                let reply = writeMessage serverPipe
                let handleMessage (message: obj) = 
                    async {
                        let message = System.Text.Json.JsonSerializer.Deserialize<ArgsType>(string message)
                        if message.args.Length >= 3 && message.args.[0] = "mcp" then
                            // agent tools: answered here from the recorded compilations, not queued behind compilations.
                            // After the tool: the extensions the front end loaded, so that their tools run here too
                            let loadErrors =
                                message.args.[3..] |> Array.choose (fun path ->
                                    match AgentTools.loadExtensions path with
                                    | Ok _ -> None
                                    | Error msg -> Some msg)
                            match (if loadErrors.Length > 0 then Error (String.concat "\n" loadErrors) else AgentTools.callTool boosterContext message.args.[1] message.args.[2]) with
                            | Ok text ->
                                reply ("j: " + text)
                                reply "x: 0"
                            | Error text ->
                                reply ("j: " + text)
                                reply "x: 1"
                        elif message.args.Length = 1 && message.args.[0].StartsWith "compile:" then
                            let project = message.args.[0].Substring(8) 
                            match isOutdated project with
                            | Some true ->
                                do! sendFinished serverPipe (int ErrorCode.ProjectOutdated)
                            | Some false ->
                                agent.Post (message, reply, token)
                            | None ->
                                do! sendFinished serverPipe (int ErrorCode.ProjectNotCached)
                        else
                            agent.Post (message, reply, token)
                        return None
                    }
                // collecting a full message in a ResizableBuffer. When it arrives do the "handleMessage" function on that.
                // A client that disconnected ends the loop: readingMessages retries empty reads otherwise, and each
                // finished connection (every build, every agent tool call) kept a loop waking every second.
                let disconnected () = if not serverPipe.IsConnected then raise (PipeException ())
                let! _ = readingMessages serverPipe handleMessage disconnected
                serverPipe.Dispose()
            with
            | PipeException _ ->
                serverPipe.Dispose()
            | ex ->
                nLogger.Error(ex, "Error in handleMessage loop")
                serverPipe.Dispose()
            }

    // start listening. When Client connects, spawn a message processor and start another listen
    let rec pipeListener token = async {
        try
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
        with ex ->
            nLogger.Error(ex, "Error in pipeListener loop")
        do! pipeListener token
        }

    let tokenSource = new CancellationTokenSource()
    Async.Start (pipeListener tokenSource.Token)
    locker.WaitOne() |> ignore

    tokenSource.Cancel()

/// `wsfscservice --mcp`: an MCP server over stdio for AI agents, it forwards tool calls to the running booster
/// of this installation, which answers from the compilations it recorded.
module McpClient =

    let notRunning (reason: string) =
        sprintf "No WebSharper booster is running for this installation (%s): %s. It starts with the first build of a WebSharper F# project (dotnet build or dotnet ws build) unless WebSharperStandalone is set, or with dotnet ws start."
            (Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)) reason

    /// Whether a booster listens on the pipe: without one, connecting would only wait for the timeout. On Linux and
    /// macOS the pipe is a socket file that stays when the booster is killed, so a refused connection tells.
    let pipeExists () =
        try
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then File.Exists(@"\\.\pipe\" + pipeName ())
            elif not (File.Exists(pipeName ())) then false
            else
                use probe = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.Unix, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Unspecified)
                try
                    probe.Connect(System.Net.Sockets.UnixDomainSocketEndPoint(pipeName ()))
                    true
                with :? System.Net.Sockets.SocketException -> false
        with _ -> true

    let callBooster (name: string) (arguments: string) : Result<string, string> =
        if not (pipeExists ()) then Error (notRunning "its pipe does not exist") else
        use pipe = new NamedPipeClientStream(".", pipeName (), PipeDirection.InOut, PipeOptions.Asynchronous)
        let connectError =
            try
                pipe.Connect(2000)
                if pipe.IsConnected then None else Some "could not connect"
            with
            | :? TimeoutException -> Some "no answer on its pipe"
            | e -> Some e.Message
        match connectError with
        | Some reason -> Error (notRunning reason)
        | None ->
        try
            pipe.ReadMode <- PipeTransmissionMode.Byte
            let options = System.Text.Json.JsonSerializerOptions()
            options.Encoder <- System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
            let message = Array.append [| "mcp"; name; arguments |] (AgentTools.loadedExtensionPaths () |> Array.ofList)
            let bytes = System.Text.Json.JsonSerializer.Serialize({ args = message }, options) |> System.Text.Encoding.UTF8.GetBytes
            pipe.Write(System.BitConverter.GetBytes(bytes.Length), 0, 4) // prepend with message length
            pipe.Write(bytes, 0, bytes.Length)
            pipe.Flush()
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                pipe.WaitForPipeDrain()
            let text = System.Text.StringBuilder()
            let handle (message: obj) =
                async {
                    let s = (message :?> System.Text.Json.JsonElement).GetString()
                    if s.StartsWith "j: " then
                        text.Append(s.Substring 3) |> ignore
                        return None
                    elif s.StartsWith "x: " then
                        return Some (int (s.Substring 3))
                    else
                        return None
                }
            let disconnected () = if not pipe.IsConnected then raise (PipeException ())
            match Async.RunSynchronously (readingMessages pipe handle disconnected) with
            | Some 0 -> Ok (text.ToString())
            | Some _ -> Error (text.ToString())
            | None -> Error "The WebSharper booster closed the connection without answering."
        with _ ->
            Error "The WebSharper booster closed the connection without answering, it may have stopped."

    /// With WebSharperMcpLog set to a file path, each tool call is appended to it as a JSON line: the tool, its
    /// arguments, the duration, whether it answered, and the start of the answer. For finding what agents miss.
    let logged (logPath: string) (call: string -> string -> Result<string, string>) (name: string) (arguments: string) =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let result = call name arguments
        try
            let ok, text = match result with Ok t -> true, t | Error t -> false, t
            let entry = System.Text.Json.Nodes.JsonObject()
            entry.["time"] <- System.Text.Json.Nodes.JsonValue.Create(DateTime.Now.ToString("o"))
            entry.["tool"] <- System.Text.Json.Nodes.JsonValue.Create(name)
            entry.["arguments"] <- (try System.Text.Json.Nodes.JsonNode.Parse(if String.IsNullOrWhiteSpace arguments then "{}" else arguments) with _ -> System.Text.Json.Nodes.JsonValue.Create(arguments) :> System.Text.Json.Nodes.JsonNode)
            entry.["ms"] <- System.Text.Json.Nodes.JsonValue.Create(sw.ElapsedMilliseconds)
            entry.["ok"] <- System.Text.Json.Nodes.JsonValue.Create(ok)
            entry.["chars"] <- System.Text.Json.Nodes.JsonValue.Create(text.Length)
            entry.["answer"] <- System.Text.Json.Nodes.JsonValue.Create(if text.Length > 600 then text.Substring(0, 600) + "..." else text)
            let options = System.Text.Json.JsonSerializerOptions(Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
            lock logPath (fun () -> File.AppendAllText(logPath, entry.ToJsonString(options) + "\n"))
        with _ -> ()
        result

    let run (extensionPaths: string list) =
        let utf8 = System.Text.UTF8Encoding(false)
        use input = new StreamReader(Console.OpenStandardInput(), utf8)
        use output = new StreamWriter(Console.OpenStandardOutput(), utf8)
        // extensions: --extension <path> arguments and WebSharperAgentExtensions (separated like PATH)
        let fromEnv =
            match Environment.GetEnvironmentVariable "WebSharperAgentExtensions" with
            | null -> []
            | v -> v.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        for path in extensionPaths @ fromEnv do
            match AgentTools.loadExtensions path with
            | Ok _ -> ()
            | Error msg -> eprintfn "wsfscservice --mcp: %s" msg
        let edition = AgentTools.edition ()
        // the edition decides what is offered and adds its pointers; the booster runs the tools
        let editionCall (name: string) (arguments: string) =
            if not (edition.Tools |> List.exists (fun t -> t.Name = name)) then Error (sprintf "Unknown tool: %s" name)
            else
                let result =
                    // tools with static content (guides) need no booster, also before the first build
                    if List.contains name edition.LocalTools then AgentTools.callTool { IsOutdated = (fun _ -> None); Build = (fun _ -> -1) } name arguments
                    else callBooster name arguments
                match result with
                | Ok text ->
                    let args = AgentTools.ToolArgs(if String.IsNullOrWhiteSpace arguments then null else (try System.Text.Json.Nodes.JsonNode.Parse arguments with _ -> null))
                    Ok (edition.Decorate name args text)
                | e -> e
        let call =
            match Environment.GetEnvironmentVariable "WebSharperMcpLog" with
            | null | "" -> editionCall
            | path -> logged path editionCall
        Mcp.serve AssemblyVersionInformation.WSVersion edition.Instructions (fun () -> AgentTools.toolsJsonOf edition.Tools) call input output
        0

[<assembly: System.Reflection.AssemblyTitleAttribute("WebSharper Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
[<assembly: System.Reflection.AssemblyDescriptionAttribute("WebSharper Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
do ()

[<EntryPoint>]
let main args =
    if args.Length > 0 && args.[0] = "--mcp" then
        // --mcp [--extension <path>]...
        let rec extensions (l: string list) =
            match l with
            | "--extension" :: path :: rest -> path :: extensions rest
            | _ :: rest -> extensions rest
            | [] -> []
        McpClient.run (extensions (List.ofArray args.[1..]))
    else
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