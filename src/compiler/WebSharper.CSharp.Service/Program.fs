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

/// The WebSharper C# booster (wscscservice): a resident process next to wscsc. Unlike the F# booster it does not
/// compile: wscsc compiles as before and reports each compilation to it (the "record" message), and it answers the
/// agent tools from those reports and the WebSharper metadata they point to. `wscscservice --mcp` is the MCP server
/// AI agents start, it forwards tool calls to the running booster of the same installation.
module WebSharper.CSharp.Service.Program

open System
open System.IO
open System.IO.Pipes
open System.Threading
open System.Runtime.InteropServices
open WebSharper.Compiler
open WebSharper.Compiler.WsFscServiceCommon

/// The booster's folder: one booster per installation, like the F# booster.
let folder () = Path.GetDirectoryName(Reflection.Assembly.GetEntryAssembly().Location)

let pipeName () = cscBoosterPipeName (folder ())

let private jsonOptions =
    let o = Text.Json.JsonSerializerOptions()
    o.Encoder <- Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    o

let private reportOptions = Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)

/// The statistics with the booster's state, for `dotnet ws stats` and their file.
let statsSnapshot () =
    Stats.snapshot AssemblyVersionInformation.WSVersion [
        "kind", Text.Json.Nodes.JsonValue.Create("csharp") :> Text.Json.Nodes.JsonNode
        "projects_kept", Text.Json.Nodes.JsonValue.Create(AgentTools.projectCount ()) :> Text.Json.Nodes.JsonNode
        "projects_max", Text.Json.Nodes.JsonValue.Create(AgentTools.maxProjects) :> Text.Json.Nodes.JsonNode
    ]

let listen (log: string -> unit) =
    let pipeName = pipeName ()
    use exited = new ManualResetEvent(false)

    let writeMessage (pipe: NamedPipeServerStream) (message: string) =
        let bytes = Text.Json.JsonSerializer.Serialize(message, jsonOptions) |> Text.Encoding.UTF8.GetBytes
        let frame = Array.zeroCreate<byte> (4 + bytes.Length)
        BitConverter.GetBytes(bytes.Length).CopyTo(frame, 0) // prepend with message length
        bytes.CopyTo(frame, 4)
        lock pipe (fun () ->
            pipe.Write(frame, 0, frame.Length)
            pipe.Flush())

    // the messages: record (from wscsc), mcp (from wscscservice --mcp), stats, stats-report, exit (from dotnet ws)
    let handle (reply: string -> unit) (args: string[]) =
        let received = Diagnostics.Stopwatch.GetTimestamp()
        match args with
        | [| "record"; json |] ->
            let ok =
                try
                    let report = Text.Json.JsonSerializer.Deserialize<AgentTools.CompileReport>(json, reportOptions)
                    // agent tools of extension packages the project references (--agentextension)
                    AgentTools.loadReportedExtensions report
                    AgentTools.record report
                    true
                with e ->
                    log (sprintf "Could not record a compilation: %s" e.Message)
                    false
            Stats.count (if ok then "msg.record" else "msg.record.failed")
            Stats.ms "msg.record_ms" (Diagnostics.Stopwatch.GetElapsedTime received)
            reply (if ok then "x: 0" else "x: 1")
        | _ when args.Length >= 3 && args.[0] = "mcp" ->
            // after the tool: the extensions the front end loaded, so that their tools run here too
            let loadErrors =
                args.[3..] |> Array.choose (fun path ->
                    match AgentTools.loadExtensions path with
                    | Ok _ -> None
                    | Error msg -> Some msg)
            let result = if loadErrors.Length > 0 then Error (String.concat "\n" loadErrors) else AgentTools.callTool args.[1] args.[2]
            let elapsed = Diagnostics.Stopwatch.GetElapsedTime received
            Stats.ms "msg.mcp_ms" elapsed
            let text, code = match result with Ok t -> t, 0 | Error t -> t, 1
            Stats.value "msg.mcp.reply_chars" (float text.Length)
            reply ("j: " + text)
            // the booster's time, for the front end's statistics
            reply ("t: " + elapsed.TotalMilliseconds.ToString("0.###", Globalization.CultureInfo.InvariantCulture))
            reply (sprintf "x: %i" code)
        | [| "stats" |] ->
            Stats.count "msg.stats"
            reply ("j: " + (statsSnapshot ()).ToJsonString())
            reply "x: 0"
        | [| "stats-report"; events |] ->
            let ok =
                try
                    match Text.Json.Nodes.JsonNode.Parse events with
                    | :? Text.Json.Nodes.JsonArray as e -> Stats.apply e; true
                    | _ -> false
                with _ -> false
            reply (if ok then "x: 0" else "x: 1")
        | [| "exit" |] ->
            Stats.count "msg.exit"
            reply "x: 0"
            exited.Set() |> ignore
        | _ ->
            // a compilation request: this booster does not compile, the client compiles standalone
            Stats.count "msg.unknown"
            reply "x: -21233"

    let serve (pipe: NamedPipeServerStream) =
        async {
            try
                let handleMessage (message: obj) =
                    async {
                        let args = Text.Json.JsonSerializer.Deserialize<ArgsType>(string message).args
                        handle (writeMessage pipe) args
                        return None
                    }
                let disconnected () = if not pipe.IsConnected then raise (PipeException ())
                let! _ = readingMessages pipe handleMessage disconnected
                ()
            with
            | PipeException _ -> ()
            | e -> log (sprintf "Error handling a message: %O" e)
            pipe.Dispose()
        }

    let rec listener (token: CancellationToken) =
        async {
            try
                let pipe =
                    new NamedPipeServerStream(pipeName, PipeDirection.InOut, NamedPipeServerStream.MaxAllowedServerInstances,
                                              PipeTransmissionMode.Byte, PipeOptions.WriteThrough ||| PipeOptions.Asynchronous)
                if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
                    File.SetUnixFileMode(pipeName, File.GetUnixFileMode pipeName ||| UnixFileMode.OtherWrite ||| UnixFileMode.OtherRead)
                do! pipe.WaitForConnectionAsync(token) |> Async.AwaitTask
                pipe.ReadMode <- PipeTransmissionMode.Byte
                Async.Start(serve pipe, token)
            with
            | :? OperationCanceledException -> ()
            | e -> log (sprintf "Error in the pipe listener: %O" e)
            if not token.IsCancellationRequested then
                return! listener token
        }

    use tokens = new CancellationTokenSource()
    Async.Start(listener tokens.Token)
    let location = folder ()
    use statsTimer = new Timer((fun _ -> Stats.save location statsSnapshot), null, 60000, 60000)
    exited.WaitOne() |> ignore
    tokens.Cancel()
    Stats.save location statsSnapshot

/// Writes the BoosterRecord of this booster for dotnet ws. Best effort: the booster works without it.
let registerBooster (folder: string) (log: string -> unit) =
    try
        use proc = Diagnostics.Process.GetCurrentProcess()
        let record =
            {
                Pid = proc.Id
                Version = AssemblyVersionInformation.WSVersion
                Folder = folder
                Started = proc.StartTime.ToUniversalTime()
                Pipe = cscBoosterPipeName folder
            }
        let path = cscBoosterRecordPath folder
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        let tmp = sprintf "%s.%i.tmp" path proc.Id
        File.WriteAllText(tmp, Text.Json.JsonSerializer.Serialize record)
        File.Move(tmp, path, true)
    with e ->
        log (sprintf "Could not register the C# booster for dotnet ws: %s" e.Message)

let unregisterBooster (folder: string) (log: string -> unit) =
    try
        let path = cscBoosterRecordPath folder
        if File.Exists path then
            let record = Text.Json.JsonSerializer.Deserialize<BoosterRecord>(File.ReadAllText path)
            if isNull (box record) || record.Pid = Environment.ProcessId then
                File.Delete path
    with e ->
        log (sprintf "Could not unregister the C# booster for dotnet ws: %s" e.Message)

/// `wscscservice --mcp`: an MCP server over stdio for AI agents, it forwards tool calls to the running C# booster
/// of this installation.
module McpClient =

    let notRunning (reason: string) =
        sprintf "No WebSharper C# booster is running for this installation (%s): %s. It starts with the first build of a WebSharper C# project (dotnet build) unless WebSharperStandalone is set."
            (folder ()) reason

    /// Calls a tool in the booster, with the paths of this process's extensions so that the booster loads them too:
    /// its answer, and the booster's time in ms.
    let callBooster (name: string) (arguments: string) : Result<string, string> * float option =
        let message = Array.append [| "mcp"; name; arguments |] (AgentTools.loadedExtensionPaths () |> Array.ofList)
        match BoosterPipe.send (pipeName ()) message 2000 with
        | Ok (0, text, ms) -> Ok text, ms
        | Ok (_, text, ms) -> Error text, ms
        | Error (BoosterPipe.NotRunning reason) -> Error (notRunning reason), None
        | Error (BoosterPipe.NoAnswer msg) -> Error msg, None

    let forwarding = Stats.Forwarding()

    /// Sends the recorded statistics to the booster, kept for later when it does not answer.
    let sendStats () =
        let events = forwarding.Take()
        if events.Count > 0 then
            match BoosterPipe.send (pipeName ()) [| "stats-report"; events.ToJsonString() |] 500 with
            | Ok (0, _, _) -> ()
            | _ -> forwarding.Return events

    let run (extensionPaths: string list) =
        let utf8 = Text.UTF8Encoding(false)
        use input = new StreamReader(Console.OpenStandardInput(), utf8)
        use output = new StreamWriter(Console.OpenStandardOutput(), utf8)
        Stats.sink <- forwarding
        // extensions log to stderr: stdout belongs to the protocol
        AgentTools.hostLog <- fun msg -> eprintfn "wscscservice --mcp: %s" msg
        AgentTools.hostVersion <- AssemblyVersionInformation.WSVersion
        AgentTools.hostIsBooster <- false
        // extensions: --extension <path> arguments and WebSharperAgentExtensions (separated like PATH)
        let fromEnv =
            match Environment.GetEnvironmentVariable "WebSharperAgentExtensions" with
            | null -> []
            | v -> v.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        for path in extensionPaths @ fromEnv do
            match AgentTools.loadExtensions path with
            | Ok _ -> ()
            | Error msg -> eprintfn "wscscservice --mcp: %s" msg
        let edition = AgentTools.edition ()
        let sessionClock = Diagnostics.Stopwatch.StartNew()
        let mutable calls = 0
        let call (name: string) (arguments: string) =
            calls <- calls + 1
            // only names of tools offered go into the statistics, not whatever an agent asked for
            let known = edition.Tools |> List.exists (fun t -> t.Name = name)
            let key = sprintf "mcp.call.%s.%s" (if known then Stats.part (AgentTools.toolSource name) else "none") (if known then Stats.part name else "unknown")
            let sw = Diagnostics.Stopwatch.StartNew()
            let result =
                if not known then Error (sprintf "Unknown tool: %s" name)
                else
                    let result =
                        // tools with static content need no booster, also before the first build
                        if List.contains name edition.LocalTools then
                            Stats.count (key + ".local")
                            AgentTools.callTool name arguments
                        else
                            let result, boosterMs = callBooster name arguments
                            boosterMs |> Option.iter (fun ms -> Stats.value (key + ".overhead_ms") (sw.Elapsed.TotalMilliseconds - ms))
                            result
                    match result with
                    | Ok text ->
                        let args = AgentTools.ToolArgs(if String.IsNullOrWhiteSpace arguments then null else (try Text.Json.Nodes.JsonNode.Parse arguments with _ -> null))
                        Ok (edition.Decorate name args text)
                    | e -> e
            Stats.ms (key + "_ms") sw.Elapsed
            Stats.count (key + (match result with Ok _ -> ".ok" | Error _ -> ".error"))
            result
        let handled (line: string) (response: Text.Json.Nodes.JsonNode option) (elapsed: TimeSpan) =
            try
                let msg = Text.Json.Nodes.JsonNode.Parse line
                let methodName = try msg.["method"].GetValue<string>() with _ -> null
                if not (isNull methodName) then
                    Stats.ms (sprintf "mcp.request.%s_ms" (Stats.part methodName)) elapsed
                    if methodName = "initialize" then
                        Stats.count "mcp.sessions"
                        Stats.count ("mcp.client." + Stats.part (try msg.["params"].["clientInfo"].["name"].GetValue<string>() with _ -> null))
                if response.IsSome then sendStats ()
            with _ -> ()
        Mcp.serve AssemblyVersionInformation.WSVersion edition.Instructions (fun () -> AgentTools.toolsJsonOf edition.Tools) call handled input output
        Stats.value "mcp.session.duration_s" sessionClock.Elapsed.TotalSeconds
        Stats.value "mcp.session.calls" (float calls)
        sendStats ()
        0

[<assembly: Reflection.AssemblyTitleAttribute("WebSharper C# Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
[<assembly: Reflection.AssemblyDescriptionAttribute("WebSharper C# Booster (" + AssemblyVersionInformation.WSVersion + ")")>]
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
    // wscscservice [<log file>]: errors go to the log file when given
    let log (msg: string) =
        if args.Length > 0 then try File.AppendAllLines(args.[0], [ msg ]) with _ -> ()
    AgentTools.hostLog <- log
    AgentTools.hostVersion <- AssemblyVersionInformation.WSVersion
    AgentTools.hostIsBooster <- true
    let location = folder ()
    // one booster per folder
    let mutexName = "Global\\" + hashPath (Path.Combine(location, "WsCscServiceMutex"))
    let mutable existing = null
    if Mutex.TryOpenExisting(mutexName, &existing) then 1 else
    use mutex = new Mutex(false, mutexName)
    if not (mutex.WaitOne 0) then 1 else
    try
        registerBooster location log
        try
            listen log
        finally
            // before releasing the mutex, so a new booster of this folder writes its record after this
            unregisterBooster location log
            mutex.ReleaseMutex()
        0
    with e ->
        log (string e)
        1
