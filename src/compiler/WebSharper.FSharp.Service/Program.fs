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

open System
open System.IO.Pipes
open System.Threading
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open FSharp.Compiler.SourceCodeServices
open System.Runtime.Caching
open WebSharper.Compiler.WsFscServiceCommon
open WebSharper.Compiler
open NLog.FSharp

[<EntryPoint>]
let main _ =
    let nLogger = Logger()
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let checkerFactory() = checker
    let dllFiles =
        System.Reflection.Assembly.GetEntryAssembly().Location
        |> System.IO.Path.GetDirectoryName
        |> Directory.EnumerateFiles
        |> (fun x -> new System.Collections.Generic.List<string>(x))

    let memCache = MemoryCache.Default
    let tryGetMetadata (r: WebSharper.Compiler.FrontEnd.Assembly) =
        if memCache.[r.FullName] = null then
            match WebSharper.Compiler.FrontEnd.TryReadFromAssembly WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata r with
            | None ->
                None
            | result ->
                let policy = CacheItemPolicy()
                let monitor = new HostFileChangeMonitor(dllFiles)
                policy.ChangeMonitors.Add monitor
                memCache.Set(r.FullName, result, policy)
                memCache.[r.FullName] :?> Result<WebSharper.Core.Metadata.Info, string> option
        else
            memCache.[r.FullName] :?> Result<WebSharper.Core.Metadata.Info, string> option

    let agent = MailboxProcessor.Start(fun inbox ->

        // the message processing function
        let rec messageLoop () = async {

            try
                // read a message
                let! (deserializedMessage: ArgsType, serverPipe: NamedPipeServerStream, token) = inbox.Receive()
                let tryGetDirectoryName (path: string) =
                    try
                       System.IO.Path.GetDirectoryName path |> Some
                    with
                    | :? System.DivideByZeroException -> None

                let switchTail (array: string array) =
                    match array with
                    | [||] -> None
                    | x -> System.String.Join(":", x) |> Some

                let projectOption = 
                    deserializedMessage.args
                    |> Array.tryFind (fun x -> x.IndexOf("--project", System.StringComparison.OrdinalIgnoreCase) >= 0)
                    |> Option.bind (fun x -> x.Split(':') |> Array.tail |> switchTail)
                let projectDirOption = projectOption |> Option.bind tryGetDirectoryName
                match projectDirOption with
                | Some project -> 
                    System.Environment.CurrentDirectory <- project
                    nLogger.Info "Compiling %s" projectOption.Value
                    let send paramPrint str = async {
                        let newMessage: string = paramPrint str
                        nLogger.Info "Server sends: %s" newMessage
                        let bytes = System.Text.Encoding.UTF8.GetBytes(newMessage)
                        do! serverPipe.WriteAsync(bytes, 0, bytes.Length, token) |> Async.AwaitTask
                        serverPipe.Flush()
                    }
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
                            
                    let sendFinished = sprintf "x: %i" |> send
                    let returnValue = WebSharper.Compiler.FSharp.Compile.compileMain deserializedMessage.args checkerFactory tryGetMetadata logger
                    do! sendFinished returnValue
                    serverPipe.WaitForPipeDrain()
                    serverPipe.Close()
                | None ->
                    ()
            with 
            | ex -> 
                nLogger.ErrorException ex "Error in MailBoxProcessor loop"
            // loop to top
            return! messageLoop ()
            }

        // start the loop
        messageLoop ()
        )


    let handOverPipe (serverPipe: NamedPipeServerStream) (token: CancellationToken) =
        async {
            try
                let handleMessage (message: byte array) = async {
                    let ms = new MemoryStream(message)
                    ms.Position <- 0L
                    let bf = new BinaryFormatter()
                    let deserializedMessage: ArgsType = bf.Deserialize(ms) :?> ArgsType
                    agent.Post (deserializedMessage, serverPipe, token)
                    }
                do! readingMessages serverPipe handleMessage
            with
            | ex ->
                nLogger.ErrorException ex "Error in handleMessage loop"
            }

    let location = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location)
    let pipeName = (location, "WsFscServicePipe") |> System.IO.Path.Combine |> hashPipeName
    let rec pipeListener token = async {
        let serverPipe = new NamedPipeServerStream( 
                          pipeName, // name of the pipe,
                          PipeDirection.InOut, // diretcion of the pipe 
                          -1, // max number of server instances
                          PipeTransmissionMode.Message, // Transmissione Mode
                          PipeOptions.WriteThrough // the operation will not return the control until the write is completed
                          ||| PipeOptions.Asynchronous)
        do! serverPipe.WaitForConnectionAsync(token) |> Async.AwaitTask
        nLogger.Debug "Client connected on %s pipeName" pipeName
        Async.Start (handOverPipe serverPipe token, token)
        do! pipeListener token
        }

    let tokenSource = new CancellationTokenSource()
    nLogger.Debug "Server listening started on %s pipeName" pipeName
    Async.Start (pipeListener tokenSource.Token)
    Console.ReadLine() |> ignore
    tokenSource.Cancel()
    0 // exit code