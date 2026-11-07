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

/// Manifest resources passed to fsc (`--resource`) whose contents are produced while fsc
/// runs: fsc reads resource files after type checking and optimization, just before writing
/// the assembly. Website, bundle and offline projects embed their WebSharper runtime metadata
/// this way, F# type checking and code generation still run in parallel with WebSharper
/// translation, only writing the assembly waits for it.
module WebSharper.Compiler.FSharp.PendingResources

open System
open System.Collections.Concurrent
open System.IO
open System.Threading.Tasks
open FSharp.Compiler.IO

let private pending = ConcurrentDictionary<string, TaskCompletionSource<byte[]>>(StringComparer.OrdinalIgnoreCase)

let private key (path: string) = try Path.GetFullPath path with _ -> path

type private PendingFileSystem() =
    inherit DefaultFileSystem()

    override _.FileExistsShim(path) =
        pending.ContainsKey(key path) || base.FileExistsShim(path)

    override _.OpenFileForReadShim(path, ?useMemoryMappedFile, ?shouldShadowCopy) =
        match pending.TryGetValue(key path) with
        | true, tcs -> new MemoryStream(tcs.Task.Result, false) :> Stream
        | _ -> base.OpenFileForReadShim(path, ?useMemoryMappedFile = useMemoryMappedFile, ?shouldShadowCopy = shouldShadowCopy)

let private install =
    lazy
        match FileSystemAutoOpens.FileSystem with
        | :? PendingFileSystem -> ()
        | :? DefaultFileSystem -> FileSystemAutoOpens.FileSystem <- PendingFileSystem()
        | _ -> failwith "Unexpected custom F# compiler file system, can't add pending resources"

/// A resource file fsc waits for: reading it blocks until Set or Release.
type PendingResource(path: string) =
    let tcs = TaskCompletionSource<byte[]>(TaskCreationOptions.RunContinuationsAsynchronously)
    do
        install.Force()
        pending.[key path] <- tcs

    /// The path to pass to fsc in `--resource`.
    member this.Path = path

    /// Provides the contents, also writing them to the file. False if already released.
    member this.Set(contents: byte[]) =
        File.WriteAllBytes(path, contents)
        tcs.TrySetResult contents

    /// Lets fsc continue with empty contents if no contents were set. True if it was
    /// released this way: the assembly needs emitting again with the contents.
    member this.Release() =
        tcs.TrySetResult [||]

    member this.IsSet = tcs.Task.IsCompleted

    interface IDisposable with
        member this.Dispose() =
            this.Release() |> ignore
            pending.TryRemove(key path) |> ignore
