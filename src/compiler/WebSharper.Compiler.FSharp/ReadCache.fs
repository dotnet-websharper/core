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

/// The booster's cache of reading F# source files into WebSharper's representation.
///
/// The F# compiler checks the files of a project in order, so the typed tree of a file only
/// depends on the files before it, the references and the options. ProjectReader reads one file
/// at a time in the same order, and what it adds to the compilation only depends on the same
/// things and on what reading the earlier files added. So the calls a file's reading made on the
/// compilation are recorded, keyed by a hash of the options, the references and the contents of
/// that file and all files before it, and replayed in a later compilation with the same key:
/// changing a file reads it and the files after it again, a WebSharper-only rebuild reads nothing.
/// A cheap structural pass (types, annotations, proxies) still runs for every file, it fills the
/// lookups that hold F# compiler objects of the current check.
module WebSharper.Compiler.FSharp.ReadCache

open System
open System.IO
open System.Collections.Generic
open WebSharper.Core.AST
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved

/// What reading one source file added to the compilation.
type FileRead =
    {
        Actions : CompilationAction[]
        FuncArgs : (ArgCurrying.Member * NotResolvedMethod * Id list * bool)[]
    }

/// Later compilation steps change the mutable fields of NotResolvedMethod, so a read is copied
/// when stored and when replayed. A method shared by a class and the function argument
/// resolver stays shared in the copy.
let copy (r: FileRead) =
    let methods = Dictionary<NotResolvedMethod, NotResolvedMethod>(HashIdentity.Reference)
    let copyMethod (m: NotResolvedMethod) =
        match methods.TryGetValue m with
        | true, c -> c
        | _ ->
            let c = { m with Kind = m.Kind }
            methods.Add(m, c)
            c
    let copyClass (cls: NotResolvedClass) =
        { cls with
            Members =
                cls.Members |> List.map (function
                    | NotResolvedMember.Method (d, m) -> NotResolvedMember.Method (d, copyMethod m)
                    | NotResolvedMember.Constructor (d, m) -> NotResolvedMember.Constructor (d, copyMethod m)
                    | m -> m
                )
        }
    {
        Actions =
            r.Actions |> Array.map (function
                | CompilationAction.AddClass (t, c) -> CompilationAction.AddClass (t, copyClass c)
                | a -> a
            )
        FuncArgs = r.FuncArgs |> Array.map (fun (mem, nr, args, i) -> mem, copyMethod nr, args, i)
    }

let private sha256 (s: string) =
    use sha = Security.Cryptography.SHA256.Create()
    sha.ComputeHash(Text.Encoding.UTF8.GetBytes s)

let private hex (b: byte[]) = BitConverter.ToString(b).Replace("-", "")

/// Reads of the files of the last compilation of recently compiled projects.
type Cache(maxProjects: int, check: bool) =
    let projects = Dictionary<string, Dictionary<string, FileRead> * int64>()
    let mutable useCounter = 0L

    /// Also reads replayed files again and compares the results, reported as warnings.
    member this.Check = check

    member this.TryGet(project: string, key: string) =
        lock projects <| fun () ->
            match projects.TryGetValue project with
            | true, (files, _) ->
                match files.TryGetValue key with
                | true, r -> Some r
                | _ -> None
            | _ -> None

    /// Keeps only the reads of the last compilation of the project.
    member this.Set(project: string, files: Dictionary<string, FileRead>) =
        lock projects <| fun () ->
            useCounter <- useCounter + 1L
            projects.[project] <- (files, useCounter)
            while projects.Count > maxProjects do
                let oldest = projects |> Seq.minBy (fun (KeyValue(_, (_, u))) -> u)
                projects.Remove oldest.Key |> ignore

    /// The cache of the booster, None in standalone compilations.
    static member val Shared : Cache option = None with get, set

    /// Creates the booster's cache, unless the WebSharperReadCache environment variable is false.
    /// WebSharperReadCacheProjects sets how many projects are kept (default 20),
    /// WebSharperReadCacheCheck=true turns on comparing replayed reads with fresh ones.
    static member FromEnvironment() =
        let env (n: string) = Environment.GetEnvironmentVariable n
        let isFalse (v: string) = not (isNull v) && v.Trim().ToLowerInvariant() = "false"
        let isTrue (v: string) = not (isNull v) && v.Trim().ToLowerInvariant() = "true"
        if isFalse (env "WebSharperReadCache") then None else
        let maxProjects =
            match Int32.TryParse(env "WebSharperReadCacheProjects") with
            | true, n when n > 0 -> n
            | _ -> 20
        Some (Cache(maxProjects, isTrue (env "WebSharperReadCacheCheck")))

/// The keys of one compilation's source files, and the reads recorded or replayed in it.
type Context(cache: Cache, project: string, projectKey: string, sourceFiles: string[]) =
    let prefixKeys = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    let mutable keysComputed = false
    let reads = Dictionary<string, FileRead>()
    let mutable hits = 0
    let mutable misses = 0

    // the key of a file: the project key and the paths and contents of it and all files before it
    let computeKeys (readerKey: string) =
        keysComputed <- true
        use sha = Security.Cryptography.SHA256.Create()
        let mutable prev = sha256 (projectKey + "|" + readerKey)
        let mutable ok = true
        for f in sourceFiles do
            if ok then
                let full = Path.GetFullPath f
                let content =
                    try Some (File.ReadAllBytes full) with _ -> None
                match content with
                | None -> ok <- false
                | Some content ->
                    let fileHash = sha.ComputeHash content
                    let path = Text.Encoding.UTF8.GetBytes full
                    prev <- sha.ComputeHash(Array.concat [ prev; path; fileHash ])
                    prefixKeys.[full] <- hex prev

    member this.Cache = cache

    /// Computes the file keys, readerKey has what the reader computed from the checked
    /// project and adds to the project key (assembly attributes).
    member this.Start(readerKey: string) =
        if not keysComputed then computeKeys readerKey

    member this.KeyOf(file: string) =
        match prefixKeys.TryGetValue(Path.GetFullPath file) with
        | true, k -> Some k
        | _ -> None

    member this.TryGet(key: string) =
        match cache.TryGet(project, key) with
        | Some r ->
            hits <- hits + 1
            reads.[key] <- r
            Some (copy r)
        | None -> None

    member this.Add(key: string, r: FileRead) =
        misses <- misses + 1
        reads.[key] <- copy r

    member this.Hits = hits
    member this.Misses = misses

    /// Stores the reads of this compilation, replacing the earlier ones of the project.
    member this.Finish() =
        cache.Set(project, Dictionary(reads))

// module version ids of references by path, time and length: a rebuild writes the same
// reference assembly again with a new time
let private moduleIds = Collections.Concurrent.ConcurrentDictionary<string, struct (int64 * int64 * string)>()

let private moduleId (path: string) =
    try
        let fi = FileInfo path
        if not fi.Exists then "-" else
        let ticks, length = fi.LastWriteTimeUtc.Ticks, fi.Length
        match moduleIds.TryGetValue path with
        | true, struct (t, l, id) when t = ticks && l = length -> id
        | _ ->
            let id = string (PEMetadata.ReadModuleId path) + "," + string length
            moduleIds.[path] <- struct (ticks, length, id)
            id
    with _ -> "?"

/// The project part of the keys: the compiler options (with the references), what the reader
/// reads from references (F# reference assemblies by module version id, WebSharper stamps or
/// sidecars) and the WebSharper settings the reader uses.
let ProjectKey (otherOptions: string[]) (wsReferences: string[]) (settings: string) =
    let sb = Text.StringBuilder()
    let add (s: string) = sb.Append(s).Append('\n') |> ignore
    let fileInfo (path: string) =
        try
            let fi = FileInfo path
            if fi.Exists then string fi.LastWriteTimeUtc.Ticks + "," + string fi.Length else "-"
        with _ -> "?"
    add "v1"
    add settings
    for o in otherOptions do
        add o
        let r =
            if o.StartsWith "-r:" then Some (o.Substring 3)
            elif o.StartsWith "--reference:" then Some (o.Substring 12)
            else None
        r |> Option.iter (fun r -> add (moduleId (r.Trim('"'))))
    for r in wsReferences do
        let stamp = Sidecar.StampPathFor r
        let sidecar = Sidecar.PathFor r
        if File.Exists stamp then
            add ("stamp:" + (try File.ReadAllText stamp with _ -> fileInfo stamp))
        elif File.Exists sidecar then
            add ("sidecar:" + fileInfo sidecar)
        else
            add "-"
    hex (sha256 (sb.ToString()))
