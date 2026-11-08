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

namespace WebSharper.Compiler

open System
open System.IO
open System.IO.Compression
open System.Text

/// The WebSharper output of an assembly, in a `<AssemblyName>.websharper` file next to it:
/// its metadata, generated JavaScript/TypeScript code, source maps and sources.
/// The compiler reads WebSharper information of references only from sidecars.
///
/// Layout: "WSSC", format version, the module version id (MVID) of the assembly,
/// entry count, then for each entry its name, compression, uncompressed and stored
/// length, followed by the entry contents.
[<Sealed>]
type Sidecar private (path: string, moduleId: Guid, data: byte[], entries: (string * byte * int * int * int)[]) =

    static let magic = [| byte 'W'; byte 'S'; byte 'S'; byte 'C' |]

    static let fail (path: string) msg =
        raise (exn (sprintf "Invalid WebSharper sidecar file %s: %s" path msg))

    let index =
        let d = Collections.Generic.Dictionary<string, int>(entries.Length)
        entries |> Array.iteri (fun i (n, _, _, _, _) -> d.[n] <- i)
        d

    let contents (i: int) =
        let _, c, raw, offset, stored = entries.[i]
        match c with
        | 0uy ->
            if offset = 0 && stored = data.Length then data else
            let a = Array.zeroCreate stored
            Buffer.BlockCopy(data, offset, a, 0, stored)
            a
        | 1uy ->
            let res = Array.zeroCreate raw
            use d = new DeflateStream(new MemoryStream(data, offset, stored), CompressionMode.Decompress)
            let mutable read = 0
            while read < raw do
                let n = d.Read(res, read, raw - read)
                if n = 0 then fail path "unexpected end of compressed entry"
                read <- read + n
            res
        | _ -> fail path (sprintf "unknown compression %d" (int c))

    /// The name of the WebSharper metadata entry.
    static member MetadataEntry : string = WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA

    /// The sidecar path for an assembly path: same folder and base name, `.websharper` extension.
    static member PathFor(assemblyPath: string) =
        Path.ChangeExtension(assemblyPath, ".websharper")

    /// The stamp file path for an assembly path: `<AssemblyName>.websharper.stamp`, a hash of what
    /// compiling a library against it uses, rewritten only when that changes.
    static member StampPathFor(assemblyPath: string) =
        Path.ChangeExtension(assemblyPath, ".websharper.stamp")

    /// The implementation assembly for a reference assembly path (in a `ref` or `refint`
    /// folder next to it, as the .NET SDK writes them), otherwise the path itself.
    static member ImplementationPath(assemblyPath: string) =
        let dir = Path.GetDirectoryName assemblyPath
        match Path.GetFileName dir with
        | "ref" | "refint" ->
            let impl = Path.Combine(Path.GetDirectoryName dir, Path.GetFileName assemblyPath)
            if File.Exists impl then impl else assemblyPath
        | _ -> assemblyPath

    /// The path of this sidecar.
    member this.Path = path

    /// The module version id of the assembly this sidecar was written for.
    member this.ModuleId = moduleId

    /// The names of the entries.
    member this.Names = entries |> Array.map (fun (n, _, _, _, _) -> n)

    /// The contents of an entry.
    member this.TryGet(name: string) =
        match index.TryGetValue name with
        | true, i -> Some (contents i)
        | _ -> None

    /// The uncompressed sizes of the entries.
    member this.Sizes = entries |> Array.map (fun (n, _, raw, _, _) -> n, raw)

    /// The name of the entry listing web resources (extra.files and web worker bundles),
    /// replacing WebResource attributes and embedded resources of the assembly.
    static member WebResourcesEntry = "WebSharper.webresources"

    /// Encodes the web resource list: a line with name and content type for each.
    static member EncodeWebResources(resources: seq<string * string>) =
        resources
        |> Seq.map (fun (name, contentType) -> name + "\t" + contentType + "\n")
        |> String.concat ""
        |> Encoding.UTF8.GetBytes

    /// Names and content types of the web resources, each stored as an entry by its name.
    member this.WebResources =
        match this.TryGet Sidecar.WebResourcesEntry with
        | None -> [||]
        | Some bytes ->
            Encoding.UTF8.GetString(bytes).Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun l ->
                match l.IndexOf '\t' with
                | -1 -> l, ""
                | i -> l.Substring(0, i), l.Substring(i + 1)
            )

    /// Reads a sidecar file. Only the header is decoded, entries are decompressed on access.
    static member Read(path: string) =
        let data = File.ReadAllBytes path
        let fail = fail path
        let int32At i =
            if i + 4 > data.Length then fail "truncated"
            int data.[i] ||| (int data.[i + 1] <<< 8) ||| (int data.[i + 2] <<< 16) ||| (int data.[i + 3] <<< 24)
        if data.Length < 26 || data.[0] <> magic.[0] || data.[1] <> magic.[1] || data.[2] <> magic.[2] || data.[3] <> magic.[3] then
            fail "not a WebSharper sidecar"
        let vlen = int data.[4]
        let ver = Encoding.UTF8.GetString(data, 5, vlen)
        if ver <> WebSharper.Core.MetadataIO.CurrentVersion then
            fail (sprintf "format version '%s', expecting '%s', recompile it with the current WebSharper version" ver WebSharper.Core.MetadataIO.CurrentVersion)
        let mutable p = 5 + vlen
        if p + 20 > data.Length then fail "truncated"
        let moduleId = Guid(Array.sub data p 16)
        p <- p + 16
        let count = int32At p
        p <- p + 4
        let headers =
            Array.init count (fun _ ->
                let nlen = int32At p
                if p + 4 + nlen + 9 > data.Length then fail "truncated"
                let name = Encoding.UTF8.GetString(data, p + 4, nlen)
                p <- p + 4 + nlen
                let c = data.[p]
                let raw = int32At (p + 1)
                let stored = int32At (p + 5)
                p <- p + 9
                name, c, raw, stored
            )
        let entries =
            headers |> Array.map (fun (name, c, raw, stored) ->
                if p + stored > data.Length then fail "truncated"
                let e = name, c, raw, p, stored
                p <- p + stored
                e
            )
        Sidecar(path, moduleId, data, entries)

    /// Reads the sidecar of an assembly, if it has one.
    static member TryReadFor(assemblyPath: string) =
        let p = Sidecar.PathFor assemblyPath
        if File.Exists p then Some (Sidecar.Read p) else None

    /// Writes a sidecar file. Entries are deflated unless `compress` returns false for their name.
    /// Rewrites the file only if its contents change, keeping its timestamp otherwise.
    static member Write(path: string, moduleId: Guid, entries: seq<string * byte[]>, ?compress: string -> bool) =
        let compress = defaultArg compress (fun _ -> true)
        let stored =
            entries |> Seq.map (fun (name, bytes: byte[]) ->
                if compress name && bytes.Length >= 256 then
                    use m = new MemoryStream(bytes.Length / 3 + 64)
                    (
                        use d = new DeflateStream(m, CompressionLevel.Fastest, true)
                        d.Write(bytes, 0, bytes.Length)
                    )
                    name, 1uy, bytes.Length, m.ToArray()
                else
                    name, 0uy, bytes.Length, bytes
            )
            |> Array.ofSeq
        use m = new MemoryStream()
        let int32 (v: int) =
            m.WriteByte(byte v); m.WriteByte(byte (v >>> 8)); m.WriteByte(byte (v >>> 16)); m.WriteByte(byte (v >>> 24))
        m.Write(magic, 0, 4)
        let v = Encoding.UTF8.GetBytes WebSharper.Core.MetadataIO.CurrentVersion
        m.WriteByte(byte v.Length)
        m.Write(v, 0, v.Length)
        m.Write(moduleId.ToByteArray(), 0, 16)
        int32 stored.Length
        for (name, c, raw, bytes) in stored do
            let n = Encoding.UTF8.GetBytes(name: string)
            int32 n.Length
            m.Write(n, 0, n.Length)
            m.WriteByte c
            int32 raw
            int32 bytes.Length
        for (_, _, _, bytes) in stored do
            m.Write(bytes, 0, bytes.Length)
        let res = m.ToArray()
        let same =
            File.Exists path && FileInfo(path).Length = int64 res.Length && File.ReadAllBytes path = res
        if not same then
            File.WriteAllBytes(path, res)

/// Web resources of loaded assemblies from their sidecar files, for reflection-based
/// resource rendering at compile time (bundles, offline sitelets).
module SidecarWebResources =

    let private cache = Collections.Concurrent.ConcurrentDictionary<string, DateTime * option<Sidecar>>()

    let private sidecarOf (asm: Reflection.Assembly) =
        // the assembly resolver loads assemblies from bytes, it knows their files
        let location = try asm.Location with _ -> ""
        let loc =
            if location <> "" then location
            else AssemblyResolution.LoadedFrom.TryGet asm |> Option.defaultValue ""
        if String.IsNullOrEmpty loc then None else
        let loc = Sidecar.ImplementationPath loc
        let p = Sidecar.PathFor loc
        let time = if File.Exists p then File.GetLastWriteTimeUtc p else DateTime.MinValue
        match cache.TryGetValue loc with
        | true, (t, sc) when t = time -> sc
        | _ ->
            let sc = if time = DateTime.MinValue then None else (try Some (Sidecar.Read p) with _ -> None)
            cache.[loc] <- (time, sc)
            sc

    /// Content type of a web resource in the sidecar of an assembly, if it has it.
    let TryFind (asm: Reflection.Assembly) (name: string) =
        sidecarOf asm |> Option.bind (fun sc ->
            sc.WebResources |> Array.tryFind (fun (n, _) -> n = name) |> Option.map snd)

    /// Contents of a web resource in the sidecar of an assembly, if it has it.
    let TryRead (asm: Reflection.Assembly) (name: string) =
        sidecarOf asm |> Option.bind (fun sc ->
            match sc.WebResources |> Array.tryFind (fun (n, _) -> n = name) with
            | Some (n, cT) -> sc.TryGet n |> Option.map (fun c -> c, cT)
            | None -> None)

    /// Reads a web resource of the assembly defining a type: from its sidecar file,
    /// or a manifest resource of the assembly.
    let ReadWebResource (ty: Type) (name: string) =
        match TryRead ty.Assembly name with
        | Some (c, cT) ->
            let contentType =
                if cT <> "" then WebSharper.Core.ContentTypes.Parse cT else
                match WebSharper.Core.ContentTypes.TryGuessByFileName name with
                | Some cT -> cT
                | None -> WebSharper.Core.ContentTypes.Text.Plain
            Encoding.UTF8.GetString c, contentType
        | None -> Utility.ReadWebResource ty name

    /// Checks if the assembly defining a type has a web resource in its sidecar file.
    let Has (ty: Type) (name: string) =
        (TryFind ty.Assembly name).IsSome

    /// Makes resource rendering find web resources in sidecar files, for compilation.
    let Install () =
        WebSharper.Core.Resources.IsExternalWebResource <- Has
