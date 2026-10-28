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

module WebSharper.Compiler.FileSystem

open System.IO
open System.Text

let DefaultEncoding =
    let emitBOM = false
    let throwOnError = true
    UTF8Encoding(emitBOM, throwOnError) :> Encoding

let fileExists (file: string) =
    FileInfo(file).Exists

let dirExists (path: string) =
    DirectoryInfo(path).Exists

module OutputFiles =

    // AsyncLocal keeps concurrent compilations in the booster service apart
    let private current = System.Threading.AsyncLocal<System.Collections.Concurrent.ConcurrentDictionary<string, unit>>()

    let Start () =
        current.Value <- System.Collections.Concurrent.ConcurrentDictionary()

    let Add (path: string) =
        match current.Value with
        | null -> ()
        | files -> files.TryAdd(Path.GetFullPath path, ()) |> ignore

    // Read by the WebSharperCheckOutputFiles MSBuild target
    let Finish (assemblyFile: string) =
        match current.Value with
        | null -> ()
        | files ->
            current.Value <- null
            let listFile = Path.ChangeExtension(assemblyFile, ".websharper.files")
            if files.IsEmpty then
                if File.Exists listFile then File.Delete listFile
            else
                File.WriteAllLines(listFile, Seq.sort files.Keys)

let PrepareFileForWriting (fullPath: string) : unit =
    let fullPath = Path.GetFullPath fullPath
    let d = Path.GetDirectoryName fullPath
    if not (dirExists d) then
        ignore (Directory.CreateDirectory d)

let EnsureBinaryFile (fullPath: string) (contents: byte []) : bool =
    PrepareFileForWriting fullPath
    OutputFiles.Add fullPath
    let inline def () =
        File.WriteAllBytes(fullPath, contents)
        true
    if fileExists fullPath
        then if File.ReadAllBytes fullPath <> contents then def () else false
        else def ()

let EnsureTextFile (fullPath: string) (contents: string) : bool =
    PrepareFileForWriting fullPath
    OutputFiles.Add fullPath
    let inline def () =
        File.WriteAllText(fullPath, contents, DefaultEncoding)
        true
    if fileExists fullPath
        then if File.ReadAllText fullPath <> contents then def () else false
        else def ()

type Binary =
    private { data: byte [] }

    member b.EnsureFile(fullPath) = EnsureBinaryFile fullPath b.data
    member b.GetBytes() = Array.copy b.data
    member b.Read() = new MemoryStream(b.data, false) :> Stream
    member b.Write(s: Stream) = s.Write(b.data, 0, b.data.Length)
    member b.WriteFile(fullPath) = b.EnsureFile(fullPath) |> ignore
    static member FromBytes(bytes) = { data = Array.copy bytes }
    static member ReadFile(fullPath) = { data = File.ReadAllBytes(fullPath) }

    static member ReadStream(stream: Stream) =
        use m = new MemoryStream()
        let buf = Array.zeroCreate (8 * 1024)
        let rec loop () =
            let k = stream.Read(buf, 0, buf.Length)
            if k > 0 then
                m.Write(buf, 0, k)
                loop ()
        loop ()
        { data = m.ToArray() }

type Content =
    | BinaryContent of Binary
    | TextContent of string

    member c.EnsureFile fullPath =
        match c with
        | BinaryContent b -> b.EnsureFile fullPath
        | TextContent s -> EnsureTextFile fullPath s

    member c.WriteFile fullPath =
        c.EnsureFile fullPath |> ignore

    static member Binary b =
        BinaryContent b

    static member ReadBinaryFile(fullPath) =
        BinaryContent (Binary.ReadFile fullPath)

    static member ReadTextFile(fullPath) =
        File.ReadAllText(fullPath, DefaultEncoding)
        |> TextContent

    static member Text t =
        TextContent t
