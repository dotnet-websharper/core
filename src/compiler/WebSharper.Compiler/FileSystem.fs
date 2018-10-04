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

let PrepareFileForWriting (fullPath: string) : unit =
    let fullPath = Path.GetFullPath fullPath
    let d = Path.GetDirectoryName fullPath
    if not (dirExists d) then
        ignore (Directory.CreateDirectory d)

let EnsureBinaryFile (fullPath: string) (contents: byte []) : bool =
    PrepareFileForWriting fullPath
    let inline def () =
        File.WriteAllBytes(fullPath, contents)
        true
    if fileExists fullPath
        then if File.ReadAllBytes fullPath <> contents then def () else false
        else def ()

let EnsureTextFile (fullPath: string) (contents: string) : bool =
    PrepareFileForWriting fullPath
    let inline def () =
        File.WriteAllText(fullPath, contents, DefaultEncoding)
        true
    if fileExists fullPath
        then if File.ReadAllText fullPath <> contents then def () else false
        else def ()

type Binary =
    private { data: byte [] }

    member b.EnsureFile(p) = EnsureBinaryFile p b.data
    member b.GetBytes() = Array.copy b.data
    member b.Read() = new MemoryStream(b.data, false) :> Stream
    member b.Write(s: Stream) = s.Write(b.data, 0, b.data.Length)
    member b.WriteFile(p) = b.EnsureFile(p) |> ignore
    static member FromBytes(bytes) = { data = Array.copy bytes }
    static member ReadFile(fullPath) = { data = File.ReadAllBytes(fullPath) }

    static member ReadStream(s: Stream) =
        use m = new MemoryStream()
        let buf = Array.zeroCreate (8 * 1024)
        let rec loop () =
            let k = s.Read(buf, 0, buf.Length)
            if k > 0 then
                m.Write(buf, 0, k)
                loop ()
        loop ()
        { data = m.ToArray() }

type Content =
    | BinaryContent of Binary
    | TextContent of string

    member c.EnsureFile p =
        match c with
        | BinaryContent b -> b.EnsureFile p
        | TextContent s -> EnsureTextFile p s

    member c.WriteFile p =
        c.EnsureFile p |> ignore

    static member Binary b =
        BinaryContent b

    static member ReadBinaryFile(p) =
        BinaryContent (Binary.ReadFile p)

    static member ReadTextFile(p) =
        File.ReadAllText(p, DefaultEncoding)
        |> TextContent

    static member Text t =
        TextContent t
