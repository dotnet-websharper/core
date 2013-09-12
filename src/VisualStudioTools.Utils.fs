// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.VisualStudioTools

module Utils =
    open System
    open System.IO
    open System.IO.Compression
    open System.Text

    let enc = UTF8Encoding(false, true)

    type Content =
        | ByteContent of byte []
        | TextContent of string

        member this.GetBytes() =
            match this with
            | ByteContent bs -> Array.copy bs
            | TextContent t -> enc.GetBytes(t)

        member this.Write(s: Stream) =
            let bs =
                match this with
                | ByteContent bs -> bs
                | TextContent t -> enc.GetBytes(t)
            s.Write(bs, 0, bs.Length)

        member this.WriteFile(path) =
            let f = FileInfo(path)
            let d = f.Directory
            if not d.Exists then
                d.Create()
            match this with
            | ByteContent bytes ->
                File.WriteAllBytes(path, bytes)
            | TextContent text ->
                File.WriteAllText(path, text)

        static member FromBytes(bs) =
            ByteContent (Array.copy bs)

        static member FromText(t) =
            TextContent t

        static member ReadBinaryFile(path) =
            ByteContent (File.ReadAllBytes(path))

        static member ReadTextFile(path) =
            TextContent (File.ReadAllText(path))

    let normalizeEntryPath (path: string) =
        let path = path.Replace(@"\", "/")
        let path = if path.StartsWith(".") then path.Substring(1) else path
        path.TrimStart([| '/' |])

    type Content with

        static member Zip(entries) =
            use memory = new MemoryStream()
            do
                use zip = new Compression.ZipArchive(memory, Compression.ZipArchiveMode.Create)
                for (path, content: Content) in entries do
                    let path = normalizeEntryPath path
                    let entry = zip.CreateEntry(path)
                    use s = entry.Open()
                    content.Write(s)
            memory.ToArray()
            |> Content.FromBytes
