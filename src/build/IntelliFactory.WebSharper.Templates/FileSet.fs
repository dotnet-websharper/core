// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Templates

open System
open System.IO
open SharpCompress

type FSDir =
    | FSDir of Map<string,FSNode>

and FSNode =
    | FSDirNode of FSDir
    | FSFileNode of byte []

[<AutoOpen>]
module FileSetImplementation =

    let SplitPath (path: string) =
        let chars = [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]
        path.Split(chars, StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let Parent entries =
        match List.rev entries with
        | [] -> []
        | x :: xs -> List.rev xs

    type FS<'D> =
        {
            ListDirs : 'D -> seq<string * 'D>
            ListFiles : 'D -> seq<string * byte[]>
        }

    let rec BuildDir fs dir =
        [
            for (n, d) in fs.ListDirs dir do
                yield (n, BuildDir fs d |> FSDirNode)
            for (n, b) in fs.ListFiles dir do
                yield (n, FSFileNode b)
        ]
        |> Map.ofSeq
        |> FSDir

    let GroupEntries all =
        let dirs =
            Seq.map fst all
            |> Seq.map Path.GetDirectoryName
            |> Seq.filter ((<>) "")
            |> Seq.distinct
            |> Seq.toArray
        let isIn dir p = Path.GetDirectoryName(p) = dir
        let listFiles dir =
            all
            |> Seq.filter (fst >> isIn dir)
            |> Seq.map (fun (x, b)-> (Path.GetFileName(x), b))
        let listDirs dir =
            Seq.filter (isIn dir) dirs
            |> Seq.map (fun x -> (Path.GetFileName(x), x))
        BuildDir { ListFiles = listFiles; ListDirs = listDirs } ""

    let ReadDir (dir: DirectoryInfo) =
        let listFiles (dir: DirectoryInfo) =
            dir.EnumerateFiles()
            |> Seq.map (fun f -> (f.Name, File.ReadAllBytes(f.FullName)))
        let listDirs (dir: DirectoryInfo) =
            dir.EnumerateDirectories()
            |> Seq.map (fun d -> (d.Name, d))
        BuildDir { ListFiles = listFiles; ListDirs = listDirs } dir

    let ReadZip (stream: Stream) =
        use re = Reader.ReaderFactory.Open(stream)
        let all = ResizeArray()
        while re.MoveToNextEntry() do
            if not re.Entry.IsDirectory then
                let path = re.Entry.FilePath
                let bytes =
                    use m = new MemoryStream()
                    re.WriteEntryTo(m)
                    m.ToArray()
                all.Add(path, bytes)
        GroupEntries all

    let rec Populate (FSDir d) (out: string) =
        EnsureDir out
        for KeyValue (k, v) in d do
            let pa = Path.Combine(out, k)
            match v with
            | FSDirNode d -> Populate d pa
            | FSFileNode f -> File.WriteAllBytes(pa, f)

    let EmptyDir =
        FSDir Map.empty

    let Cd path (FSDir d) =
        match Map.tryFind path d with
        | Some (FSDirNode d) -> Some d
        | _ -> None

[<Sealed>]
type FileSet(dir: FSDir) =

    let sub path =
        match Cd path dir with
        | Some dir -> FileSet(dir)
        | None -> failwithf "Invalid sub-directory: %s" path

    member fs.Populate(out) = Populate dir out
    member fs.Item with get path = sub path

    static member FromDirectory(path: string) =
        let d = DirectoryInfo(path)
        if d.Exists then FileSet(ReadDir d) else
            invalidArg "path" (sprintf "No such directory: %s" path)

    static member FromZip(s) =
        FileSet(ReadZip s)

    static member FromZipFile(path: string) =
        use stream = File.OpenRead(path)
        FileSet.FromZip(stream)
