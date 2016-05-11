// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Sitelets.Offline.Extra

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
module P = PathUtility

type EntryKind =
    | DirectoryKind
    | FileKind

type Entry =
    | DirectoryEntry of P.Path
    | FileEntry of P.Path

    member this.Path =
        match this with
        | DirectoryEntry p
        | FileEntry p -> p

let findEntries (basePath: string) (pattern: Pattern) =

    let ( /. ) a b =
        P.SubPath a b

    let toAbsolutePath =
        P.ToAbsolute basePath

    let getEntry path =
        let p = toAbsolutePath path
        if Directory.Exists(p) then
            Some (DirectoryEntry path)
        elif File.Exists(p) then
            Some (FileEntry path)
        else
            None

    let findFileChildren p pat =
        match pat with
        | None -> Directory.GetFiles(toAbsolutePath p)
        | Some pat -> Directory.GetFiles(toAbsolutePath p, pat)
        |> Seq.map (fun x ->
            FileEntry (p /. Path.GetFileName(x)))

    let rec findAllEntries path =
        match getEntry path with
        | Some (DirectoryEntry _ as d) ->
            let ap = toAbsolutePath path
            seq {
                yield d
                yield! findFileChildren path None
                for d in Directory.GetDirectories(ap) do
                    yield! findAllEntries (path /. Path.GetFileName(d))
            }
        | Some e -> Seq.singleton e
        | None -> Seq.empty

    let rec findFileSystemEntries (pattern: Pattern) =
        match pattern with
        | P.RootPattern ->
            Option.toArray (getEntry P.RootPath) :> seq<_>
        | P.SubPattern (p1, "**") ->
            findDirectories p1
            |> Seq.collect findAllEntries
        | P.SubPattern (p1, p2) ->
            findDirectories p1
            |> Seq.collect (fun dir ->
                let p = toAbsolutePath dir
                let dirs =
                    Directory.GetDirectories(p, p2)
                    |> Seq.map (fun n ->
                        DirectoryEntry (dir /. Path.GetFileName(n)))
                let files = findFileChildren dir (Some p2)
                Seq.append dirs files)

    and findDirectories pattern =
        findFileSystemEntries pattern
        |> Seq.choose (fun e ->
            match e with
            | DirectoryEntry d -> Some d
            | _ -> None)

    findFileSystemEntries pattern
    |> Seq.filter (fun e ->
        match e with
        | DirectoryEntry P.RootPath -> false
        | _ -> true)
    |> Seq.distinct
    |> Seq.cache

let makeDir (path: string) =
    if Directory.Exists(path) |> not then
        Directory.CreateDirectory(path) |> ignore
        stdout.WriteLine("Created directory: {0}", path)

let copyFile (source: string) (dest: string) =
    let destDir = Path.GetDirectoryName(dest)
    makeDir destDir
    let doCopy () =
        File.Copy(source, dest, true)
        stdout.WriteLine("Copied {0} to {1}", source, dest)
    let getLastWriteTime p =
        FileInfo(p).LastWriteTimeUtc
    if File.Exists(dest) then
        if getLastWriteTime dest < getLastWriteTime source then
            doCopy ()
    else
        doCopy ()

let rec copyDirectory (src: string) (dest: string) =
    for s in Directory.GetFiles(src) do
        let d = Path.Combine(dest, Path.GetFileName(s))
        copyFile s d
    for dir in Directory.GetDirectories(src) do
        let d = Path.Combine(dest, Path.GetFileName(dir))
        copyDirectory dir d

let copyEntryTo (src: string) (dest: string) (entry: Entry) =
    let p1 = P.ToAbsolute src entry.Path
    let p2 = P.ToAbsolute dest entry.Path
    match entry with
    | FileEntry _ -> copyFile p1 p2
    | DirectoryEntry _ -> copyDirectory p1 p2

let commentRegex =
    Regex(@"\s*//.*")

let removeComments (text: string) =
    commentRegex.Replace(text, "")

let readLines file =
    File.ReadAllLines(file)
    |> Array.choose (fun line ->
        match removeComments(line).Trim() with
        | "" -> None
        | s -> Some s)

let CopyFiles (dir: string) (dest: string) =
    stdout.WriteLine(dir)
    let extras = Path.Combine(dir, "extra.files")
    if File.Exists(extras) then
        for line in readLines extras do
            match P.TryParsePattern(line) with
            | Some p ->
                findEntries dir p
                |> Seq.iter (copyEntryTo dir dest)
            | None ->
                stderr.WriteLine("Invalid pattern: {0}", line)
    else
        stdout.WriteLine("No extra.files specified. Skipping...")
