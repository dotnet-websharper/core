// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Sitelets.Offline.Extra

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let invalidPathChars =
    HashSet(Path.GetInvalidPathChars())

let isValidPath (path: string) =
    path <> null && not (Seq.exists invalidPathChars.Contains path)

let invalidFilePatternChars =
    let h = HashSet(Path.GetInvalidFileNameChars())
    h.Remove('*') |> ignore
    h.Remove('?') |> ignore
    h

let isValidFilePattern (filename: string) =
    if filename = null then
        nullArg "filename in isValidFileName"
    match filename.ToUpper() with
    | "CON" | "PRN" | "AUX" | "NUL"
    | "COM1" | "COM2" | "COM3" | "COM4" | "COM5"
    | "COM6" | "COM7" | "COM8" | "COM9"
    | "LPT1" | "LPT2" | "LPT3" | "LPT4"
    | "LPT5" | "LPT6" | "LPT7" | "LPT8" | "LPT9" -> false
    | _ -> not (Seq.exists invalidFilePatternChars.Contains filename)

type Result<'T> =
    | Ok of 'T
    | Error of string

    member this.Value =
        match this with
        | Ok x -> x
        | Error e -> failwith e

type LocalPath =
    | RPath
    | SPath of LocalPath * string

    member this.SubPath(subPath: string) =
        if isValidFilePattern subPath then
            match subPath, this with
            | ".", _
            | "..", RPath ->  Ok this
            | "..", SPath (x, _) -> Ok x
            | _ -> Ok (SPath (this, subPath))
        else
            Error ("Invalid filename pattern: " + string subPath)

    override this.ToString() =
        use w = new StringWriter()
        this.Write(w)
        w.ToString()

    member this.Write(w: TextWriter) =
        match this with
        | RPath -> w.Write('.')
        | SPath (p, x) ->
            p.Write(w)
            w.Write(Path.DirectorySeparatorChar)
            w.Write(x)

    static member Parse(path: string) =
        if isValidPath path then
            let chars =
                [|
                    Path.AltDirectorySeparatorChar
                    Path.DirectorySeparatorChar
                |]
            path.Split(chars, StringSplitOptions.RemoveEmptyEntries)
            |> Seq.fold (fun root subPath ->
                match root with
                | Ok root ->
                    if isValidFilePattern subPath then
                        match subPath with
                        | "." -> Ok root
                        | ".." ->
                            match root with
                            | SPath (root, _) -> Ok root
                            | RPath -> Ok RPath
                        | _ -> Ok (root / subPath)
                    else
                        Error ("Invalid filename pattern: " + subPath)
                | e -> e)
                (Ok RPath)
        else
            Error ("Invalid file path: " + string path)

    static member Root = RPath

    static member ( / ) (self: LocalPath, subPath: string) =
        self.SubPath(subPath).Value

type EntryKind =
    | DirectoryKind
    | FileKind

type Entry =
    | DirectoryEntry of LocalPath
    | FileEntry of LocalPath

    member this.Path =
        match this with
        | DirectoryEntry p
        | FileEntry p -> p

let toAbsolutePath basePath localPath =
    match localPath with
    | RPath -> basePath
    | _ -> Path.Combine(basePath, string localPath)

let findEntries (basePath: string) (pattern: LocalPath) =

    let toAbsolutePath =
        toAbsolutePath basePath

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
            FileEntry (p / Path.GetFileName(x)))

    let rec findAllEntries path =
        match getEntry path with
        | Some (DirectoryEntry _ as d) ->
            let ap = toAbsolutePath path
            seq {
                yield d
                yield! findFileChildren path None
                for d in Directory.GetDirectories(ap) do
                    yield! findAllEntries (path / Path.GetFileName(d))
            }
        | Some e -> Seq.singleton e
        | None -> Seq.empty

    let rec findFileSystemEntries pattern =
        match pattern with
        | RPath ->
            Option.toArray (getEntry RPath) :> seq<_>
        | SPath (p1, "**") ->
            findDirectories p1
            |> Seq.collect findAllEntries
        | SPath (p1, p2) ->
            findDirectories p1
            |> Seq.collect (fun dir ->
                let p = toAbsolutePath dir
                let dirs =
                    Directory.GetDirectories(p, p2)
                    |> Seq.map (fun n ->
                        DirectoryEntry (dir / Path.GetFileName(n)))
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
        | DirectoryEntry RPath -> false
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
    let p1 = toAbsolutePath src entry.Path
    let p2 = toAbsolutePath dest entry.Path
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
            match LocalPath.Parse(line) with
            | Ok p ->
                findEntries dir p
                |> Seq.iter (copyEntryTo dir dest)
            | Error e ->
                stderr.WriteLine(e)
    else
        stdout.WriteLine("No extra.files specified. Skipping...")
