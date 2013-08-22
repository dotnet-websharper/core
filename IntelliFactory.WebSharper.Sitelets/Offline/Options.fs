// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Provides tools for creating the pages as physical files.
module internal IntelliFactory.WebSharper.Sitelets.Offline.Options

open System.IO

exception BadOptions of string

/// Represents command-line options.
type T =
    {
        ReferenceFiles : list<FileInfo>
        OutputDirectory : DirectoryInfo
        ProjectDirectory : DirectoryInfo
        SourceAssembly : FileInfo
        Mode : Output.Mode
    }

let Default =
    let pwd = System.Environment.CurrentDirectory
    {
        ReferenceFiles = []
        OutputDirectory = DirectoryInfo pwd
        SourceAssembly = FileInfo "nofile"
        ProjectDirectory = DirectoryInfo pwd
        Mode = Output.Debug
    }

/// The help string that describes the options.
let Help =
    let a = System.Reflection.Assembly.GetExecutingAssembly()
    let v = System.Diagnostics.FileVersionInfo.GetVersionInfo a.Location
    [
        "IntelliFactory WebSharper (TM) Compiler build " + v.FileVersion
        "Copyright (c) IntelliFactory. All Rights Reserved."
        ""
        "Usage: WebSharper.exe sitelets [OPTIONS] [INPUTS]"
        "-mode <mode>    Either Debug([-].*)? or Release([-].*)? (defaults to Debug)."
        "-ref <file>     Path to a reference assembly."
        "-out <dir>      Path to the output directory. Short form: -o."
        "-project <dir>  Path to the project directory."
        "-site <file>    Path to the assembly containing the web site."
    ]
    |> String.concat System.Environment.NewLine

/// Parses the command-line arguments.
let Parse (args: seq<string>) =
    let args = Seq.toList args

    let trim (s: string) =
        let s = s.Trim()
        if s.StartsWith "\"" && s.EndsWith "\"" then
            s.Substring(1, s.Length - 2).Trim()
        else
            s

    let setMode opts (mode: string) =
        let m = mode.Trim().ToLower()
        let oMode =
            if m.StartsWith "debug" then
                Output.Debug
            elif m.StartsWith "release" then
                Output.Release
            else
                BadOptions ("Invalid mode option: " + mode +
                    ". User either debug or release.")
                |> raise

        { opts with Mode = oMode}

    let setOutputDirectory opts path =
        {
            opts with
                OutputDirectory = Directory.CreateDirectory path
        }

    let setProjectDirectory opts path =
        let dir = DirectoryInfo path
        if not dir.Exists then
            raise <| BadOptions ("Could not find directory: " + path)
        { opts with ProjectDirectory = dir }

    let addReferenceFile opts path =
        let f = FileInfo path
        if not f.Exists then
            raise <| BadOptions ("Could not find file: " + f.FullName)
        { opts with ReferenceFiles = f :: opts.ReferenceFiles }

    let setSourceAssembly opts path =
        let file = FileInfo path
        if not file.Exists then
            raise <| BadOptions ("Could not find file: " + path)
        { opts with SourceAssembly = file}

    let rec proc opts xs =
        match xs with
            | [] ->
                opts
            | "-project" :: d :: xs ->
                proc (setProjectDirectory opts (trim d)) xs
            | "-mode" :: f :: xs ->
                proc (setMode opts (trim f)) xs
            | "-ref" :: f :: xs ->
                proc (addReferenceFile opts (trim f)) xs
            | "-out" :: f :: xs ->
                proc (setOutputDirectory opts (trim f)) xs
            | "-site" ::f :: xs ->
                proc (setSourceAssembly opts (trim f)) xs
            | x :: xs ->
                let x =
                    match x with
                    | "-src" -> "-source"
                    | "-o" -> "-out"
                    | "-s" -> "-site"
                    | _ ->
                        raise (BadOptions <| "Invalid parameter: " + x)
                proc opts (x :: xs)
    let opts =
        List.ofSeq args
        |> proc Default
    if opts.OutputDirectory.Exists
        && opts.SourceAssembly.Exists
        && opts.OutputDirectory.FullName <> ""
    then opts
    else raise (BadOptions "")

