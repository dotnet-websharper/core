// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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
        SourceDirectories : list<DirectoryInfo>
        OutputDirectory : DirectoryInfo
        ProjectDirectory : DirectoryInfo
        SourceAssembly : FileInfo
        Mode : Output.Mode
    }

let Default =
    let pwd = System.Environment.CurrentDirectory
    {
        SourceDirectories = []
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
        "Usage: wsos.exe [OPTIONS] [INPUTS]"
        "--mode           Either Debug or Release (defaults to Debug)."
        "--source:<dir>   Path to the source directory. Short form: -src."
        "--out:<dir>      Path to the output directory. Short form: -o."
        "--project:<dir>  Path to the project directory."
        "--site:<dir>     Name of the assembly containing the web site. \
            Short form: -s."
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
            match m with
            | "debug" ->
                Output.Debug
            | "release" ->
                Output.Release
            | _         ->
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

    let setSourceDirectory opts path =
        let dir = DirectoryInfo path
        if not dir.Exists then
            raise <| BadOptions ("Could not find directory: " + path)
        { opts with SourceDirectories = dir :: opts.SourceDirectories}

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
            | "-source" :: f :: xs ->
                proc (setSourceDirectory opts (trim f)) xs
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

