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

namespace IntelliFactory.WebSharper.Compiler

module C = Commands

module HtmlCommand =

    type Mode =
        | Debug
        | Release

    type Config =
        {
            MainAssemblyPath : string
            Mode : Mode
            OutputDirectory : string
            ProjectDirectory : string
            ReferenceAssemblyPaths : list<string>
        }

        static member Create(mainAssemblyPath) =
            {
                MainAssemblyPath = mainAssemblyPath
                Mode = Debug
                OutputDirectory = "."
                ProjectDirectory = "."
                ReferenceAssemblyPaths = []
            }

    exception BadOptions of string

    let GetErrors config =
        [
            if C.NoDir config.ProjectDirectory then
                yield "-project parameter invalid (not such directory): " + string config.ProjectDirectory
            if C.NoFile config.MainAssemblyPath then
                yield "-site parameter invalid (no such file)"
            for r in config.ReferenceAssemblyPaths do
                if C.NoFile r then
                    yield "-ref parameter invalid (no such file): " + string r
        ]

    type IHtmlCommand =
        abstract Execute : C.Environment * Config -> C.Result

    let Exec env config =
        let t =
            Type.GetType("IntelliFactory.WebSharper.Sitelets.Offline.HtmlCommand,
                IntelliFactory.WebSharper.Sitelets", throwOnError = true)
        let cmd = Activator.CreateInstance(t) :?> IHtmlCommand
        cmd.Execute(env, config)

    let Parse (args: list<string>) =
        let trim (s: string) =
            let s = s.Trim()
            if s.StartsWith "\"" && s.EndsWith "\"" then
                s.Substring(1, s.Length - 2).Trim()
            else
                s
        let setMode opts (mode: string) =
            let m = mode.Trim().ToLower()
            let oMode =
                if m.StartsWith("debug") then
                    Debug
                elif m.StartsWith("release") then
                    Release
                else
                    BadOptions ("Invalid mode option: " + mode +
                        ". User either debug or release.")
                    |> raise
            { opts with Mode = oMode}
        let setOutputDirectory opts path =
            { opts with OutputDirectory = path }
        let setProjectDirectory opts path =
            { opts with ProjectDirectory = path }
        let addReferenceFile opts path =
            { opts with ReferenceAssemblyPaths = path :: opts.ReferenceAssemblyPaths }
        let setSourceAssembly opts path =
            { opts with MainAssemblyPath = path}
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
                    | "-o" -> "-out"
                    | "-s" -> "-site"
                    | _ -> raise (BadOptions <| "Invalid parameter: " + x)
                proc opts (x :: xs)
        match args with
        | "html" :: args ->
            try
                let def = Config.Create("")
                let cfg = proc def args
                match GetErrors cfg with
                | [] -> C.Parsed cfg
                | errors -> C.ParseFailed errors
            with BadOptions reason ->
                C.ParseFailed [reason]
        | _ -> C.NotRecognized

    let Description =
        "generates static sites from sitelet assemblies"

    let Usage =
        [
            "Usage: WebSharper.exe sitelets [OPTIONS] [INPUTS]"
            "-mode <mode>    Either Debug([-].*)? or Release([-].*)? (defaults to Debug)."
            "-ref <file>     Path to a reference assembly."
            "-out <dir>      Path to the output directory. Short form: -o"
            "-project <dir>  Path to the project directory."
            "-site <file>    Path to the assembly containing the web site. Short form: -s"
        ]
        |> String.concat System.Environment.NewLine

    let Instance =
        C.DefineCommand<Config> "html" Description Usage Parse Exec
