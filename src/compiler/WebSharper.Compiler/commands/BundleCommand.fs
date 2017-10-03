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

namespace WebSharper.Compiler

module C = Commands

module BundleCommand =

    type Config =
        {
            AssemblyPaths : list<string>
            AppConfigFile : option<string>
            FileName : string
            OutputDirectory : string
            SourceMap : bool
            DeadCodeElimination : bool
        }

        static member Create() =
            {
                AssemblyPaths = []
                AppConfigFile = None
                FileName = "Bundle"
                OutputDirectory = "."
                SourceMap = false
                DeadCodeElimination = true
            }

    let GetErrors config =
        [
            for p in config.AssemblyPaths do
                if C.NoFile p then
                    yield "No such file: " + p
            if C.IsFile config.OutputDirectory then
                yield "-out parameter invalid: " + config.OutputDirectory
        ]

    let Parse (args: list<string>) =
        let rec proc opts xs =
            match xs with
            | [] ->
                opts
            | "-name" :: name :: xs ->
                proc { opts with FileName = name } xs
            | "-o" :: f :: xs | "-out" :: f :: xs ->
                proc { opts with OutputDirectory = f } xs
            | "-appconfig" :: f :: xs ->
                proc { opts with AppConfigFile = Some f } xs
            | x :: xs ->
                proc { opts with AssemblyPaths = x :: opts.AssemblyPaths } xs
        match args with
        | "bundle" :: args ->
            let def = Config.Create()
            let cfg = proc def args
            match GetErrors cfg with
            | [] -> C.Parsed cfg
            | errors -> C.ParseFailed errors
        | _ -> C.NotRecognized

    let Exec env config =
        C.MkDir config.OutputDirectory

        let resolver =
            let r = AssemblyResolver.Create()
            config.AssemblyPaths
            |> Seq.map Path.GetDirectoryName
            |> Seq.distinct
            |> r.SearchDirectories

        let loader = Loader.Create resolver ignore

        let bundle = Bundle((config.AssemblyPaths |> List.map loader.LoadFile), resolver, config.SourceMap, config.DeadCodeElimination, config.FileName, ?appConfig = config.AppConfigFile)
        System.IO.Directory.CreateDirectory config.OutputDirectory |> ignore
        let write (c: Content) (ext: string) =
            c.WriteFile(Path.Combine(config.OutputDirectory, config.FileName + ext))
        let writeMapped (c: Content) m (ext: string) =
            write c ext
            m |> Option.iter (fun mc ->
                let mapExt = ext.Replace(".js", ".map")
                write mc mapExt
                File.AppendAllLines(
                    Path.Combine(config.OutputDirectory, config.FileName + ext),
                    [| "//# sourceMappingURL=" + config.FileName + mapExt |]
                )
            )
        write bundle.CSS ".css"
        write bundle.HtmlHeaders ".head.html"
        write bundle.JavaScriptHeaders ".head.js"
        writeMapped bundle.JavaScript bundle.Mapping ".js"
        writeMapped bundle.MinifiedJavaScript bundle.MinifiedMapping ".min.js"
        //write bundle.TypeScript ".d.ts"
        // TODO : correct .d.ts output for bundles (WIG types currenty not included)
        C.Ok

    let Description =
        "generates JS/CSS bundles from WebSharper assembly sets"

    let Usage =
        [
            "Usage: WebSharper.exe bundle [OPTIONS] X.dll ..."
            "-name <id>      Name prefix for the output files"
            "-out <dir>      Path to the output directory. Short form: -o"
        ]
        |> String.concat System.Environment.NewLine

    let Instance =
        C.DefineCommand<Config> "bundle" Description Usage Parse Exec
