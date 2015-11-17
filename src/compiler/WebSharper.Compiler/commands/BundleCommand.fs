// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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
type Binary = IntelliFactory.Core.FileSystem.Binary

module BundleCommand =

    type Config =
        {
            AssemblyPaths : list<string>
            AppConfigFile : option<string>
            FileName : string
            OutputDirectory : string
        }

        static member Create() =
            {
                AssemblyPaths = []
                AppConfigFile = None
                FileName = "Bundle"
                OutputDirectory = "."
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
        let bundle =
            Bundle.Create().WithDefaultReferences()
                .WithAppConfig(?appConfigFile = config.AppConfigFile)
            |> (fun b ->
                (b, config.AssemblyPaths)
                ||> Seq.fold (fun b p -> b.WithAssembly(p)))
            |> (fun b -> b.WithTransitiveReferences())
        let write (c: Content) (ext: string) =
            c.WriteFile(Path.Combine(config.OutputDirectory, config.FileName + ext))
        write bundle.CSS ".css"
        write bundle.HtmlHeaders ".head.html"
        write bundle.JavaScriptHeaders ".head.js"
        write bundle.JavaScript ".js"
        write bundle.MinifiedJavaScript ".min.js"
        //write bundle.TypeScript ".d.ts"
        // TODO : correct .d.ts output for bundles (WIG types currenty not included)

        let writeBinaryFile (output, bytes) =
            Binary.FromBytes(bytes).WriteFile(output)
        for r in bundle.ContentFiles do
            writeBinaryFile (System.IO.Path.Combine(config.OutputDirectory, r.FileName), r.GetContentData())

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
