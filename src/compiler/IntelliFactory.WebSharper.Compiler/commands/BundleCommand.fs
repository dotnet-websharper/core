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

module BundleCommand =

    type Config =
        {
            AssemblyPaths : list<string>
            FileName : string
            OutputDirectory : string
        }

        static member Create() =
            {
                AssemblyPaths = []
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
        write bundle.TypeScript ".d.ts"
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
