// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open FileSystem
module PC = WebSharper.PathConventions
module C = Commands
module Re = WebSharper.Core.Resources 

module UnpackCommand =
    type Config =
        {
            Assemblies : list<string>
            RootDirectory : string
            UnpackSourceMap : bool
            UnpackTypeScript : bool
            DownloadResources : bool
            Loader : option<Loader>
            Logger : LoggerBase
        }

        static member Create() =
            {
                Assemblies = []
                RootDirectory = "."
                UnpackSourceMap = false
                UnpackTypeScript = false
                DownloadResources = false
                Loader = None
                Logger = ConsoleLogger()
            }

    let GetErrors config =
        [
            for a in config.Assemblies do
                if C.NoFile a then
                    yield "invalid file: " + a
            if C.NoDir config.RootDirectory then
                yield "-root: invalid directory " + config.RootDirectory
        ]

    let Parse args =
        let rec proc opts xs =
            match xs with
            | [] ->
                opts
            | "-root" :: root :: xs ->
                proc { opts with RootDirectory = root } xs
            | "-sm" :: xs ->
                proc { opts with UnpackSourceMap = true } xs
            | "-dts" :: xs ->
                proc { opts with UnpackTypeScript = true } xs
            | x :: xs ->
                proc { opts with Assemblies = x :: opts.Assemblies } xs
        match args with
        | "-unpack" :: root :: args ->
            let def = Config.Create()
            let cfg = { proc def args with RootDirectory = root }
            match GetErrors cfg with
            | [] -> C.Parsed cfg
            | errors -> C.ParseFailed errors
        | "unpack" :: args ->
            let def = Config.Create()
            let cfg = proc def args
            match GetErrors cfg with
            | [] -> C.Parsed cfg
            | errors -> C.ParseFailed errors
        | _ -> C.NotRecognized

    let private localResTyp = typeof<Re.IDownloadableResource>

    let Exec env cmd =
        let errors = ResizeArray()
        let baseDir =
            let pathToSelf = typeof<Config>.Assembly.Location
            Path.GetDirectoryName(pathToSelf)
        let aR =
            AssemblyResolver.Create()
                .SearchDirectories([baseDir])
                .SearchPaths(cmd.Assemblies)
        let loader = 
            match cmd.Loader with
            | Some l ->
                l.WithAssemblyResolver(aR)
            | _ ->
                Loader.Create aR errors.Add
        let pc = PC.PathUtility.FileSystem(cmd.RootDirectory)
        let writeTextFile (output, text) =
            Content.Text(text).WriteFile(output)
        let writeBinaryFile (output, bytes) =
            Binary.FromBytes(bytes).WriteFile(output)
        System.IO.Directory.CreateDirectory cmd.RootDirectory |> ignore
        let emit text path =
            match text with
            | Some text -> writeTextFile (path, text)
            | None -> ()
        let emitWithMap text path mapping mapFileName mapPath =
            if cmd.UnpackSourceMap then
                let text =
                    text |> Option.map (fun t ->
                    match mapping with
                    | None -> t
                    | Some _ -> t + ("\n//# sourceMappingURL=" + mapFileName))
                emit text path
                emit mapping mapPath
            else
                emit text path
        let script = PC.ResourceKind.Script
        let content = PC.ResourceKind.Content

        if cmd.DownloadResources then
            aR.Wrap <| fun () ->
                for p in cmd.Assemblies do
                    DownloadResources.DownloadResource p cmd.RootDirectory |> errors.AddRange
            cmd.Logger.TimedStage "Download Resource"

        for p in cmd.Assemblies do
            match (try loader.LoadFile (p, false) |> Some with _ -> None) with 
            | None -> () 
            | Some a ->
            let aid = PC.AssemblyId.Create(a.Name)
            emitWithMap a.ReadableJavaScript (pc.JavaScriptPath aid)
                a.MapFileForReadable (pc.MapFileName aid) (pc.MapFilePath aid)
            emitWithMap a.CompressedJavaScript (pc.MinifiedJavaScriptPath aid)
                a.MapFileForCompressed (pc.MinifiedMapFileName aid) (pc.MinifiedMapFilePath aid)
            emit a.TypeScript (pc.TypeScriptPath aid)
            if cmd.UnpackTypeScript then
                emit a.TypeScriptDeclarations (pc.TypeScriptDefinitionsPath aid)
            let writeText k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeTextFile (p, c)
            let writeBinary k fn c =
                let p = pc.EmbeddedPath(PC.EmbeddedResource.Create(k, aid, fn))
                writeBinaryFile (p, c)
            for r in a.GetScripts() do
                writeText script r.FileName r.Content
            for r in a.GetContents() do
                writeBinary content r.FileName (r.GetContentData())
        
        if errors.Count = 0 then
            C.Ok
        else
            C.Errors (List.ofSeq errors)

    let Description =
        "unpacks resources from WebSharper assemblies"

    let Usage =
        [
            "Usage: WebSharper.exe unpack [OPTIONS] assembly.dll ..."
            "-root <dir>    Path to web project root directory"
            "-sm            Unpack source maps and source files"
            //"-dts           Unpack TypeScript declaration files"
        ]
        |> String.concat System.Environment.NewLine

    let Instance =
        C.DefineCommand<Config> "unpack" Description Usage Parse Exec
