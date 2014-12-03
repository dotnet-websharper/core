// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// The main entry-point module of WebSharper.
module internal IntelliFactory.WebSharper.Program

open System
open System.IO
open System.Diagnostics
open System.Reflection
open IntelliFactory.Core
open IntelliFactory.WebSharper.Core

module FE = IntelliFactory.WebSharper.Compiler.FrontEnd

let writeTextFile (output, text) =
    Content.Text(text).WriteFile(output)

let writeBinaryFile (output, bytes) =
    Binary.FromBytes(bytes).WriteFile(output)

let guard action =
    try action () with exn ->
        let temp = Path.GetTempFileName()
        writeTextFile (temp, string exn)
        stdout.WriteLine("[Error] {0}(1,1): {1}: {2}", temp,
            exn.GetType().FullName, exn.Message)
        1

let pathToSelf = Assembly.GetExecutingAssembly().Location
let baseDir = Path.GetDirectoryName pathToSelf

let compile (aR: AssemblyResolver) (opts: Options.CompilationOptions) =
    let sw = Stopwatch()
    sw.Start()
    let refPaths =
        opts.Input :: opts.References
        |> Seq.map Path.GetFullPath
        |> Set.ofSeq
    let aR = aR.SearchPaths(refPaths)
    aR.Wrap <| fun () ->
        let k =
            let aLoader = FE.Loader.Create aR stderr.WriteLine
            let assem = aLoader.LoadFile opts.Input
            let snk =
                opts.KeyPair
                |> Option.map (fun x ->
                    let bs = File.ReadAllBytes x
                    StrongNameKeyPair(bs))
            let refs = List.map aLoader.LoadFile opts.References
            let options : FE.Options =
                {
                    ErrorLimit = opts.ErrorLimit
                    KeyPair = snk
                    References = refs
                }
            let result = FE.Compile options stderr.WriteLine assem
            if result then
                assem.Write snk opts.Output
                match opts.OutputJavaScript with
                | Some path ->
                    match assem.ReadableJavaScript with
                    | Some js -> writeTextFile (path, js)
                    | None -> ()
                | None -> ()
                match opts.OutputMinified with
                | Some path ->
                    match assem.CompressedJavaScript with
                    | Some js -> writeTextFile (path, js)
                    | None -> ()
                | None -> ()
                match opts.OutputTypeScript with
                | Some path ->
                    match assem.TypeScriptDeclarations with
                    | Some dts -> writeTextFile (path, dts)
                    | None -> ()
                | None -> ()
                for (assem, k, v) in opts.Extraction do
                    let a = Mono.Cecil.AssemblyDefinition.ReadAssembly assem
                    for r in a.MainModule.Resources do
                        match r with
                        | :? Mono.Cecil.EmbeddedResource as r ->
                            if r.Name = k then
                                let data = r.GetResourceData()
                                writeBinaryFile (v, data)
                        | _ ->
                            ()
                0
            else
                1
        sw.Stop()
        if k = 0 then
            stdout.WriteLine("Compilation succeeded in {0} seconds.",
                sw.Elapsed.TotalSeconds)
        else
            stderr.WriteLine("Compilation failed in {0} seconds.",
                sw.Elapsed.TotalSeconds)
        k

let ShowResult (r: Compiler.Commands.Result) =
    match r with
    | Compiler.Commands.Ok -> 0
    | Compiler.Commands.Errors errors ->
        for e in errors do
            stderr.WriteLine(e)
        1

let Run (aR: AssemblyResolver) (opts: Options.T) =
    match opts with
    | Options.Compile opts ->
        compile aR opts
    | Options.Dependencies path ->
        DependencyReporter.Run path

type private EA = InterfaceGenerator.Pervasives.ExtensionAttribute

open IntelliFactory.WebSharper.Compiler

let (|Cmd|_|) (cmd: Commands.ICommand) argv =
    match cmd.Parse argv with
    | Commands.NotRecognized -> None
    | Commands.Parsed f ->
        match f (Commands.Environment.Create()) with
        | Commands.Ok -> Some 0
        | Commands.Errors errors ->
            for e in errors do
                stderr.WriteLine(e)
            Some 1
    | Commands.ParseFailed err ->
        for e in err do
            stderr.WriteLine(e)
        stderr.WriteLine()
        stderr.WriteLine(cmd.Usage)
        Some 1

let AR =
    AssemblyResolver.Create()
        .WithBaseDirectory(baseDir)
        .SearchDirectories([baseDir])

let RunInterfaceGenerator path args =
    let searchPaths =
        let (|S|_|) (a: string) (b: string) =
            if b.StartsWith a then Some (b.Substring a.Length) else None
        let rec loop acc args =
            match args with
            | S "-r:" assem :: rest -> loop (assem :: acc) rest
            | "-r" :: assem :: rest -> loop (assem :: acc) rest
            | _ :: rest -> loop acc rest
            | [] | [_] -> acc
        loop [path] args
    let AR = AR.SearchPaths searchPaths
    AR.Wrap <| fun () ->
        let name = AssemblyName.GetAssemblyName(path)
        match AR.Resolve(name) with
        | None ->
            failwithf "Could not resolve: %s" path
        | Some assem ->
            let ad =
                match Attribute.GetCustomAttribute(assem, typeof<EA>) with
                | :? EA as attr ->
                    attr.GetAssembly()
                | _ ->
                    failwith "Failed to load assembly definition - \
                        is the assembly properly marked with \
                        ExtensionAttribute?"
            let c = InterfaceGenerator.Compiler.Create()
            c.Start(args, ad, assem, AR)

[<EntryPoint>]
let Start argv =
    guard <| fun () ->
        match List.ofArray argv with
        | Cmd BundleCommand.Instance r -> r
        | Cmd HtmlCommand.Instance r -> r
        | Cmd UnpackCommand.Instance r -> r
        | "ig" :: path :: args -> RunInterfaceGenerator path args
        | argv -> Options.Run (Run AR) argv
