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

/// The main entry-point module of WebSharper.
module internal IntelliFactory.WebSharper.Program

open System
open System.IO
open System.Diagnostics
open System.Reflection
open IntelliFactory.Core
open IntelliFactory.WebSharper.Core
open IntelliFactory.WebSharper.Core.Plugins

module FE = IntelliFactory.WebSharper.Compiler.FrontEnd
module Plugins = IntelliFactory.WebSharper.Core.Plugins.Configuration

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

let run (aR: AssemblyResolver) (opts: Options.T) =
    match opts with
    | Options.Compile opts ->
        compile aR opts
    | Options.Unpack (rootDirectory, assemblies) ->
        let pc = PathConventions.PathUtility.FileSystem(rootDirectory)
        let aR = aR.SearchPaths(assemblies)
        let loader = FE.Loader.Create aR stderr.WriteLine
        let emit text path =
            match text with
            | Some text -> writeTextFile (path, text)
            | None -> ()
        let script = PathConventions.ResourceKind.Script
        let content = PathConventions.ResourceKind.Content
        for p in assemblies do
            let a = loader.LoadFile p
            let aid = PathConventions.AssemblyId.Create(a.FullName)
            emit a.ReadableJavaScript (pc.JavaScriptPath aid)
            emit a.CompressedJavaScript (pc.MinifiedJavaScriptPath aid)
            emit a.TypeScriptDeclarations (pc.TypeScriptDefinitionsPath aid)
            let writeText k fn c =
                let p = pc.EmbeddedPath(PathConventions.EmbeddedResource.Create(k, aid, fn))
                writeTextFile (p, c)
            let writeBinary k fn c =
                let p = pc.EmbeddedPath(PathConventions.EmbeddedResource.Create(k, aid, fn))
                writeBinaryFile (p, c)
            for r in a.GetScripts() do
                writeText script r.FileName r.Content
            for r in a.GetContents() do
                writeBinary content r.FileName (r.GetContentData())
        0
    | Options.Dependencies path ->
        DependencyReporter.Run path

type private EA = InterfaceGenerator.Pervasives.ExtensionAttribute

[<EntryPoint>]
let Start args =
    let fullArgs =
        [|
            yield pathToSelf
            yield! args
        |]
    let aR =
        AssemblyResolver.Create()
            .WithBaseDirectory(baseDir)
            .SearchDirectories([baseDir])
    let plugins () =
        match List.ofArray args with
        | "ig" :: path :: args ->
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
            let aR = aR.SearchPaths searchPaths
            aR.Wrap <| fun () ->
                let assem = aR.Resolve(AssemblyName.GetAssemblyName path)
                let ad =
                    match assem with
                    | None -> failwithf "Could not resolve: %s" path
                    | Some assem ->
                        match Attribute.GetCustomAttribute(assem, typeof<EA>) with
                        | :? EA as attr ->
                            attr.GetAssembly()
                        | _ ->
                            failwith "Failed to load assembly definition - \
                                is the assembly properly marked with \
                                ExtensionAttribute?"
                let c = InterfaceGenerator.Compiler.Create()
                c.Start(args, ad, aR)
                |> Some
        | "bundle" :: out :: paths ->
            let bundle =
                FE.Bundle.Create().WithDefaultReferences()
                |> (fun b ->
                    (b, paths)
                    ||> Seq.fold (fun b p -> b.WithAssembly(p)))
                |> (fun b -> b.WithTransitiveReferences())
            let write (c: FE.Content) (ext: string) =
                c.WriteFile(out + ext)
            write bundle.CSS ".css"
            write bundle.HtmlHeaders ".head.html"
            write bundle.JavaScriptHeaders ".head.js"
            write bundle.JavaScript ".js"
            write bundle.MinifiedJavaScript ".min.js"
            write bundle.TypeScript ".d.ts"
            Some 0
        | _ ->
            let env =
                {
                    new IEnvironment with
                        member e.AssemblyResolver = aR
                        member e.CommandLineArgs = fullArgs
                }
            Plugins.GetPlugins()
            |> Seq.tryPick (fun p ->
                match p.Run(env) with
                | Success -> Some 0
                | Error -> Some -1
                | Pass -> None)
    let main () =
        Options.Run plugins (run aR) (Array.toList args)
    guard main
