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

/// The main entry-point module of WebSharper.
module WebSharper.Program

module FE = IntelliFactory.WebSharper.Compiler.FrontEnd

let private guard action =
    try action () with exn ->
        let temp = System.IO.Path.GetTempFileName()
        System.IO.File.WriteAllText(temp, string exn)
        stdout.WriteLine("[Error] {0}(1,1): {1}: {2}", temp,
            exn.GetType().FullName, exn.Message)
        1

let private compile (opts: Options.CompilationOptions) =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let paths =
        opts.Input :: opts.References
        |> Seq.map (System.IO.Path.GetDirectoryName
            >> System.IO.Path.GetFullPath)
        |> Set.ofSeq
    paths
    |> Seq.iter Loader.AddSearchPath
    let k =
        let aLoader = FE.Loader.Create paths stderr.WriteLine
        let assem = aLoader.LoadFile opts.Input
        let snk =
            opts.KeyPair
            |> Option.map (fun x ->
                let bs = System.IO.File.ReadAllBytes x
                System.Reflection.StrongNameKeyPair bs)
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
                | Some js -> System.IO.File.WriteAllText(path, js)
                | None -> ()
            | None ->
                ()
            match opts.OutputMinified with
            | Some path ->
                match assem.CompressedJavaScript with
                | Some js -> System.IO.File.WriteAllText(path, js)
                | None -> ()
            | None ->
                ()
            for (assem, k, v) in opts.Extraction do
                let a = Mono.Cecil.AssemblyDefinition.ReadAssembly assem
                for r in a.MainModule.Resources do
                    match r with
                    | :? Mono.Cecil.EmbeddedResource as r ->
                        if r.Name = k then
                            let data = r.GetResourceData()
                            System.IO.File.WriteAllBytes(v, data)
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

let private run (opts: Options.T) =
    match opts with
    | Options.Compile opts ->
        compile opts
    | Options.Unpack (folder, assemblies) ->
        let paths =
            Seq.map System.IO.Path.GetDirectoryName assemblies
            |> Set.ofSeq
        let loader = FE.Loader.Create paths stderr.WriteLine
        for p in assemblies do
            let a = loader.LoadFile p
            match a.ReadableJavaScript with
            | Some js ->
                let path =
                    System.IO.Path.Combine(folder,
                        System.IO.Path.GetFileName p + ".js")
                System.IO.File.WriteAllText(path, js)
            | None -> ()
            match a.CompressedJavaScript with
            | Some js ->
                let path =
                    System.IO.Path.Combine(folder,
                        System.IO.Path.GetFileName p + ".min.js")
                System.IO.File.WriteAllText(path, js)
            | None -> ()
        0
    | Options.Dependencies path ->
        DependencyReporter.Run path

guard (fun () -> Options.Run run)
|> System.Environment.Exit

