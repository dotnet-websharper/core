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

module WebSharper.CSharp.Program

open System
open System.IO
open System.Reflection
open WebSharper
open WebSharper.Compiler

open WebSharper.Compile.CommandTools
open WebSharper.Compiler.FrontEnd

let Compile config =
    StartTimer()

    if config.AssemblyFile = null then
        failwith "You must provide assembly output path."

    if not (File.Exists config.AssemblyFile) then () else

    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)
    
    let loader = Loader.Create aR (printfn "%s")
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    let refErrors = ResizeArray()
    let refMeta =
        let metas = refs |> List.choose (fun r -> 
            try ReadFromAssembly FullMetadata r
            with e ->
                refErrors.Add e.Message
                None
        )
        if refErrors.Count > 0 || List.isEmpty metas then None 
        else 
            try
                Some { 
                    WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
                        Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
                }
            with e ->
                refErrors.Add <| "Error merging WebSharper metadata: " + e.Message
                None

    TimedStage "Loading referenced metadata"

    if refErrors.Count > 0 then
        for err in refErrors do 
            eprintfn "WebSharper error %s" err
    else

    let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler(printfn "%s", UseVerifier = false)

    let referencedAsmNames =
        paths
        |> Seq.map (fun i -> 
            let n = Path.GetFileNameWithoutExtension(i)
            n, i
        )
        |> Map.ofSeq

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let assemblyResolveHandler = ResolveEventHandler(fun _ e ->
            let assemblyName = AssemblyName(e.Name).Name
            match Map.tryFind assemblyName referencedAsmNames with
            | None -> null
            | Some p -> 
                if assemblyName = "FSharp.Core" then
                    typeof<option<_>>.Assembly
                elif assemblyName = thisName then
                    Assembly.Load(File.ReadAllBytes(p))
                else
                    Assembly.LoadFrom(p)
        )

    System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)

    if config.ProjectFile = null then
        failwith "You must provide project file path."
    
    let comp =
        compiler.Compile(refMeta, config.CompilerArgs, config.ProjectFile, config.WarnOnly)

    let mutable hasErrors = false

    if not (List.isEmpty comp.Errors) then        
        for pos, e in comp.Errors do
            match pos with
            | Some pos ->
                eprintfn "%s (%d,%d)-(%d,%d) WebSharper error %s" 
                    pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) (e.ToString())
            | _ ->
                eprintfn "WebSharper error %s" (e.ToString())
        if not config.WarnOnly then hasErrors <- true
        
    if hasErrors then () else

    let assem = loader.LoadFile config.AssemblyFile
    let js =
        ModifyAssembly (match refMeta with Some m -> m | _ -> WebSharper.Core.Metadata.Info.Empty) 
            (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap assem
            
    if config.PrintJS then
        match js with 
        | Some js ->
            printfn "%s" js
        | _ -> ()

    assem.Write (config.KeyFile |> Option.map readStrongNameKeyPair) config.AssemblyFile

    TimedStage "Writing resources into assembly"

    match config.ProjectType with
    | Some Bundle ->
        ExecuteCommands.Bundle config |> ignore
    | Some Html ->
        ExecuteCommands.Html config |> ignore
    | Some Website ->
        ExecuteCommands.Unpack config |> ignore
    | _ when Option.isSome config.OutputDir ->
        ExecuteCommands.Unpack config |> ignore
    | _ -> ()

let compileMain argv =

    match List.ofArray argv with
    | Cmd BundleCommand.Instance r -> r 
    | Cmd HtmlCommand.Instance r -> r
    | Cmd UnpackCommand.Instance r -> r
    | _ ->

    let wsArgs = ref WsConfig.Empty
    let refs = ResizeArray()
    let resources = ResizeArray()
    let cscArgs = ResizeArray()

    let cArgv =
        [|
            let isRNext = ref false
            for a in argv do
                match a with
                | "-r" ->
                    isRNext := true
                | _ ->
                    if !isRNext then
                        isRNext := false   
                        yield "-r:" + a
                    else
                        yield a
        |]

    for a in cArgv do
        let setProjectType t =
            match (!wsArgs).ProjectType with
            | None -> wsArgs := { !wsArgs with ProjectType = Some t }
            | _ -> failwith "Conflicting WebSharper project types set."
        match a with
        | "--jsmap" -> wsArgs := { !wsArgs with SourceMap = true } 
        | "--dts" -> wsArgs := { !wsArgs with TypeScript = true } 
        | "--wig" -> setProjectType WIG
        | "--bundle" -> setProjectType Bundle
        | "--html" -> setProjectType Html
        | "--site" -> setProjectType Website
        | "--wswarnonly" ->
            wsArgs := { !wsArgs with WarnOnly = true } 
        | StartsWith "--ws:" wsProjectType ->
            match wsProjectType.ToLower() with
            | "ignore" -> ()
            | "bundle" -> setProjectType Bundle
            | "extension" | "interfacegenerator" -> setProjectType WIG
            | "html" -> setProjectType Html
            | "library" -> ()
            | "site" | "web" | "website" | "export" -> setProjectType Website
            | _ -> invalidArg "type" ("Invalid project type: " + wsProjectType)
        | "--printjs" -> wsArgs := { !wsArgs with PrintJS = true }
        | "--vserrors" ->
            wsArgs := { !wsArgs with VSStyleErrors = true }
            cscArgs.Add a
        | StartsWith "--wsoutput:" o ->
            wsArgs := { !wsArgs with OutputDir = Some o }
        | StartsWith "--project:" p ->
            wsArgs := { !wsArgs with ProjectFile = Path.Combine(Directory.GetCurrentDirectory(), p) }
        | "/debug" | "/debug+" | "/debug:full" ->
            wsArgs := { !wsArgs with IsDebug = true }
            cscArgs.Add a
        | StartsWith "/doc:" d ->
            wsArgs := { !wsArgs with Documentation = Some d }
            cscArgs.Add a
        | StartsWith "/analyzer:" _ ->
            ()
        | StartsWith "/out:" o ->
            wsArgs := { !wsArgs with AssemblyFile = o }
            cscArgs.Add a
        | StartsWith "/reference:" r ->
            refs.Add r
            cscArgs.Add a
        | StartsWith "/resource:" r ->
            resources.Add r
            cscArgs.Add a
        | StartsWith "/keyfile:" k ->
            wsArgs := { !wsArgs with KeyFile = Some k }
        | _ -> 
            cscArgs.Add a  
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = cscArgs.ToArray() 
            VSStyleErrors = true
        }

    try
        Compile !wsArgs
        0
    with e ->
        let intermediaryOutput = (!wsArgs).AssemblyFile
        if File.Exists intermediaryOutput then 
            let failedOutput = intermediaryOutput + ".failed"
            if File.Exists failedOutput then File.Delete failedOutput
            File.Move (intermediaryOutput, failedOutput)

        sprintf "Global error '%s' at %s" e.Message e.StackTrace
        |> WebSharper.Compiler.ErrorPrinting.NormalizeErrorString
        |> eprintf "WebSharper error: %s" 
        1

[<EntryPoint>]
let main argv =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    compileMain argv