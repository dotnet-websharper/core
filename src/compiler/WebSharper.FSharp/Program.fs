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

module WebSharper.FSharp.Program

open System
open System.IO
open System.Reflection
open WebSharper
open WebSharper.Compiler

open WebSharper.Compile.CommandTools

module FE = WebSharper.Compiler.FrontEnd

let logf x = 
    Printf.kprintf ignore x

open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

let Compile (config : WsConfig) =
    let started = System.DateTime.Now
    let errors, exitCode = SimpleSourceCodeServices().Compile(config.CompilerArgs)

    if exitCode <> 0 then 
        WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(ignore).PrintErrors(errors, config.ProjectFile)

        exitCode
    else
    
    let logf x =
        if config.VSStyleErrors then logf x else Printf.kprintf System.Console.WriteLine x

    let ended = System.DateTime.Now
    logf "F# compilation: %A" (ended - started)
    let startedWS = ended 
    let started = ended 
    
    if config.AssemblyFile = null then
        failwith "You must provide assembly output path."

    if not (File.Exists config.AssemblyFile) then
        failwith "Output assembly not found"

    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)

    if config.ProjectType = Some WIG then  
        aR.Wrap <| fun () ->
        RunInterfaceGenerator aR (config.KeyFile |> Option.map readStrongNameKeyPair) config

        let ended = System.DateTime.Now
        logf "WIG running time: %A" (ended - started)
        0

    else    
    let loader = WebSharper.Compiler.FrontEnd.Loader.Create aR (logf "%s")
    let refs = [ for r in config.References -> loader.LoadFile(r) ]
    let refErrors = ResizeArray()
    let refMeta =
        let metas = refs |> List.choose (fun r -> 
            try FE.ReadFromAssembly FE.FullMetadata r
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

    let ended = System.DateTime.Now
    logf "Loading referenced metadata: %A" (ended - started)
    let started = ended 
    
    if refErrors.Count > 0 then
        for err in refErrors do 
            logf "WebSharper error %s" err
        1
    else

    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(logf "%s")

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

    let ended = System.DateTime.Now
    logf "WebSharper translation: %A" (ended - started)

    let mutable hasErrors = false

    if not (List.isEmpty comp.Errors) then        
        for pos, e in comp.Errors do
            match pos with
            | Some pos ->
                logf "%s (%d,%d)-(%d,%d) WebSharper error %s" 
                    pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) (e.ToString())
            | _ ->
                logf "WebSharper error %s" (e.ToString())
        if not config.WarnOnly then hasErrors <- true
        
    if hasErrors then 1 else

    let started = System.DateTime.Now
    
    let ended = System.DateTime.Now
    logf "Loading output assembly: %A" (ended - started)
    let started = ended 
    
    let jsResOpt = 
        WebSharper.Compiler.FrontEnd.CreateResources (match refMeta with Some m -> m | _ -> WebSharper.Core.Metadata.Info.Empty) 
            (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap thisName
            
    let ended = System.DateTime.Now
    logf "Packaging and serializing metadata: %A" (ended - started)
    let started = ended 

    if config.PrintJS then
        match jsResOpt with 
        | Some (js, _) ->
            printfn "%s" js
            logf "%s" js
        | _ -> ()

    match jsResOpt with
    | Some (_, res) ->
        
        let resFolder =
            let path = Path.Combine(Path.GetDirectoryName(config.AssemblyFile), "WebSharper")
            Directory.CreateDirectory(path) |> ignore
            path

        let resPaths =
            [|
                for name, content in res do
                    let p = Path.Combine(resFolder, name)
                    File.WriteAllBytes(p, content)
                    yield p
            |]

        let configWithRes =
            { config with
                CompilerArgs =
                    [| 
                        yield! config.CompilerArgs
                        for p in resPaths do
                            yield "--resource:" + p
                        match config.KeyFile with
                        | Some k -> yield "--keyfile:" + k
                        | _ -> ()
                    |]
            }

        let errors, exitCode = SimpleSourceCodeServices().Compile(configWithRes.CompilerArgs)

        if exitCode <> 0 then 
            WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(ignore).PrintErrors(errors, config.ProjectFile)
            failwith "Writing resources failed"

    | _ -> ()

    let ended = System.DateTime.Now
    logf "Writing resources: %A" (ended - started)

    logf "WebSharper compilation full: %A" (ended - startedWS)

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

    System.AppDomain.CurrentDomain.remove_AssemblyResolve(assemblyResolveHandler)
    0

let compileMain argv =

    logf "%s" Environment.CommandLine            
    logf "Started at: %A" System.DateTime.Now

    match List.ofArray argv |> List.tail with
    | Cmd BundleCommand.Instance r -> r
    | Cmd HtmlCommand.Instance r -> r
    | Cmd UnpackCommand.Instance r -> r
    | _ ->

    let wsArgs = ref WsConfig.Empty
    let refs = ResizeArray()
    let resources = ResizeArray()
    let fscArgs = ResizeArray()

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
            fscArgs.Add a
        | StartsWith "--wsoutput:" o ->
            wsArgs := { !wsArgs with OutputDir = Some o }
        | StartsWith "--project:" p ->
            wsArgs := { !wsArgs with ProjectFile = Path.Combine(Directory.GetCurrentDirectory(), p) }
        | StartsWith "--doc:" d ->
            wsArgs := { !wsArgs with Documentation = Some d }
            fscArgs.Add a
        | StartsWith "-o:" o | StartsWith "--out:" o ->
            wsArgs := { !wsArgs with AssemblyFile = o }
            fscArgs.Add a
        | StartsWith "-r:" r | StartsWith "--reference:" r ->
            refs.Add r
            fscArgs.Add a
        | "--debug" | "--debug+" | "--debug:full" | "-g" | "-g+" | "-g:full" ->
            wsArgs := { !wsArgs with IsDebug = true }
            fscArgs.Add a
        | StartsWith "--resource:" r ->
            resources.Add r
            fscArgs.Add a
        | StartsWith "--keyfile:" k ->
            wsArgs := { !wsArgs with KeyFile = Some k }
        | _ -> 
            fscArgs.Add a  
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = fscArgs.ToArray() 
        }

    let logf x =
        if (!wsArgs).VSStyleErrors then logf x else Printf.kprintf System.Console.WriteLine x
    
#if DEBUG 
    let exitCode = Compile !wsArgs
    logf "Stopped at: %A" System.DateTime.Now
    exitCode       
#else
    try 
        let exitCode = Compile !wsArgs
        logf "Stopped at: %A" System.DateTime.Now
        exitCode       
    with _ ->
        let intermediaryOutput = (!wsArgs).AssemblyFile
        if File.Exists intermediaryOutput then 
            let failedOutput = intermediaryOutput + ".failed"
            if File.Exists failedOutput then File.Delete failedOutput
            File.Move (intermediaryOutput, failedOutput)
        reraise()
#endif

open Microsoft.FSharp.Compiler.ErrorLogger

[<EntryPoint>]
let main(argv) =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parameter)    

#if DEBUG
    compileMain(Array.append [| "fsc.exe" |] argv); 
#else
    try compileMain(Array.append [| "fsc.exe" |] argv); 
    with e -> 
        errorRecovery e Microsoft.FSharp.Compiler.Range.range0; 
        1
#endif