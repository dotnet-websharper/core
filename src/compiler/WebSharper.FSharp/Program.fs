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
open WebSharper.Compiler.FrontEnd
open System.Diagnostics

open Microsoft.FSharp.Compiler.SourceCodeServices
let Compile (config : WsConfig) =    
    StartTimer()
    
    if config.AssemblyFile = null then
        failwith "You must provide assembly output path."

    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(printfn "%s", checker)

    let errors, exitCode = checker.Compile(config.CompilerArgs)
    
    if exitCode <> 0 then 
        compiler.PrintErrors(errors, config.ProjectFile)
        failwith "F# compilation error"
    if not (File.Exists config.AssemblyFile) then
        failwith "Output assembly not found"

    TimedStage "F# compilation"
            
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

        TimedStage "WIG running time"
        0
    
    else    
    let loader = Loader.Create aR (printfn "%s")
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    let refMeta =
        System.Threading.Tasks.Task.Run(fun () ->
            let mutable refErrors = false
            let metas = refs |> List.choose (fun r -> 
                try ReadFromAssembly FullMetadata r
                with e ->
                    eprintfn "WebSharper error %s" e.Message
                    None
            )
            if refErrors then None
            elif List.isEmpty metas then Some WebSharper.Core.Metadata.Info.Empty 
            else
                try
                    Some { 
                        WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
                            Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
                    }
                with e ->
                    eprintfn "WebSharper error Error merging WebSharper metadata: %s" e.Message
                    None
        )
    
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
        
    if hasErrors then 1 else
    
    let assem = loader.LoadFile config.AssemblyFile
    let js =
        ModifyAssembly (match refMeta.Result with Some m -> m | _ -> WebSharper.Core.Metadata.Info.Empty) 
            (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap assem
            
    if config.PrintJS then
        match js with 
        | Some js ->
            printfn "%s" js
        | _ -> ()

    assem.Write (config.KeyFile |> Option.map readStrongNameKeyPair) config.AssemblyFile

    TimedStage "Writing resources into assembly"

    compiler.PrintWarnings(comp, config.ProjectFile)

    match config.ProjectType with
    | Some Bundle ->
        ExecuteCommands.Bundle config |> ignore
        TimedStage "Bundling"
    | Some Html ->
        ExecuteCommands.Html config |> ignore
        TimedStage "Writing offline sitelets"
    | Some Website ->
        ExecuteCommands.Unpack config |> ignore
        TimedStage "Unpacking"
    | _ when Option.isSome config.OutputDir ->
        ExecuteCommands.Unpack config |> ignore
        TimedStage "Unpacking"
    | _ -> ()

    0

let compileMain argv =

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
        try
            match a with
            | "--jsmap" -> wsArgs := { !wsArgs with SourceMap = true } 
            | "--dts" -> wsArgs := { !wsArgs with TypeScript = true } 
            | "--wig" -> setProjectType WIG
            | "--bundle" -> setProjectType Bundle
            | "--html" -> setProjectType Html
            | "--site" -> setProjectType Website
            | "--wswarnonly" -> wsArgs := { !wsArgs with WarnOnly = true } 
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
        with e ->
            failwithf "Parsing argument failed: '%s' - %s" a e.Message
    fscArgs.Add "--define:FSHARP41"
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = fscArgs.ToArray() 
        }

    try 
        Compile !wsArgs
    with _ ->
        let intermediaryOutput = (!wsArgs).AssemblyFile
        if File.Exists intermediaryOutput then 
            let failedOutput = intermediaryOutput + ".failed"
            if File.Exists failedOutput then File.Delete failedOutput
            File.Move (intermediaryOutput, failedOutput)
        reraise()

let formatArgv (argv: string[]) =
    match argv with
    | [| a |] when a.StartsWith "@" -> File.ReadAllLines a.[1..]
    | _ -> argv
    |> Array.append [| "fsc.exe" |]

[<EntryPoint>]
let main(argv) =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    
#if DEBUG
    compileMain (formatArgv argv)
#else
    try compileMain (formatArgv argv)
    with e -> 
        sprintf "Global error '%s' at %s" e.Message e.StackTrace
        |> WebSharper.Compiler.ErrorPrinting.NormalizeErrorString
        |> eprintf "WebSharper error %s" 
        1
#endif
