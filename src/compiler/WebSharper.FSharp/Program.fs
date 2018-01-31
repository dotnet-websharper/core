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
open ErrorPrinting

exception ArgumentError of string
let argError msg = raise (ArgumentError msg)

open Microsoft.FSharp.Compiler.SourceCodeServices
let Compile (config : WsConfig) (warnSettings: WarnSettings) =    
    StartTimer()
    
    if config.AssemblyFile = null then
        argError "You must provide assembly output path."
    
    if config.ProjectType <> Some WIG && config.ProjectFile = null then
        argError "You must provide project file path."

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let mainProxiesFile() =
        "../../../build/" + (if config.IsDebug then "Debug" else "Release") + "/Proxies.args"    

    if thisName = "WebSharper.Main.Proxies" then 
        File.WriteAllLines(mainProxiesFile(), config.CompilerArgs)
        printfn "Written Proxies.args"
        0 
    else

    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(printfn "%s", checker)

    let isBundleOnly = config.ProjectType = Some BundleOnly
    
    let exitCode = 
        if isBundleOnly then 0 else
            let errors, exitCode = 
                checker.Compile(config.CompilerArgs) |> Async.RunSynchronously
    
            PrintFSharpErrors warnSettings errors
    
            if exitCode = 0 then 
                if not (File.Exists config.AssemblyFile) then
                    argError "Output assembly not found"

                TimedStage "F# compilation"

            exitCode
            
    if exitCode <> 0 then 
        exitCode
    else

    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            if not isBundleOnly then yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)

    if config.ProjectType = Some WIG then  
        aR.Wrap <| fun () ->
        try 
            RunInterfaceGenerator aR (config.KeyFile |> Option.map readStrongNameKeyPair) config
            TimedStage "WIG running time"
            0
        with e ->
            PrintGlobalError (sprintf "Error running WIG assembly: %s at %s" e.Message e.StackTrace)
            1
    
    else    
    let loader = Loader.Create aR (printfn "%s")
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    let refMeta =
        System.Threading.Tasks.Task.Run(fun () ->
            let mutable refError = false
            let metas = refs |> List.choose (fun r -> 
                match TryReadFromAssembly FullMetadata r with
                | None -> None
                | Some (Ok m) -> Some m
                | Some (Error e) ->
                    refError <- true
                    PrintGlobalError e
                    None
            )
            if refError then None
            elif List.isEmpty metas then Some ([], WebSharper.Core.Metadata.Info.Empty) 
            else
                try
                    Some (
                        metas,
                        { 
                            WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
                                Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
                        }
                    )
                with e ->
                    refError <- true
                    PrintGlobalError ("Error merging WebSharper metadata: " + e.Message)
                    None
        )
    
    let referencedAsmNames =
        paths
        |> Seq.map (fun i -> 
            let n = Path.GetFileNameWithoutExtension(i)
            n, i
        )
        |> Map.ofSeq

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

    let compilerArgs =
        if thisName = "WebSharper.Main" then
            printfn "Reading Proxies.args"
            File.ReadAllLines(mainProxiesFile())
        else
            config.CompilerArgs    
    
    let comp =
        compiler.Compile(refMeta, compilerArgs, config.ProjectFile, thisName)

    match comp with
    | None ->
        1
    | Some comp ->

    if not (List.isEmpty comp.Errors || config.WarnOnly) then        
        PrintWebSharperErrors config.WarnOnly config.ProjectFile comp
        1
    else
            
    let currentMeta, sources =
        if isBundleOnly then
            TransformMetaSources comp.AssemblyName (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap 
        else
            let assem = loader.LoadFile config.AssemblyFile
    
            let js, currentMeta, sources =
                ModifyAssembly (Some comp) (match refMeta.Result with Some (_, m) -> m | _ -> WebSharper.Core.Metadata.Info.Empty) 
                    (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap config.AnalyzeClosures assem

            PrintWebSharperErrors config.WarnOnly config.ProjectFile comp
            
            if config.PrintJS then
                match js with 
                | Some js ->
                    printfn "%s" js
                | _ -> ()

            assem.Write (config.KeyFile |> Option.map readStrongNameKeyPair) config.AssemblyFile

            TimedStage "Writing resources into assembly"
            currentMeta, sources

    match config.ProjectType with
    | Some Bundle 
    | Some BundleOnly ->
        // comp.Graph does not have graph of dependencies and we need full graph here for bundling
        let metas =
            match refMeta.Result with
            | Some (metas, _) -> metas
            | _ -> []
        Bundling.Bundle config metas currentMeta sources refs
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
    | Cmd HtmlCommand.Instance r -> r
    | Cmd UnpackCommand.Instance r -> r
    | _ ->

    let wsArgs = ref WsConfig.Empty
    let warn = ref WarnSettings.Default
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

    let parseIntSet (s: string) = s.Split(',') |> Seq.map int |> Set
    
    for a in cArgv do
        let setProjectType t =
            match (!wsArgs).ProjectType with
            | None -> wsArgs := { !wsArgs with ProjectType = Some t }
            | _ -> argError "Conflicting WebSharper project types set."
        try
            match a with
            | "--jsmap" -> wsArgs := { !wsArgs with SourceMap = true } 
            | "--dts" -> wsArgs := { !wsArgs with TypeScript = true } 
            | "--wig" -> setProjectType WIG
            | "--bundle" -> setProjectType Bundle
            | "--bundleonly" -> setProjectType BundleOnly
            | "--html" -> setProjectType Html
            | "--site" -> setProjectType Website
            | "--wswarnonly" -> wsArgs := { !wsArgs with WarnOnly = true } 
            | "--dce-" -> wsArgs := { !wsArgs with DeadCodeElimination = false } 
            | StartsWith "--ws:" wsProjectType ->
                match wsProjectType.ToLower() with
                | "ignore" -> ()
                | "bundle" -> setProjectType Bundle
                | "bundleonly" -> setProjectType BundleOnly
                | "extension" | "interfacegenerator" -> setProjectType WIG
                | "html" -> setProjectType Html
                | "library" -> ()
                | "site" | "web" | "website" | "export" -> setProjectType Website
                | _ -> invalidArg "type" ("Invalid project type: " + wsProjectType)
            | "--dlres" -> wsArgs := { !wsArgs with DownloadResources = true }
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
            | StartsWith "--nowarn:" w ->
                warn := { !warn with NoWarn = (!warn).NoWarn + parseIntSet w }
            | StartsWith "--warn:" l ->
                warn := { !warn with WarnLevel = int l }
            | StartsWith "--warnon:" w ->
                warn := { !warn with NoWarn = (!warn).NoWarn - parseIntSet w }
            | "--warnaserror+" ->
                warn := { !warn with AllWarnAsError = true }
            | "--warnaserror-" ->
                warn := { !warn with AllWarnAsError = false }
            | StartsWith "--warnaserror:" w | StartsWith "--warnaserror+:" w ->
                warn := { !warn with WarnAsError = (!warn).WarnAsError + parseIntSet w }
            | StartsWith "--warnaserror-:" w ->
                warn := { !warn with DontWarnAsError = (!warn).DontWarnAsError + parseIntSet w }
            | StartsWith "--closures:" c ->
                match c.ToLower() with
                | "true" ->
                    wsArgs := { !wsArgs with AnalyzeClosures = Some false }
                | "movetotop" ->
                    wsArgs := { !wsArgs with AnalyzeClosures = Some true }
                | _ ->
                    printfn "--closures:%s argument unrecognized, value must be true or movetotop" c
            | StartsWith "--preferreduilang:" _ ->
                () // not handled by FSC 16.0.2
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

    let clearOutput() =
        try
            let intermediaryOutput = (!wsArgs).AssemblyFile
            if File.Exists intermediaryOutput then 
                let failedOutput = intermediaryOutput + ".failed"
                if File.Exists failedOutput then File.Delete failedOutput
                File.Move (intermediaryOutput, failedOutput)
        with _ ->
            PrintGlobalError "Failed to clean intermediate output!"

    try 
        let exitCode = Compile !wsArgs !warn
        if exitCode <> 0 then clearOutput()
        exitCode            
    with _ ->
        clearOutput()
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
    with 
    | ArgumentError msg -> 
        PrintGlobalError msg
        1    
    | e -> 
        PrintGlobalError (sprintf "Global error '%s' at %s" e.Message e.StackTrace)
        1
#endif
