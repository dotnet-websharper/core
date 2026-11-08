// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

module WebSharper.Compiler.CSharp.Compile

open System
open System.IO
open System.Reflection
open WebSharper.Compiler

open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
open WebSharper.Compiler.CSharp.ErrorPrinting

module C = WebSharper.Compiler.Commands

let fullTime (logger: LoggerBase) =
    logger.ExitContext()
    logger.TimedStage "Total compilation time"

let Compile config (logger: LoggerBase) tryGetMetadata =
    logger.ProjectFile <- config.ProjectFile
    logger.TimingLogPath <- defaultArg config.TimingLog ""
    config.ArgWarnings |> List.iter (PrintGlobalWarning logger)
    FileSystem.OutputFiles.Start()
    SidecarWebResources.Install()

    if config.AssemblyFile = null then
        argError "You must provide assembly output path."

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let isBundleOnly = config.ProjectType = Some BundleOnly
    
    if not (isBundleOnly || File.Exists config.AssemblyFile) then 
        logger.Out "Assembly file does not exist"
    else

    logger.TimedStage "Reading configuration"
    logger.EnterContext()

    let mergeDirs =
        match config.ChangeTracking, config.OutputDir with
        | true, Some outputDir ->
            let projDir = Path.GetDirectoryName config.ProjectFile
            let baseline = Path.Combine(projDir, ".websharper/baseline")
            let outputDir = 
                match config.ProjectType with
                | Some Website ->
                    Path.Combine(outputDir, "Scripts", "WebSharper")
                | _ ->
                    outputDir
            let modified = Path.Combine(projDir, ".websharper/modified")
            let conflict = Path.Combine(projDir, ".websharper/conflict")
            let exitCode = Merging.finishMerge logger baseline outputDir modified conflict
            if exitCode <> 0 then
                argError "" // exits without printing more errors   
            Merging.prepareForBuild logger baseline outputDir modified conflict
            logger.TimedStage "Making copy of output folder for merging"
            Some (baseline, outputDir, modified, conflict)
        | _ ->
            None
    
    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            if not isBundleOnly then yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)

    let loader = Loader.Create aR logger.Error
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    logger.TimedStage "Loading referenced assemblies"
    let mutable refError = false
    let refReadOptions = config.ReferenceReadOptions
    let wsRefs, metas =
        refs |> List.choose (fun r -> 
            match tryGetMetadata refReadOptions r with
            | None -> None
            | Some (Ok m) -> Some (r, m)
            | Some (Error e) ->
                refError <- true
                PrintGlobalError logger e
                None
        ) |> List.unzip
    logger.TimedStage "Reading referenced metadata"
    let refMeta =
        if refError then None
        elif List.isEmpty metas then Some WebSharper.Core.Metadata.Info.Empty 
        else 
            try
                Some (WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas)
            with e ->
                refError <- true
                PrintGlobalError logger (sprintf "Error merging WebSharper metadata: %A" e)
                None

    logger.TimedStage "Merging referenced metadata"

    match refMeta with
    | None ->
        argError "" // exits without printing more errors    
    | Some refMeta ->

    let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler()

    if config.ProjectFile = null then
        argError "You must provide project file path."
    
    let assem = if isBundleOnly then None else Some (loader.LoadFile config.AssemblyFile)
    logger.TimedStage "Loading output assembly"

#if !DEBUG
    // WebSharperCompile also runs when csc did not: skip if the assembly was not written since
    // the last WebSharper compilation finished (its output list is written last), and the
    // sidecar belongs to it. A csc run rewrites the assembly, also when only reference stamps
    // changed and the deterministic output has the same module id.
    let alreadyCompiled (a: Assembly) =
        let listFile = Path.ChangeExtension(config.AssemblyFile, ".websharper.files")
        a.HasRuntimeMetadata
        || (a.Sidecar |> Option.exists (fun sc -> sc.ModuleId = a.ModuleId)
            && File.Exists listFile
            && File.GetLastWriteTimeUtc listFile >= File.GetLastWriteTimeUtc config.AssemblyFile)
    // --wstranslateonly: only WebSharper inputs of a library changed (.websharper.wsdeps)
    if assem.IsSome && not config.TranslateOnly && alreadyCompiled assem.Value then
        logger.TimedStage "WebSharper resources already exist, skipping"
        fullTime logger
    else
#endif

    let comp =
        aR.Wrap <| fun () ->
            compiler.Compile(refMeta, config, logger)
    
    if not (List.isEmpty comp.Errors || config.WarnOnly) then        
        fullTime logger
        PrintWebSharperErrors logger config.WarnOnly comp
        argError "" // exits without printing more errors
    else

    // web worker bundles need the code of references, read again if only the library parts were read
    let getExtraBundleRefMetas() =
        if comp.CompiledExtraBundles.Count = 0 then []
        elif refReadOptions = WebSharper.Core.MetadataIO.ReadOptions.Full then metas
        else
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let full =
                wsRefs |> List.choose (fun r ->
                    match tryGetMetadata WebSharper.Core.MetadataIO.ReadOptions.Full r with
                    | Some (Ok m) -> Some m
                    | Some (Error e) -> failwith e
                    | None -> None
                )
            logger.TimedStageElapsed("Reading full referenced metadata for extra bundles", sw.Elapsed)
            full

    let js, currentMeta, runtimeMeta, sources, extraBundles =
        let currentMeta = comp.ToCurrentMetadata(config.WarnOnly)
        if isBundleOnly then
            let currentMeta, sources = TransformMetaSources comp.AssemblyName currentMeta config.SourceMap 
            let extraBundles = 
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger (getExtraBundleRefMetas()) currentMeta refs comp comp.AssemblyName
            try
                HandleExtraFiles logger false comp.AssemblyName config.ProjectFile config.OutputDir true |> ignore
            with e ->
                PrintGlobalError logger (sprintf "Error processing extra.files: %A" e)
            WriteDependencies config.AssemblyFile wsRefs WebSharper.Core.MetadataIO.ReadOptions.Full (ExecutedReferences config.References aR.LoadedFiles) [] false
            None, currentMeta, currentMeta, sources, extraBundles
        else
            let assem = assem.Value

            // website, offline and service projects: the assembly is emitted again with runtime
            // metadata, compiling the code in parallel with creating the output, the runtime
            // metadata is only needed for writing it
            let runtimeMetaReady = System.Threading.Tasks.TaskCompletionSource<byte[]>()
            let emitTask =
                if config.EmbedsRuntimeMetadata then
                    Some (System.Threading.Tasks.Task.Run(fun () ->
                        WebSharper.Compiler.CSharp.Emit.EmitWithResources
                            compiler.CommandLineArguments compiler.RoslynCompilation config.AssemblyFile
                            [ WebSharper.Constants.EmbeddedResources.EMBEDDED_RUNTIME_METADATA, fun () -> runtimeMetaReady.Task.Result ]
                    ))
                else None

            let extraBundles = 
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger (getExtraBundleRefMetas()) currentMeta refs comp comp.AssemblyName

            // extra.files of libraries and web worker bundles go into the sidecar file
            let extraFiles =
                let embed =
                    match config.ProjectType with
                    | Some (Bundle | Website | Html) -> false
                    | _ -> true
                try
                    HandleExtraFiles logger embed comp.AssemblyName config.ProjectFile config.OutputDir (config.ProjectType = Some Bundle)
                with e ->
                    PrintGlobalError logger (sprintf "Error processing extra.files: %A" e)
                    [||]
            let webResources = Array.append extraFiles (Bundling.ExtraBundleWebResources extraBundles)

            let runtimeMeta =
                match config.ProjectType with
                | Some (Bundle | Website | Html | Service) -> Some (config.RuntimeMetadata, metas)
                | _ when config.DeadCodeElimination = Some true -> Some (config.RuntimeMetadata, metas)
                | _ -> None

            let isLibrary = config.ProjectType = None 

            let isSitelet =
                match config.ProjectType with
                | Some Service
                | Some Html ->
                    true
                | Some Website
                | _ when Option.isSome config.OutputDir ->
                    true
                | _ -> 
                    false

            let prebundle =
                match config.ProjectType with
                | Some Html ->
                    true
                | _ ->
                    config.PreBundle

            let dce =
                config.DeadCodeElimination |> Option.defaultValue (
                    match config.ProjectType with
                    | Some Bundle -> true
                    | _ -> false
                )

            let mode =
                if isSitelet then
                    if prebundle then
                        CreateResourcesMode.ProdSitelet(config.RuntimeMetadata, metas, refs)
                    else
                        CreateResourcesMode.DebugSitelet(refMeta, config.RuntimeMetadata, metas)
                elif isLibrary then
                    CreateResourcesMode.Library(refMeta)
                else
                    CreateResourcesMode.Other(refMeta)

            let res, runtimeMetaBytes, sidecarEntries =
                CreateOutputFor {
                    Logger = logger
                    Compilation = Some comp
                    CurrentMetadata = currentMeta
                    SourceMap = config.SourceMap
                    TypeScriptDeclaration = config.TypeScriptDeclaration
                    TypeScript = config.TypeScriptOutput
                    DeadCodeElimination = dce
                    AnalyzeClosures = config.AnalyzeClosures
                    Mode = mode
                    WebResources = webResources
                } (Some assem) comp.AssemblyName

            match config.ProjectType, config.DeadCodeElimination, config.OutputDir with
            | None, Some true, Some outputDir ->
                UnpackLibraryCode logger (Some comp) refMeta currentMeta config.TypeScriptDeclaration config.TypeScriptOutput runtimeMeta outputDir
            | _ -> ()

            PrintWebSharperErrors logger config.WarnOnly comp

            if config.PrintJS then
                for (fname, js) in res.JSFiles do
                    logger.Out("// " + fname)
                    logger.Out(js)

            logger.TimedStage "Printing WebSharper errors and warnings"

            // the assembly is left as csc wrote it, except for website, bundle, offline and
            // service projects, emitted again with runtime metadata, and Proxy projects
            let moduleId =
                match runtimeMetaBytes with
                | Some bytes when emitTask.IsSome ->
                    runtimeMetaReady.SetResult bytes
                    let ranGenerators = emitTask.Value.GetAwaiter().GetResult()
                    logger.TimedStage (if ranGenerators then "Emitting assembly with runtime metadata (with source generators)" else "Emitting assembly with runtime metadata (join)")
                    ReadModuleId config.AssemblyFile
                | Some _ ->
                    failwith "Unexpected runtime metadata"
                | None when emitTask.IsSome ->
                    runtimeMetaReady.SetException(exn "No runtime metadata")
                    failwith "Expected runtime metadata to embed into the assembly"
                | None when config.ProjectType = Some Proxy ->
                    WriteProxyAssembly config.AssemblyFile config.KeyFile
                    ReplaceReferenceAssembly logger config.ProjectDir config.CompilerArgs config.AssemblyFile
                    logger.TimedStage "Writing attributes-only assembly for Proxy project"
                    ReadModuleId config.AssemblyFile
                | None ->
                    assem.ModuleId

            WriteSidecar config.AssemblyFile moduleId sidecarEntries
            WriteDependencies config.AssemblyFile wsRefs (if comp.CompiledExtraBundles.Count > 0 then WebSharper.Core.MetadataIO.ReadOptions.Full else refReadOptions) (ExecutedReferences config.References aR.LoadedFiles) [] config.CanTranslateOnly
            logger.TimedStage "Writing sidecar file"
            Some res.JSFiles, currentMeta, res.RuntimeMetadata |> Option.defaultValue currentMeta, res.Sources, extraBundles

    match config.JSOutputPath, js with
    | Some path, Some jss ->
        let asmPath = Path.Combine(path, thisName)
        Directory.CreateDirectory(asmPath) |> ignore
        for (fname, js) in jss do
            let jsPath = Path.Combine(asmPath, fname)
            let fullPath = Path.Combine(Path.GetDirectoryName config.ProjectFile, jsPath)
            File.WriteAllText(fullPath, js)
            FileSystem.OutputFiles.Add fullPath
        logger.TimedStage ("Writing code files to " + asmPath)
    | _ -> ()

    // TODO minimized output
    //match config.MinJSOutputPath, js with
    //| Some path, Some (_, minjs) ->
    //    File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), minjs)
    //    logger.TimedStage ("Writing " + path)
    //| _ -> ()

    let handleCommandResult stageName cmdRes =  
        let res =
            match cmdRes with
            | C.Ok -> 0
            | C.Errors errors ->
                if config.WarnOnly || config.DownloadResources = Some false then
                    errors |> List.iter (PrintGlobalWarning logger)
                    0
                else
                    errors |> List.iter (PrintGlobalError logger)
                    1
        logger.ExitContext()
        logger.TimedStage stageName
        if res = 1 then argError "" // exits without printing more errors    

    let unpack() =
        match ExecuteCommands.GetWebRoot config with
        | Some webRoot ->
            ExecuteCommands.Unpack webRoot config loader logger |> handleCommandResult "Unpacking"
        | None ->
            PrintGlobalError logger "Failed to unpack website project, no WebSharperOutputDir specified"

    match config.ProjectType with
    | Some (Bundle | BundleOnly) ->
        aR.Wrap <| fun () ->
            Bundling.Bundle config logger metas currentMeta comp sources refs extraBundles
        logger.TimedStage "Bundling"
    | Some Html ->
        logger.Out "Start writing offline sitelet"
        logger.EnterContext()
        let htmlRes = ExecuteCommands.Html config runtimeMeta logger 
        match htmlRes with
        | C.Ok -> 
            htmlRes |> handleCommandResult "Writing offline sitelet"
            unpack()
        | _ ->
            htmlRes |> handleCommandResult "Writing offline sitelet"
    | Some Website ->
        unpack()
    | _ when Option.isSome config.OutputDir && config.DeadCodeElimination <> Some true ->
        unpack()
    | _ -> ()

    match mergeDirs with 
    | Some (baseline, outputDir, modified, conflict) ->
        let exitCode = Merging.doMerge logger baseline outputDir modified conflict
        if exitCode <> 0 then
            argError "" // exits without printing more errors   
        logger.TimedStage "Merging output with previously existing changes"
    | None -> ()

    FileSystem.OutputFiles.Finish config.AssemblyFile
    fullTime logger

let compileMain (argv: string[]) tryGetMetadata (logger: LoggerBase) =

    match HandleDefaultArgsAndCommands logger argv false with
    | Some r -> r
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
        match RecognizeWebSharperArg a !wsArgs with
        | Some na -> wsArgs := na
        | _ ->
        match a with
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
            match r.Split(',') with 
            | [| res |] -> resources.Add (res, None)
            | [| res; fullName |] -> resources.Add (res, Some fullName)
            | _ -> argError ("Unexpected value /resource:" + r)
            cscArgs.Add a
        | StartsWith "/keyfile:" k ->
            wsArgs := { !wsArgs with KeyFile = Some k }
        | StartsWith "--cscrsp:" r ->
            wsArgs := { !wsArgs with CompilerResponseFile = Some r }
        | _ -> 
            cscArgs.Add a
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = cscArgs.ToArray() 
        }
    wsArgs := SetDefaults false !wsArgs

    if (!wsArgs).UseJavaScriptSymbol |> Option.exists id then
        let cArgs = (!wsArgs).CompilerArgs
        if cArgs |> Array.contains "-define:JAVASCRIPT" |> not then
            wsArgs := 
                { !wsArgs with 
                    CompilerArgs = Array.append cArgs [|"-define:JAVASCRIPT"|]
                }

    try
        Compile !wsArgs logger tryGetMetadata
        0
    with _ ->
        let intermediaryOutput = (!wsArgs).AssemblyFile
        if File.Exists intermediaryOutput then 
            let failedOutput = intermediaryOutput + ".failed"
            if File.Exists failedOutput then File.Delete failedOutput
            File.Move (intermediaryOutput, failedOutput)
        reraise()
