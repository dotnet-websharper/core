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

module WebSharper.Compiler.FSharp.Compile

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open WebSharper.Compiler

open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
module C = WebSharper.Compiler.Commands
open WebSharper.Compiler.FSharp.ErrorPrinting
open FSharp.Compiler.CodeAnalysis

let clearOutput config logger =
    try
        let intermediaryOutput = config.AssemblyFile
        if File.Exists intermediaryOutput then 
            let failedOutput = intermediaryOutput + ".failed"
            if File.Exists failedOutput then File.Delete failedOutput
            File.Move (intermediaryOutput, failedOutput)
    with _ ->
        PrintGlobalError logger "Failed to clean intermediate output!"

let createAssemblyResolver (config : WsConfig) includeCurrent =
    let compilerDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            if includeCurrent then
                yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)
            .SearchDirectories([compilerDir])
    aR

let handleCommandResult logger config warnSettings stageName exitContext cmdRes =  
    let res =
        match cmdRes with
        | C.Ok -> 0
        | C.Errors errors ->
            if config.WarnOnly || config.DownloadResources = Some false then
                errors |> List.iter (PrintGlobalWarning warnSettings logger)
                0
            else
                errors |> List.iter (PrintGlobalError logger)
                1
    if exitContext then
        logger.ExitContext()
    logger.TimedStage stageName
    res

let RunFSharpSourceGeneration (logger: LoggerBase) (config : WsConfig) =
    let sourceFiles = config.CompilerArgs[1 ..] |> Array.filter (fun a -> not (a.StartsWith "-"))
    let isSupportedFile (f: string) =
        f.EndsWith ".fs" || f.EndsWith ".fsi" || f.EndsWith ".fsx" || f.EndsWith ".fsscript" || f.EndsWith ".ml" || f.EndsWith ".mli"
    let unsupportedFiles = sourceFiles |> Array.filter (fun f -> not (isSupportedFile f))
    if unsupportedFiles.Length > 0 then
        let aR = createAssemblyResolver config false    
        aR.Wrap <| fun () ->
            let generatedFiles = ResizeArray()
            let keepUntouched = ResizeArray()
            let generatedPaths = HashSet()
            let mutable hasError = false
            let mutable needsUpdate = false
            let printError msg =
                PrintGlobalError logger msg
                hasError <- true
            let propsFile = Path.Combine(config.ProjectDir, $"obj/{Path.GetFileName(config.ProjectFile)}.websharper.props")
            let propsFileOpt, propsFileTimeStamp =
                if File.Exists propsFile then
                    try
                        let propsFileLines = File.ReadAllLines propsFile
                        propsFileLines
                        |> Seq.pairwise
                        |> Seq.choose (fun (line1, line2) ->
                            if line1.StartsWith("    <Compile Include=\"") 
                                || line2.StartsWith("      <DependentUpon>") 
                            then
                                Some (line2[21..line2.Length - 17], line1[22 .. line1.Length - 3])
                            else
                                None
                        )
                        |> Seq.groupBy fst
                        |> Seq.map (fun (filePath, dependents) ->
                            filePath, 
                            dependents |> Seq.map snd |> Array.ofSeq
                        )
                        |> Array.ofSeq
                        |> Some
                        , File.GetLastWriteTime propsFile
                    with e -> 
                        logger.Out $"Error during reading props file: {e.Message}"
                        None, DateTime.MinValue
                else
                    logger.Out $"Props file does not exist: {propsFile}"
                    None, DateTime.MinValue
            let transformedFiles =  
                let generators = System.Collections.Generic.Dictionary()
                sourceFiles |> Array.collect (fun f ->
                    if isSupportedFile f then
                        [| f |]
                    else
                        let ext = (Path.GetExtension f).TrimStart('.')
                        // search for generator type in references
                        let generator =
                            match generators.TryGetValue ext with
                            | true, g -> g
                            | _ ->
                                let genInfo =
                                    config.References |> Array.tryPick (fun r ->
                                        let asm = Mono.Cecil.AssemblyDefinition.ReadAssembly(r)
                                        
                                        asm.CustomAttributes |> Seq.tryPick (fun a -> 
                                            if a.AttributeType.FullName = "WebSharper.FSharpSourceGeneratorAttribute" && string a.ConstructorArguments[0].Value = ext then
                                                Some (asm.FullName, a.ConstructorArguments[1].Value :?> Mono.Cecil.TypeReference)
                                            else
                                                None
                                        )                                        
                                    )
                                if Option.isNone genInfo then
                                    printError (sprintf "No generator found for extension '%s', needs an assembly-level FSharpSourceGenerator attribute" ext)
                                let g =
                                    match genInfo with 
                                    | Some (asmName, gen) ->
                                        // load generator type with Reflection
                                        let genInstance = 
                                            let fqn = gen.FullName.Replace('/', '+') + ", " + asmName
                                            try
                                                let genType = 
                                                    Type.GetType(fqn, true)
                                                Some (Activator.CreateInstance(genType))
                                            with
                                            | :? FileLoadException as e ->
                                                printError (sprintf "Failed to create generator instance for extension '%s', type '%s': %s" ext fqn e.InnerException.Message)
                                                None
                                            | :? TargetInvocationException as e ->
                                                printError (sprintf "Failed to create generator instance for extension '%s', type '%s': %s at %s" ext fqn e.InnerException.Message e.InnerException.StackTrace)
                                                None
                                            | e -> 
                                                printError (sprintf "Failed to create generator instance for extension '%s', type '%s': %s" ext fqn e.Message)
                                                None
                                        match genInstance with
                                        | Some (:? WebSharper.ISourceGenerator as ig) -> Some ig
                                        | Some _ ->
                                            printError (sprintf "Generator type for extension '%s' must implement WebSharper.ISourceGenerator" ext)
                                            None
                                        | None -> None
                                    | None -> None
                                generators[ext] <- g
                                g
                        match generator with
                        | Some gen ->
                            let fullPath = Path.Combine(config.ProjectDir, f)
                            let prevOutputOpt =
                                propsFileOpt |> Option.bind (fun propsFile ->
                                    propsFile
                                    |> Array.tryPick (fun (pf, pOutputs) ->
                                        if pf = f then Some pOutputs else None
                                    )     
                                    |> Option.map (fun outputs ->
                                        propsFileTimeStamp, outputs
                                    )
                                )    
                            let generateInput =
                                {
                                    RelativeFilePath = f
                                    FilePath = fullPath
                                    ProjectFilePath = config.ProjectFile
                                    Print = logger.Out
                                    PrintError = logger.Error
                                    PreviousOutputFiles = prevOutputOpt
                                } : WebSharper.GenerateCall
                            try
                                match gen.Generate generateInput with
                                | Some genRes ->
                                    for genFile in genRes do
                                        let genFileRel =
                                            if Path.IsPathRooted genFile then
                                                Merging.getRelativePath config.ProjectDir genFile
                                            else
                                                genFile
                                        if not (generatedPaths.Add genFile) then
                                            printError (sprintf "Duplicated output while generating F# source from input file '%s'" f)
                                        generatedFiles.Add (f, genFileRel)
                                    needsUpdate <- true
                                    genRes
                                | None ->
                                    match prevOutputOpt with
                                    | None ->
                                        printError (sprintf "There are no cached outputs and F# source generator have not returned outputs for input file '%s'" f)
                                        [||]
                                    | Some (_, prevOutput) ->
                                        for o in prevOutput do
                                            generatedFiles.Add (f, o)   
                                        prevOutput
                            with e ->
                                printError (sprintf "Error while generating F# source from input file '%s': %s at %s" f e.Message e.StackTrace)
                                [||]
                        | None ->
                            [||]                
                )
            if not hasError && needsUpdate then
                let propsFileLines =
                    seq {
                        """<?xml version="1.0" encoding="utf-8" standalone="no"?>"""
                        """<Project ToolsVersion="14.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">"""
                        """  <ItemGroup Condition=" '$(DesignTimeBuild)' == 'true' ">"""
                        for (f, genFileRel) in generatedFiles do
                            $"""    <Compile Include="{genFileRel}">"""
                            $"""      <DependentUpon>{f}</DependentUpon>"""
                            """      <WebSharperGenerated>true</WebSharperGenerated>"""
                            """    </Compile>"""
                        """  </ItemGroup>"""
                        """</Project>"""
                    }
                File.WriteAllLines(propsFile, propsFileLines)
            let otherArgs = config.CompilerArgs[1 ..] |> Array.filter (fun a -> a.StartsWith "-")
            { config with CompilerArgs = Array.concat [| [| config.CompilerArgs[0] |]; otherArgs;  transformedFiles |] }
    else
        config

let Compile (config : WsConfig) (warnSettings: WarnSettings) (logger: LoggerBase) (checkerFactory: unit -> FSharpChecker) (tryGetMetadata: Assembly -> Result<WebSharper.Core.Metadata.Info, string> option) =    
    config.ArgWarnings |> List.iter (PrintGlobalWarning warnSettings logger)
    
    if config.AssemblyFile = null then
        argError "You must provide assembly output path."
    
    if config.ProjectType <> Some WIG && config.ProjectFile = null then
        argError "You must provide project file path."

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let mainProxiesFile() =
        "../../../build/" + (if config.IsDebug then "Debug" else "Release") + "/Proxies.args"    

    if thisName = "WebSharper.StdLib.Proxies" then
        let config =
            { config with
                References =
                    config.References
                    |> Array.filter (fun r -> not (r.EndsWith "WebSharper.StdLib.Proxies.dll"))
            }
        let mainProxiesFile = mainProxiesFile()
        Directory.CreateDirectory(Path.GetDirectoryName(mainProxiesFile)) |> ignore
        let fixedArgs =
            config.CompilerArgs 
            |> Array.map (fun s -> 
                s.Replace(@"net10.0\.NETCoreApp,Version=v10.0.AssemblyAttributes.fs", 
                    @"netstandard2.0\.NETStandard,Version=v2.0.AssemblyAttributes.fs"
                    ).Replace(@"net10.0/.NETCoreApp,Version=v10.0.AssemblyAttributes.fs", 
                        @"netstandard2.0/.NETStandard,Version=v2.0.AssemblyAttributes.fs")
            )
        File.WriteAllLines(mainProxiesFile, fixedArgs)
        MakeDummyDll config.AssemblyFile thisName
        logger.Out "Written Proxies.args"
        0 
    else

    let checker = checkerFactory()
    
    let config = RunFSharpSourceGeneration logger config

    let isBundleOnly = config.ProjectType = Some BundleOnly

    let jsCompilerArgs =
        let ca =
            if thisName = "WebSharper.StdLib" then
                logger.Out "Reading Proxies.args"
                File.ReadAllLines(mainProxiesFile())
            else
                config.CompilerArgs    
        if not (config.UseJavaScriptSymbol |> Option.exists id) || ca |> Array.contains "--define:JAVASCRIPT" then
            ca
        else
            Array.append ca [|"--define:JAVASCRIPT"|]

    let exitCode = 
        if isBundleOnly then
            MakeDummyDll config.AssemblyFile thisName
            0
        else
            let errors, exnOpt = 
                checker.Compile(if config.ProjectType = Some Proxy then jsCompilerArgs else config.CompilerArgs) |> Async.RunSynchronously
    
            PrintFSharpErrors warnSettings logger errors
    
            if Option.isNone exnOpt then 
                if not (File.Exists config.AssemblyFile) then
                    argError "Output assembly not found"

                logger.TimedStage "F# compilation"
                0
            else
                1
            
    if exitCode <> 0 then 
        exitCode
    elif config.ProjectType = Some WIG then  
        let aR = createAssemblyResolver config true
        aR.Wrap <| fun () ->
            try 
                RunInterfaceGenerator aR config.KeyFile config logger
                0
            with e ->
                PrintGlobalError logger (sprintf "Error running WIG assembly: %A" e)
                1
    else 

    let mergeDirs, exitCode =
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
            if exitCode = 0 then
                Merging.prepareForBuild logger baseline outputDir modified conflict
                logger.TimedStage "Making copy of output folder for merging"
                Some (baseline, outputDir, modified, conflict), 0
            else
                None, exitCode
        | _ ->
            None, 0

    if exitCode <> 0 then 
        exitCode
    else
    
    let aR = createAssemblyResolver config true
    let loader = Loader.Create aR logger.Error
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    let wsRefsMeta =
        System.Threading.Tasks.Task.Run(fun () ->
            let mutable refError = false
            let wsRefs, metas = 
                refs |> List.choose (fun r -> 
                    match tryGetMetadata r with
                    | None -> None
                    | Some (Ok m) -> Some (r, m)
                    | Some (Error e) ->
                        refError <- true
                        PrintGlobalError logger e
                        None
                ) |> List.unzip
            if refError then None
            elif List.isEmpty metas then Some ([], [], WebSharper.Core.Metadata.Info.Empty) 
            else
                try
                    Some (
                        wsRefs, metas, WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas
                    )
                with e ->
                    refError <- true
                    PrintGlobalError logger (sprintf "Error merging WebSharper metadata: %A" e)
                    None
        )
    
    let refMeta = 
        wsRefsMeta.ContinueWith(fun (t: System.Threading.Tasks.Task<_>) -> 
            match t.Result with 
            | Some (_, _, m) -> Some m 
            | _ -> None
        )

    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(checker)
    compiler.WarnSettings <- warnSettings

    let comp =
        aR.Wrap <| fun () ->
            compiler.Compile(refMeta, jsCompilerArgs, config, thisName, logger)

    let exitCode =
        match comp with
        | None ->        
            1
        | Some comp ->

        if not (List.isEmpty comp.Errors || config.WarnOnly) then        
            PrintWebSharperErrors config.WarnOnly config.ProjectFile warnSettings logger comp
            1
        else
    
        let getRefMeta() =
            match wsRefsMeta.Result with 
            | Some (_, _, m) -> m 
            | _ -> WebSharper.Core.Metadata.Info.Empty

        let getRefMetas() =
            match wsRefsMeta.Result with 
            | Some (_, m, _) -> m 
            | _ -> []

        let js, currentMeta, runtimeMeta, sources, extraBundles, textOutputs =
            let currentMeta = comp.ToCurrentMetadata(config.WarnOnly)
            if isBundleOnly then
                let currentMeta, sources = TransformMetaSources comp.AssemblyName currentMeta config.SourceMap 
                let extraBundles =
                    aR.Wrap <| fun () ->
                        Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice1Of2 comp.AssemblyName)
                try
                    HandleExtraFiles None comp.AssemblyName config.ProjectFile config.OutputDir true
                with e ->
                    PrintGlobalError logger (sprintf "Error processing extra.files: %A" e)
                None, currentMeta, currentMeta, sources, extraBundles, [||]
            else
                let assem = loader.LoadFile config.AssemblyFile

                if config.ProjectType = Some Proxy then
                    EraseAssemblyContents assem
                    logger.TimedStage "Erasing assembly content for Proxy project"

                let extraBundles = 
                    aR.Wrap <| fun () ->
                        Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice2Of2 assem)
    
                let runtimeMeta =
                    match config.ProjectType with
                    | Some (Bundle | Website | Html | Service) -> Some (config.RuntimeMetadata, getRefMetas())
                    | _ when config.DeadCodeElimination = Some true -> Some (config.RuntimeMetadata, getRefMetas())
                    | _ -> None

                let isLibrary = config.ProjectType = None 

                let isSitelet =
                    match config.ProjectType with
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
                            CreateResourcesMode.ProdSitelet(config.RuntimeMetadata, getRefMetas(), refs)
                        else
                            CreateResourcesMode.DebugSitelet(getRefMeta(), config.RuntimeMetadata, getRefMetas())
                    elif isLibrary then
                        CreateResourcesMode.Library(getRefMeta())
                    else
                        CreateResourcesMode.Other(getRefMeta())

                let res =
                    assem
                    |> ModifyAssembly {
                        Logger = logger
                        Compilation = Some comp
                        CurrentMetadata = currentMeta
                        SourceMap = config.SourceMap
                        TypeScriptDeclaration = config.TypeScriptDeclaration
                        TypeScript = config.TypeScriptOutput
                        DeadCodeElimination = dce
                        AnalyzeClosures = config.AnalyzeClosures
                        Mode = mode
                    }
                
                let extraFilesEmbedAssem =
                    match config.ProjectType with
                    | Some (Bundle | Website | Html) -> None
                    | _ -> Some assem
                try
                    HandleExtraFiles extraFilesEmbedAssem comp.AssemblyName config.ProjectFile config.OutputDir (config.ProjectType = Some Bundle)
                with e ->
                    PrintGlobalError logger (sprintf "Error processing extra.files: %A" e)

                match config.ProjectType, config.DeadCodeElimination, config.OutputDir with
                | None, Some true, Some outputDir ->
                    UnpackLibraryCode logger (Some comp) (getRefMeta()) currentMeta config.TypeScriptDeclaration config.TypeScriptOutput runtimeMeta outputDir
                | _ -> ()
                
                match config.ProjectType with
                | Some (Bundle | Website | Html) ->
                    let wsRefs =
                        match wsRefsMeta.Result with 
                        | Some (r, _, m) -> r 
                        | _ -> []
                    AddExtraAssemblyReferences wsRefs assem
                | _ -> ()

                PrintWebSharperErrors config.WarnOnly config.ProjectFile warnSettings logger comp
            
                if config.PrintJS then
                    for (fname, js) in res.JSFiles do
                        logger.Out("// " + fname)
                        logger.Out(js)

                assem.Write (config.KeyFile |> Option.map File.ReadAllBytes) config.AssemblyFile

                logger.TimedStage "Writing resources into assembly"
                Some res.JSFiles, currentMeta, res.RuntimeMetadata |> Option.defaultValue currentMeta, res.Sources, extraBundles, res.AllTextFiles

        match config.JSOutputPath, js with
        | Some path, Some jss ->
            let asmPath = Path.Combine(path, thisName)
            Directory.CreateDirectory(asmPath) |> ignore
            if textOutputs |> Array.isEmpty || config.ProjectType <> None then
                for (fname, js) in jss do
                    let jsPath = Path.Combine(asmPath, fname)
                    File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, jsPath), js)
                    logger.TimedStage ("Writing " + jsPath)
            else
                for (name, content) in textOutputs do
                    if not <| name.ToLower().EndsWith ".meta" then
                        let filePath = Path.Combine(asmPath, name)
                        let fullPath = Path.Combine(Path.GetDirectoryName config.ProjectFile, filePath)
                        Directory.CreateDirectory(Path.GetDirectoryName fullPath) |> ignore
                        File.WriteAllText(fullPath, content)
                        logger.TimedStage ("Writing " + name)
        | _ -> ()

        // TODO minimized output
        //match config.MinJSOutputPath, js with
        //| Some path, Some (_, minjs) ->
        //    File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), minjs)
        //    logger.TimedStage ("Writing " + path)
        //| _ -> ()

        let unpack() =
            match ExecuteCommands.GetWebRoot config with
            | Some webRoot ->
                ExecuteCommands.Unpack webRoot config loader logger |> handleCommandResult logger config warnSettings "Unpacking" false
            | None ->
                PrintGlobalError logger "Failed to unpack website project, no WebSharperOutputDir specified"
                1

        match config.ProjectType with
        | Some (Bundle | BundleOnly) ->
            // comp.Graph does not have graph of dependencies and we need full graph here for bundling
            let metas =
                match wsRefsMeta.Result with
                | Some (_, metas, _) -> metas
                | _ -> []
            aR.Wrap <| fun () ->
                Bundling.Bundle config logger metas currentMeta comp sources refs extraBundles
            logger.TimedStage "Bundling"
            0
        | Some Html ->
            logger.Out "Start writing offline sitelet"
            logger.EnterContext()
            let htmlRes = ExecuteCommands.Html config runtimeMeta logger |> handleCommandResult logger config warnSettings "Finished writing offline sitelet" true
            if htmlRes = 0 then
                unpack()
            else
                htmlRes
        | Some Website ->
            unpack()
        | _ when Option.isSome config.OutputDir && config.DeadCodeElimination <> Some true ->
            unpack()
        | _ ->
            0

    let exitCode =
        if exitCode <> 0 then
            clearOutput config logger
            exitCode
        else
            match mergeDirs with 
            | Some (baseline, outputDir, modified, conflict) ->
                let exitCode = Merging.doMerge logger baseline outputDir modified conflict
                logger.TimedStage "Merging output with previously existing changes"
                exitCode
            | None -> 0

    exitCode

type ParseOptionsResult =
    | HelpOrCommand of int
    | ParsedOptions of WsConfig * WarnSettings

let ParseOptions (argv: string[]) (logger: LoggerBase) = 

    match HandleDefaultArgsAndCommands logger argv true with
    | Some r -> 
        HelpOrCommand r
    | _ ->
    
    let wsArgs = ref WsConfig.Empty
    let warn = ref WarnSettings.Default
    let refs = ResizeArray()
    let resources = ResizeArray()
    let fscArgs = ResizeArray()
    if not (argv.[0].EndsWith "fsc.exe" || argv.[0].EndsWith "fsc.dll") then
        fscArgs.Add "fsc.exe"   

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

    let fsCodeRE = System.Text.RegularExpressions.Regex(@"^(?:FS)?([0-9]+)$")

    let parseWarnCodeSet (s: string) =
        s.Split(',')
        |> Seq.choose (fun s ->
            let m = fsCodeRE.Match(s)
            if m.Success then
                Some (int m.Groups.[1].Value)
            else
                None
        )
        |> Set
    
    for a in cArgv do
        match RecognizeWebSharperArg a !wsArgs with
        | Some na -> wsArgs := na
        | _ ->
        match a with
        | "--vserrors" ->
            warn := { !warn with VSStyleErrors = true }
            fscArgs.Add a
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
            match r.Split(',') with 
            | [| res |] -> resources.Add (res, None)
            | [| res; fullName |] -> resources.Add (res, Some fullName)
            | _ -> argError ("Unexpected value --resource:" + r)
            fscArgs.Add a
        | StartsWith "--keyfile:" k ->
            wsArgs := { !wsArgs with KeyFile = Some k }
            fscArgs.Add a
        | StartsWith "--targetprofile:" p ->
            wsArgs := { !wsArgs with TargetProfile = p }
            fscArgs.Add a
        | StartsWith "--nowarn:" w ->
            warn := { !warn with NoWarn = (!warn).NoWarn + parseWarnCodeSet w }
        | StartsWith "--warn:" l ->
            warn := { !warn with WarnLevel = int l }
        | StartsWith "--warnon:" w ->
            warn := { !warn with NoWarn = (!warn).NoWarn - parseWarnCodeSet w }
        | "--warnaserror+" ->
            warn := { !warn with AllWarnAsError = true }
        | "--warnaserror-" ->
            warn := { !warn with AllWarnAsError = false }
        | StartsWith "--warnaserror:" w | StartsWith "--warnaserror+:" w ->
            warn := { !warn with WarnAsError = (!warn).WarnAsError + parseWarnCodeSet w }
        | StartsWith "--warnaserror-:" w ->
            warn := { !warn with DontWarnAsError = (!warn).DontWarnAsError + parseWarnCodeSet w }
        | StartsWith "--preferreduilang:" _ ->
            () // not handled by FSC 16.0.2
        | _ -> 
            fscArgs.Add a  
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = fscArgs.ToArray() 
        }
    wsArgs := SetDefaults true !wsArgs

    ParsedOptions (!wsArgs, !warn)

let StandAloneCompile config warnSettings logger checkerFactory tryGetMetadata = 
    try 
        let exitCode = 
            Compile config warnSettings logger checkerFactory tryGetMetadata
        exitCode            
    with _ ->
        clearOutput config logger
        reraise()
