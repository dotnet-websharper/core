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
open System.IO
open System.Reflection
open WebSharper.Compiler

open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
module C = WebSharper.Compiler.Commands
open WebSharper.Compiler.FSharp.ErrorPrinting
open FSharp.Compiler.CodeAnalysis

let createAssemblyResolver (config : WsConfig) =
    let compilerDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let paths =
        [
            for r in config.References -> Path.GetFullPath r
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

let Compile (config : WsConfig) (warnSettings: WarnSettings) (logger: LoggerBase) (checkerFactory: unit -> FSharpChecker) (tryGetMetadata: Assembly -> Result<WebSharper.Core.Metadata.Info, string> option) =    
    config.ArgWarnings |> List.iter (PrintGlobalWarning warnSettings logger)
    
    if config.AssemblyFile = null then
        argError "You must provide assembly output path."
    
    if config.ProjectType <> Some WIG && config.ProjectFile = null then
        argError "You must provide project file path."

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let mainProxiesFile() =
        "../../../build/" + (if config.IsDebug then "Debug" else "Release") + "/Proxies.args"    

    if thisName = "WebSharper.Main.Proxies" then
        let config =
            { config with
                References =
                    config.References
                    |> Array.filter (fun r -> not (r.EndsWith "WebSharper.Main.Proxies.dll"))
            }
        let mainProxiesFile = mainProxiesFile()
        Directory.CreateDirectory(Path.GetDirectoryName(mainProxiesFile)) |> ignore
        let fixedArgs =
            config.CompilerArgs 
            |> Array.map (fun s -> 
                s.Replace(@"net6.0\.NETCoreApp,Version=v6.0.AssemblyAttributes.fs", 
                    @"netstandard2.0\.NETStandard,Version=v2.0.AssemblyAttributes.fs"
                    ).Replace(@"net6.0/.NETCoreApp,Version=v6.0.AssemblyAttributes.fs", 
                        @"netstandard2.0/.NETStandard,Version=v2.0.AssemblyAttributes.fs")
            )
        File.WriteAllLines(mainProxiesFile, fixedArgs)
        MakeDummyDll config.AssemblyFile thisName
        logger.Out "Written Proxies.args"
        0 
    else

    let checker = checkerFactory()
    
    let isBundleOnly = config.ProjectType = Some BundleOnly

    let exitCode = 
        if isBundleOnly then
            MakeDummyDll config.AssemblyFile thisName
            0
        else
            let errors, exitCode = 
                checker.Compile(config.CompilerArgs) |> Async.RunSynchronously
    
            PrintFSharpErrors warnSettings logger errors
    
            if exitCode = 0 then 
                if not (File.Exists config.AssemblyFile) then
                    argError "Output assembly not found"

                logger.TimedStage "F# compilation"

            exitCode
            
    if exitCode <> 0 then 
        exitCode
    elif config.ProjectType = Some WIG then  
        let aR = createAssemblyResolver config
        aR.Wrap <| fun () ->
            try 
                RunInterfaceGenerator aR config.KeyFile config logger
                0
            with e ->
                PrintGlobalError logger (sprintf "Error running WIG assembly: %A" e)
                1
    else 

    let aR = createAssemblyResolver config
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
                        wsRefs, metas,
                        { 
                            WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
                                Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
                        }
                    )
                with e ->
                    refError <- true
                    PrintGlobalError logger (sprintf "Error merging WebSharper metadata: %A" e)
                    None
        )
    
    let compilerArgs =
        if thisName = "WebSharper.Main" then
            logger.Out "Reading Proxies.args"
            File.ReadAllLines(mainProxiesFile())
        else
            config.CompilerArgs    
    
    let refMeta = 
        wsRefsMeta.ContinueWith(fun (t: System.Threading.Tasks.Task<_>) -> 
            match t.Result with 
            | Some (_, _, m) -> Some m 
            | _ -> None
        )

    let compilerArgs =
        if not config.UseJavaScriptSymbol || compilerArgs |> Array.contains "--define:JAVASCRIPT" then
            compilerArgs
        else
            Array.append compilerArgs [|"--define:JAVASCRIPT"|]

    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(checker)
    compiler.WarnSettings <- warnSettings

    let comp =
        aR.Wrap <| fun () ->
            compiler.Compile(refMeta, compilerArgs, config, thisName, logger)

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

    let js, currentMeta, sources, extraBundles =
        let currentMeta = comp.ToCurrentMetadata(config.WarnOnly)
        if isBundleOnly then
            let currentMeta, sources = TransformMetaSources comp.AssemblyName currentMeta config.SourceMap 
            let extraBundles =
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice1Of2 comp.AssemblyName)
            None, currentMeta, sources, extraBundles
        else
            let assem = loader.LoadFile config.AssemblyFile

            if config.ProjectType = Some Proxy then
                EraseAssemblyContents assem

            let extraBundles = 
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice2Of2 assem)
    
            let runtimeMeta =
                match config.ProjectType with
                | Some (Bundle | Website | Service) -> Some (config.RuntimeMetadata, getRefMetas())
                | _ -> None

            let js, currentMeta, sources =
                ModifyAssembly logger (Some comp) (getRefMeta()) currentMeta config.SourceMap config.AnalyzeClosures runtimeMeta assem

            match config.ProjectType with
            | Some (Bundle | Website) ->
                let wsRefs =
                    match wsRefsMeta.Result with 
                    | Some (r, _, m) -> r 
                    | _ -> []
                AddExtraAssemblyReferences wsRefs assem
            | _ -> ()

            PrintWebSharperErrors config.WarnOnly config.ProjectFile warnSettings logger comp
            
            if config.PrintJS then
                match js with 
                | Some (js, _) ->
                    sprintf "%s" js
                    |> logger.Out
                | _ -> ()

            logger.TimedStage "Erasing assembly content for Proxy project"

            assem.Write (config.KeyFile |> Option.map File.ReadAllBytes) config.AssemblyFile

            logger.TimedStage "Writing resources into assembly"
            js, currentMeta, sources, extraBundles

    match config.JSOutputPath, js with
    | Some path, Some (js, _) ->
        File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), js)
        logger.TimedStage ("Writing " + path)
    | _ -> ()

    match config.MinJSOutputPath, js with
    | Some path, Some (_, minjs) ->
        File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), minjs)
        logger.TimedStage ("Writing " + path)
    | _ -> ()

    match config.ProjectType with
    | Some (Bundle | BundleOnly) ->
        // comp.Graph does not have graph of dependencies and we need full graph here for bundling
        let metas =
            match wsRefsMeta.Result with
            | Some (_, metas, _) -> metas
            | _ -> []

        let currentJS =
            lazy CreateBundleJSOutput logger (getRefMeta()) currentMeta comp.EntryPoint
        aR.Wrap <| fun () ->
            Bundling.Bundle config logger metas currentMeta comp currentJS sources refs extraBundles
        logger.TimedStage "Bundling"
        0
    | Some Html ->
        logger.Out "Start writing offline sitelet"
        logger.EnterContext()
        let rm = comp.ToRuntimeMetadata()
        let runtimeMeta = 
            { rm with
                Dependencies = 
                    WebSharper.Core.DependencyGraph.Graph.FromData(
                        getRefMetas() |> Seq.map (fun m -> m.Dependencies)
                        |> Seq.append [ rm.Dependencies ]
                    ).GetData()
            }
        ExecuteCommands.Html config runtimeMeta logger |> handleCommandResult logger config warnSettings "Finished writing offline sitelet" true
    | Some Website
    | _ when Option.isSome config.OutputDir ->
        match ExecuteCommands.GetWebRoot config with
        | Some webRoot ->
            ExecuteCommands.Unpack webRoot config loader logger |> handleCommandResult logger config warnSettings "Unpacking" false
        | None ->
            PrintGlobalError logger "Failed to unpack website project, no WebSharperOutputDir specified"
            1
    | _ ->
        0

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
    let clearOutput() =
        try
            let intermediaryOutput = config.AssemblyFile
            if File.Exists intermediaryOutput then 
                let failedOutput = intermediaryOutput + ".failed"
                if File.Exists failedOutput then File.Delete failedOutput
                File.Move (intermediaryOutput, failedOutput)
        with _ ->
            PrintGlobalError logger "Failed to clean intermediate output!"
    try 
        let exitCode = 
            Compile config warnSettings logger checkerFactory tryGetMetadata
        if exitCode <> 0 then 
            clearOutput()
        exitCode            
    with _ ->
        clearOutput()
        reraise()

