module WebSharper.Compiler.FSharp.Compile

open System
open System.IO
open System.Reflection
open WebSharper.Compiler

open WebSharper.Compiler.LoggerBase
open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
module C = WebSharper.Compiler.Commands
open WebSharper.Compiler.FSharp.ErrorPrinting
open FSharp.Compiler.SourceCodeServices

let Compile (config : WsConfig) (warnSettings: WarnSettings) (logger: LoggerBase) (checkerFactory: unit -> FSharpChecker) (tryGetMetadata: Assembly -> Result<WebSharper.Core.Metadata.Info, string> option) =
    let (StartTimer, TimedStage) = logger.TimedOut()
    StartTimer()
    
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
        File.WriteAllLines(mainProxiesFile, config.CompilerArgs)
        MakeDummyDll config.AssemblyFile thisName
        logger.Out "Written Proxies.args"
        0 
    else

    let checker = checkerFactory()
    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(logger.Out, checker)
    compiler.WarnSettings <- warnSettings

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

                TimedStage "F# compilation"

            exitCode
            
    if exitCode <> 0 then 
        exitCode
    else

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

    if config.ProjectType = Some WIG then  
        aR.Wrap <| fun () ->
        try 
            RunInterfaceGenerator aR config.KeyFile config
            TimedStage "WIG running time"
            0
        with e ->
            PrintGlobalError logger (sprintf "Error running WIG assembly: %A" e)
            1
    
    else    
    let loader = Loader.Create aR logger.Out
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

    let comp =
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
            let extraBundles = Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice1Of2 comp.AssemblyName)
            None, currentMeta, sources, extraBundles
        else
            let assem = loader.LoadFile config.AssemblyFile

            if config.ProjectType = Some Proxy then
                EraseAssemblyContents assem

            let extraBundles = Bundling.AddExtraBundles config logger (getRefMetas()) currentMeta refs comp (Choice2Of2 assem)
    
            let js, currentMeta, sources =
                ModifyAssembly logger (Some comp) (getRefMeta()) currentMeta config.SourceMap config.AnalyzeClosures assem

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

            TimedStage "Erasing assembly content for Proxy project"

            assem.Write (config.KeyFile |> Option.map File.ReadAllBytes) config.AssemblyFile

            TimedStage "Writing resources into assembly"
            js, currentMeta, sources, extraBundles

    match config.JSOutputPath, js with
    | Some path, Some (js, _) ->
        File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), js)
        TimedStage ("Writing " + path)
    | _ -> ()

    match config.MinJSOutputPath, js with
    | Some path, Some (_, minjs) ->
        File.WriteAllText(Path.Combine(Path.GetDirectoryName config.ProjectFile, path), minjs)
        TimedStage ("Writing " + path)
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
        Bundling.Bundle config logger metas currentMeta comp currentJS sources refs extraBundles
        TimedStage "Bundling"
        0
    | Some Html ->
        ExecuteCommands.Html config |> ignore
        TimedStage "Writing offline sitelets"
        0
    | Some Website
    | _ when Option.isSome config.OutputDir ->
        match ExecuteCommands.GetWebRoot config with
        | Some webRoot ->
            let res =
                match ExecuteCommands.Unpack webRoot config logger with
                | C.Ok -> 0
                | C.Errors errors ->
                    if config.WarnOnly || config.DownloadResources = Some false then
                        errors |> List.iter (PrintGlobalWarning warnSettings logger)
                        0
                    else
                        errors |> List.iter (PrintGlobalError logger)
                        1
            TimedStage "Unpacking"
            res
        | None ->
            PrintGlobalError logger "Failed to unpack website project, no WebSharperOutputDir specified"
            1
    | _ ->
        0

let compileMain (argv: string[]) checkerFactory tryGetMetadata (logger: LoggerBase) =

    match HandleDefaultArgsAndCommands logger argv true with
    | Some r -> r
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
            wsArgs := { !wsArgs with VSStyleErrors = true }
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
    wsArgs := SetDefaultProjectFile !wsArgs true
    wsArgs := SetScriptBaseUrl !wsArgs

    let clearOutput() =
        try
            let intermediaryOutput = (!wsArgs).AssemblyFile
            if File.Exists intermediaryOutput then 
                let failedOutput = intermediaryOutput + ".failed"
                if File.Exists failedOutput then File.Delete failedOutput
                File.Move (intermediaryOutput, failedOutput)
        with _ ->
            PrintGlobalError logger "Failed to clean intermediate output!"

    try 
        let exitCode = Compile !wsArgs !warn logger checkerFactory tryGetMetadata
        if exitCode <> 0 then clearOutput()
        exitCode            
    with _ ->
        clearOutput()
        reraise()

