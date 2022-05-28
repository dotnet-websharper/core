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

module WebSharper.Compiler.CSharp.Compile

open System
open System.IO
open System.Reflection
open WebSharper.Compiler

open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
open WebSharper.Compiler.CSharp.ErrorPrinting

module C = WebSharper.Compiler.Commands

let Compile config (logger: LoggerBase) tryGetMetadata =
    if config.AssemblyFile = null then
        argError "You must provide assembly output path."

    let isBundleOnly = config.ProjectType = Some BundleOnly
    
    if not (isBundleOnly || File.Exists config.AssemblyFile) then 
        ()
    else

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
    let refMeta =
        if refError then None
        elif List.isEmpty metas then Some WebSharper.Core.Metadata.Info.Empty 
        else 
            try
                Some { 
                    WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
                        Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
                }
            with e ->
                refError <- true
                PrintGlobalError logger (sprintf "Error merging WebSharper metadata: %A" e)
                None

    logger.TimedStage "Loading referenced metadata"

    match refMeta with
    | None ->
        argError "" // exits without printing more errors    
    | Some refMeta ->

    let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler()

    if config.ProjectFile = null then
        argError "You must provide project file path."
    
    let assem = if isBundleOnly then None else Some (loader.LoadFile config.AssemblyFile)

    // remove for debugging
    if assem.IsSome && assem.Value.HasWebSharperMetadata then
        logger.TimedStage "WebSharper resources already exist, skipping"
    else

    let comp =
        aR.Wrap <| fun () ->
            compiler.Compile(refMeta, config, logger)
    
    if not (List.isEmpty comp.Errors || config.WarnOnly) then        
        PrintWebSharperErrors logger config.WarnOnly comp
        argError "" // exits without printing more errors
    else

    let js, currentMeta, sources, extraBundles =
        let currentMeta = comp.ToCurrentMetadata(config.WarnOnly)
        if isBundleOnly then
            let currentMeta, sources = TransformMetaSources comp.AssemblyName currentMeta config.SourceMap 
            let extraBundles = 
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger metas currentMeta refs comp (Choice1Of2 comp.AssemblyName)
            None, currentMeta, sources, extraBundles
        else
            let assem = assem.Value

            if config.ProjectType = Some Proxy then
                EraseAssemblyContents assem
                logger.TimedStage "Erasing assembly content for Proxy project"

            let extraBundles = 
                aR.Wrap <| fun () ->
                    Bundling.AddExtraBundles config logger metas currentMeta refs comp (Choice2Of2 assem)

            let runtimeMeta =
                match config.ProjectType with
                | Some (Bundle | Website) -> Some (config.RuntimeMetadata, metas)
                | _ -> None

            let js, currentMeta, sources =
                ModifyAssembly logger (Some comp) refMeta currentMeta config.SourceMap config.AnalyzeClosures runtimeMeta assem

            match config.ProjectType with
            | Some (Bundle | Website) ->
                AddExtraAssemblyReferences wsRefs assem
            | _ -> ()

            PrintWebSharperErrors logger config.WarnOnly comp

            if config.PrintJS then
                match js with 
                | Some (js, _) ->
                    printfn "%s" js
                | _ -> ()

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
        logger.TimedStage stageName
        if res = 1 then argError "" // exits without printing more errors    

    match config.ProjectType with
    | Some (Bundle | BundleOnly) ->
        let currentJS =
            lazy CreateBundleJSOutput logger refMeta currentMeta comp.EntryPoint
        aR.Wrap <| fun () ->
            Bundling.Bundle config logger metas currentMeta comp currentJS sources refs extraBundles
        logger.TimedStage "Bundling"
    | Some Html ->
        let rm = comp.ToRuntimeMetadata()
        let runtimeMeta = 
            { rm with
                Dependencies = 
                    WebSharper.Core.DependencyGraph.Graph.FromData(
                        metas |> Seq.map (fun m -> m.Dependencies)
                        |> Seq.append [ rm.Dependencies ]
                    ).GetData()
            }
        ExecuteCommands.Html config runtimeMeta logger |> handleCommandResult "Writing offline sitelets"
    | Some Website
    | _ when Option.isSome config.OutputDir ->
        match ExecuteCommands.GetWebRoot config with
        | Some webRoot ->
            ExecuteCommands.Unpack webRoot config loader logger |> handleCommandResult "Unpacking"
        | None ->
            PrintGlobalError logger "Failed to unpack website project, no WebSharperOutputDir specified"
    | _ -> ()

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
        | _ -> 
            cscArgs.Add a
    wsArgs := 
        { !wsArgs with 
            References = refs |> Seq.distinct |> Array.ofSeq
            Resources = resources.ToArray()
            CompilerArgs = cscArgs.ToArray() 
        }
    wsArgs := SetDefaults false !wsArgs

    if (!wsArgs).UseJavaScriptSymbol then
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
