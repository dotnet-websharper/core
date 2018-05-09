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

open WebSharper.Compiler.CommandTools
open WebSharper.Compiler.FrontEnd
module C = WebSharper.Compiler.Commands

open ErrorPrinting

let Compile config =
    StartTimer()

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
    
    let loader = Loader.Create aR (printfn "%s")
    let refs = [ for r in config.References -> loader.LoadFile(r, false) ]
    let mutable refError = false
    let wsRefs, metas = 
        refs |> List.choose (fun r -> 
            match TryReadFromAssembly FullMetadata r with
            | None -> None
            | Some (Ok m) -> Some (r, m)
            | Some (Error e) ->
                refError <- true
                PrintGlobalError e
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
                PrintGlobalError ("Error merging WebSharper metadata: " + e.Message)
                None

    TimedStage "Loading referenced metadata"

    match refMeta with
    | None ->
        argError "" // exits without printing more errors    
    | Some refMeta ->

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
        argError "You must provide project file path."
    
    let comp =
        compiler.Compile(refMeta, config)
    
    if not (List.isEmpty comp.Errors || config.WarnOnly) then        
        PrintWebSharperErrors config.WarnOnly comp
        argError "" // exits without printing more errors
    else

    let js, currentMeta, sources =
        if isBundleOnly then
            let currentMeta, sources = TransformMetaSources comp.AssemblyName (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap 
            None, currentMeta, sources
        else
            let assem = loader.LoadFile config.AssemblyFile

            let js, currentMeta, sources =
                ModifyAssembly (Some comp) refMeta
                    (comp.ToCurrentMetadata(config.WarnOnly)) config.SourceMap config.AnalyzeClosures assem

            match config.ProjectType with
            | Some (Bundle | Website) ->
                AddExtraAssemblyReferences wsRefs assem
            | _ -> ()

            PrintWebSharperErrors config.WarnOnly comp

            if config.PrintJS then
                match js with 
                | Some (js, _) ->
                    printfn "%s" js
                | _ -> ()

            assem.Write (config.KeyFile |> Option.map readStrongNameKeyPair) config.AssemblyFile

            TimedStage "Writing resources into assembly"
            js, currentMeta, sources

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
        let currentJS =
            lazy CreateBundleJSOutput refMeta currentMeta
        Bundling.Bundle config metas currentMeta currentJS sources refs
        TimedStage "Bundling"
    | Some Html ->
        ExecuteCommands.Html config |> ignore
        TimedStage "Writing offline sitelets"
    | Some Website
    | _ when Option.isSome config.OutputDir ->
        match ExecuteCommands.GetWebRoot config with
        | Some webRoot ->
            let res =
                match ExecuteCommands.Unpack webRoot config with
                | C.Ok -> 0
                | C.Errors errors ->
                    if config.WarnOnly || config.DownloadResources = Some false then
                        errors |> List.iter PrintGlobalWarning
                        0
                    else
                        errors |> List.iter PrintGlobalError
                        1
            TimedStage "Unpacking"
            if res = 1 then argError "" // exits without printing more errors    
        | None ->
            PrintGlobalError "Failed to unpack website project, no WebSharperOutputDir specified"
    | _ -> ()

let rec compileMain (argv: string[]) =

    match HandleDefaultArgsAndCommands argv false with
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
            VSStyleErrors = true
        }
    wsArgs := SetDefaultProjectFile !wsArgs false

    let wsconfig = Path.Combine(Path.GetDirectoryName (!wsArgs).ProjectFile, "wsconfig.json")
    if File.Exists wsconfig then
        wsArgs := (!wsArgs).AddJson(File.ReadAllText wsconfig)

    try
        Compile !wsArgs
        0
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

[<EntryPoint>]
let main argv =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    try
        compileMain (formatArgv argv)
    with
    | ArgumentError "" -> 
        1    
    | ArgumentError msg -> 
        PrintGlobalError msg
        1    
    | e -> 
        PrintGlobalError (sprintf "Global error '%s' at %s" e.Message e.StackTrace)
        1
