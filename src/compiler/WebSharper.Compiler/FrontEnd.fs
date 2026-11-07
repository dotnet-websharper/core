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

module WebSharper.Compiler.FrontEnd

open WebSharper.Core
open WebSharper.Constants
open System.IO
open System.Collections.Generic

module M = WebSharper.Core.Metadata
module MIO = WebSharper.Core.MetadataIO
module P = WebSharper.PathConventions

type Assembly = WebSharper.Compiler.Assembly
type Content = WebSharper.Compiler.Content
type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
type Loader = WebSharper.Compiler.Loader
type Symbols = WebSharper.Compiler.Symbols
type O = WebSharper.Core.JavaScript.Output

/// Reads the selected sections of the WebSharper metadata of an assembly from its sidecar file.
/// An assembly marked by WebSharper but without a sidecar, or with a sidecar written for
/// another build of it, is an error.
let TryReadFromAssemblyWith (readOptions: MIO.ReadOptions) (a: Assembly) =
    let sidecarPath () =
        match a.LoadPath with
        | Some p -> Sidecar.PathFor p
        | None -> a.Name + ".websharper"
    match (try Ok a.Sidecar with e -> Error e.Message) with
    | Error e -> Some (Error e)
    | Ok None ->
        if a.HasSidecarMarker then
            Some (Error (sprintf "WebSharper library %s has no sidecar file at %s. Copy the .websharper file together with the .dll." a.FullName (sidecarPath ())))
        elif a.HasWebSharperMetadata then
            // WebSharper 11 writes metadata only to sidecar files
            Some (Error (sprintf "WebSharper library %s was built with WebSharper 10 or earlier (it has embedded metadata and no .websharper sidecar file). Use a version of it built with WebSharper 11, or recompile it." a.FullName))
        else None
    | Ok (Some sc) ->
        if sc.ModuleId <> a.ModuleId then
            Some (Error (sprintf "The WebSharper sidecar file %s was written for another build of %s. Rebuild the library, or copy its matching .websharper file." sc.Path a.FullName))
        else
        match sc.TryGet Sidecar.MetadataEntry with
        | None -> None
        | Some data ->
            try
                Ok (MIO.DecodeBytesWith readOptions data 0 data.Length)
            with e -> Error (sprintf "Failed to deserialize metadata for %s from %s. Error: %s" a.FullName sc.Path e.Message)
            |> Some

/// Writes the sidecar file of an assembly.
let WriteSidecar (assemblyPath: string) (moduleId: System.Guid) (entries: seq<string * byte[]>) =
    let path = Sidecar.PathFor assemblyPath
    let entries = Array.ofSeq entries
    // metadata sections are compressed already
    Sidecar.Write(path, moduleId, entries, compress = fun n -> n <> EMBEDDED_METADATA)
    FileSystem.OutputFiles.Add path
    // the stamp changes only when what compiling a library against this one reads changes,
    // dependent library projects use it as compilation input instead of the sidecar
    match entries |> Array.tryFind (fun (n, _) -> n = EMBEDDED_METADATA) with
    | Some (_, meta) ->
        let stampPath = Sidecar.StampPathFor assemblyPath
        let stamp = MIO.InterfaceHash meta
        if not (File.Exists stampPath && File.ReadAllText stampPath = stamp) then
            File.WriteAllText(stampPath, stamp)
        FileSystem.OutputFiles.Add stampPath
    | None -> ()

/// The implementation files of references among the files an assembly resolver loaded.
let ExecutedReferences (referencePaths: seq<string>) (loadedFiles: seq<string>) =
    let refs = HashSet(referencePaths |> Seq.map Path.GetFullPath, System.StringComparer.OrdinalIgnoreCase)
    loadedFiles |> Seq.map Path.GetFullPath |> Seq.filter refs.Contains |> Seq.distinct |> Array.ofSeq

/// Lists the files of references a compilation depended on, in `<AssemblyName>.websharper.deps`
/// next to the assembly. MSBuild adds them to the compilation inputs: a reference assembly
/// does not change when only WebSharper-relevant parts of a library do. `executed` are the
/// implementation files of references whose code ran (macros, generators), their
/// implementation changes show neither in reference assemblies nor in stamps.
/// `sourceGenerators` are the files loaded for F# source generators, their output is compiled
/// by the .NET compiler.
/// Projects that can be translated without compiling their assembly (`WsConfig.CanTranslateOnly`,
/// libraries) list all but the source generators in `<AssemblyName>.websharper.wsdeps` instead:
/// when one of them is newer than that file, which is written by every WebSharper compilation,
/// MSBuild runs WebSharper alone (`--wstranslateonly`), also when the .NET compiler was skipped.
let WriteDependencies (assemblyPath: string) (references: seq<Assembly>) (readOptions: MIO.ReadOptions) (executed: seq<string>) (sourceGenerators: seq<string>) (canTranslateOnly: bool) =
    let referenceFiles =
        references |> Seq.choose (fun r ->
            match r.LoadPath with
            | Some p ->
                let sidecar = Sidecar.PathFor p
                let stamp = Sidecar.StampPathFor p
                if readOptions = MIO.ReadOptions.Library && File.Exists stamp then Some stamp
                elif File.Exists sidecar then Some sidecar
                else None
            | None -> None
        )
    let generatorFiles = sourceGenerators |> Seq.sort |> Seq.distinct |> Array.ofSeq
    let translationFiles =
        let generators = HashSet(generatorFiles, System.StringComparer.OrdinalIgnoreCase)
        Seq.append referenceFiles (Seq.sort executed)
        |> Seq.filter (generators.Contains >> not)
        |> Seq.distinct
        |> Array.ofSeq
    let compileInputs, translationInputs =
        if canTranslateOnly then
            generatorFiles, translationFiles
        else
            Array.append translationFiles generatorFiles |> Array.distinct, [||]
    let path = Path.ChangeExtension(assemblyPath, ".websharper.deps")
    if not (File.Exists path && File.ReadAllLines path = compileInputs) then
        File.WriteAllLines(path, compileInputs)
    let wsPath = Path.ChangeExtension(assemblyPath, ".websharper.wsdeps")
    if canTranslateOnly then
        // always written: its time is when WebSharper last translated the project
        File.WriteAllLines(wsPath, translationInputs)
    elif File.Exists wsPath then
        File.Delete wsPath

/// When the final assembly differs from what fsc emitted (WIG, Proxy and BundleOnly projects),
/// the reference assembly written by fsc (--refout) is replaced by the final assembly.
let ReplaceReferenceAssembly (logger: LoggerBase) (projectDir: string) (compilerArgs: string[]) (assemblyPath: string) =
    compilerArgs |> Array.tryPick (fun a ->
        let a = a.Trim()
        if a.StartsWith "--refout:" || a.StartsWith "/refout:" || a.StartsWith "-refout:" then
            Some (a.Substring(a.IndexOf ':' + 1).Trim('"'))
        else None
    )
    |> function
    | Some refOut ->
        let refOut = if Path.IsPathRooted refOut then refOut else Path.Combine(projectDir, refOut)
        Directory.CreateDirectory(Path.GetDirectoryName refOut) |> ignore
        File.Copy(assemblyPath, refOut, true)
        logger.Out (sprintf "Replaced reference assembly %s" refOut)
    | None ->
        ()

let TryReadFromAssembly options (a: Assembly) =
    TryReadFromAssemblyWith (MIO.ReadOptions.Of options) a
    |> Option.map (Result.map (M.ApplyMetadataOptions options))

let TryReadRuntimeFromAssembly options (a: Assembly) =
    a.TryGetResource EMBEDDED_RUNTIME_METADATA
    |> Option.map (fun data ->
        let m =
            try
                Ok (MIO.DecodeBytesWith (MIO.ReadOptions.Of options) data 0 data.Length)
            with e -> Error (sprintf "Failed to deserialize runtime metadata for %s. Error: %s" a.FullName e.Message)
        m |> Result.map (M.ApplyMetadataOptions options)
    )

let ReadFromAssembly options a =
    match TryReadFromAssembly options a with
    | None -> None
    | Some (Ok m) -> Some m
    | Some (Error e) -> raise (exn e) 

let ReadRuntimeFromAssembly options a =
    match TryReadRuntimeFromAssembly options a with
    | None -> None
    | Some (Ok m) -> Some m
    | Some (Error e) -> raise (exn e) 

let ReadFromFile options (path: string) =
    let aR = AssemblyResolver.Create().SearchPaths([path])
    let loader = Loader.Create aR ignore
    loader.LoadFile(path, false) |> ReadFromAssembly options

let ReadFullFromFile (path: string) =
    let aR = AssemblyResolver.Create().SearchPaths([path])
    let loader = Loader.Create aR ignore
    let asm = loader.LoadFile(path, false)
    let meta = ReadFromAssembly M.FullMetadata asm
    asm, meta

let ReadRuntimeFromFile (path: string) =
    let aR = AssemblyResolver.Create().SearchPaths([path])
    let loader = Loader.Create aR ignore
    let asm = loader.LoadFile(path, false)
    let meta = ReadRuntimeFromAssembly M.FullMetadata asm
    asm, meta

/// Adds resource hashes to the metadata of a WIG-generated assembly
/// and serializes it for its sidecar file: embedded web resources and the given ones,
/// written to the sidecar file.
let ModifyWIGAssembly (current: M.Info) (a: PEMetadata.PEAssembly) (webResources: (string * string * byte[])[]) =
    let assemblyName = a.Name
    for r in Assembly.GetAllResources a do
        let p = assemblyName + "/" + r.FileName
        let d = r.GetContentData()
        current.ResourceHashes.Add(p, AST.StableHash.data d)
    for name, _, d in webResources do
        current.ResourceHashes.[assemblyName + "/" + name] <- AST.StableHash.data d
    MIO.EncodeToBytes current

let ModifyTSAssembly (current: M.Info) (a: Assembly) =
    ModifyWIGAssembly current a.Image [||]

/// The module version id of an assembly file, written to its sidecar.
let ReadModuleId (assemblyPath: string) =
    PEMetadata.ReadModuleId assemblyPath
    
let TransformMetaSources assemblyName (current: M.Info) sourceMap =
    if sourceMap then
        let current, fileNames = transformAllSourcePositionsInMetadata assemblyName false current
        let sources = fileNames |> Array.choose (fun (fn, key) -> if File.Exists fn then Some (key, File.ReadAllText fn) else None)
        current, sources
    else
        transformAllSourcePositionsInMetadata assemblyName true current |> fst, [||]

let GetDepsFromJSExports (jsExport: JsExport list) entryPointNode (graph: WebSharper.Core.DependencyGraph.Graph) =
    let jsExportNames =
        jsExport |> List.choose (function 
            | ExportByName n -> Some n
            | _ -> None
        )
    let nodes =
        seq {
            yield entryPointNode
            match jsExportNames with
            | [] -> ()
            | _ ->
                let e = System.Collections.Generic.HashSet jsExportNames
                yield! 
                    graph.Nodes |> Seq.filter (function
                        | M.AssemblyNode a -> e.Contains a
                        | M.TypeNode td
                        | M.MethodNode (td, _)
                        | M.ConstructorNode (td, _) 
                            -> e.Contains td.Value.FullName || e.Contains td.Value.Assembly
                        | _ -> false
                    )
            yield!  
                jsExport |> Seq.choose (function 
                    | ExportNode n -> Some n
                    | _ -> None
                )
            if jsExport |> Seq.contains ExportCurrentAssembly then
                yield 
                    // assembly nodes are ordered, current is always last
                    graph.Nodes |> Seq.filter (function
                        | M.AssemblyNode _ -> true
                        | _ -> false
                    )
                    |> Seq.last
        }
        |> graph.GetDependencies
    nodes

[<RequireQualifiedAccess>]
type CreateResourcesMode =
    | ProdSitelet of runtimeMetadataOptions: M.MetadataOptions * refMetas: M.Info list * refAssembliesForSourceMapping: Assembly list
    | DebugSitelet of refMeta: M.Info * runtimeMetadataOptions: M.MetadataOptions * refMetas: M.Info list
    | Library of refMeta: M.Info
    | Other of refMeta: M.Info

type CreateResourcesArgs =
    {
        Logger : LoggerBase
        Compilation : Compilation option
        CurrentMetadata : M.Info
        SourceMap : bool
        TypeScriptDeclaration : bool
        TypeScript : bool
        DeadCodeElimination : bool 
        AnalyzeClosures : bool option
        Mode : CreateResourcesMode
        /// Web resources (extra.files and web worker bundles) going into the sidecar:
        /// name, content type and contents.
        WebResources : (string * string * byte[])[]
    }

[<RequireQualifiedAccess>]
type CreateResourcesFor =
    | Assembly of PEMetadata.PEAssembly
    | AssemblyName of string

type GeneratedResources =
    {
        JSFiles : (string * string)[]
        TSFiles : (string * string)[]
        DTSFiles : (string * string)[]
        MapFiles : (string * string)[]
        Sources : (string * string)[]
        Metadata : M.Info
        RuntimeMetadata : M.Info option
    }
    
    member this.SourceFiles =
        this.Sources |> Array.map (fun (name, contents) -> EMBEDDED_SOURCES + name, contents) 
    
    member this.AllTextFiles =
        Array.concat [| 
            this.JSFiles
            this.TSFiles 
            this.DTSFiles 
            this.MapFiles 
            this.SourceFiles
        |]

let CreateResources (args: CreateResourcesArgs) (resFor: CreateResourcesFor) =
    let logger = args.Logger
    let comp = args.Compilation
    let refMeta = 
        match args.Mode with
        | CreateResourcesMode.DebugSitelet (refMeta, _, _)
        | CreateResourcesMode.Library refMeta
        | CreateResourcesMode.Other refMeta -> refMeta
        | _ -> M.Info.Empty
    let current = args.CurrentMetadata
    let sourceMap = args.SourceMap
    let dts = args.TypeScriptDeclaration
    let ts = args.TypeScript
    let dce = args.DeadCodeElimination
    let closures = args.AnalyzeClosures
    let assemblyName = 
        match resFor with 
        | CreateResourcesFor.Assembly a -> a.Name
        | CreateResourcesFor.AssemblyName n -> n 
    let currentPosFixed, sources =
        TransformMetaSources assemblyName current sourceMap
        
    let currentPosFixed =
        match comp with
        | Some c -> c.HideInternalProxies currentPosFixed
        | _ -> currentPosFixed
    
    logger.TimedStage "Source position transformations"

    let epStyle =
        if args.Mode.IsLibrary then
            JavaScriptPackager.EntryPointStyle.LibraryBundle
        else
            JavaScriptPackager.EntryPointStyle.OptionalEntryPoint

    let pkg = 
        match args.Mode with
        | CreateResourcesMode.ProdSitelet _ ->
            [||]
        | _ ->
            let pkg =
                JavaScriptPackager.packageAssembly O.JavaScript refMeta current assemblyName dce (comp |> Option.bind (fun c -> c.EntryPoint)) epStyle
            logger.TimedStage "Packaging assembly"
            pkg
    
    let mapPkg f pkg =
        pkg |> Array.map (fun (n, p) -> n, p |> List.map f)    

    let pkg =
        match comp, closures with
        | Some comp, Some moveToTop ->
            let clPkg = pkg |> mapPkg (Closures.ExamineClosures(logger, comp, moveToTop).TransformStatement)
            logger.TimedStage "Closure analyzation"
            clPkg
        | _ -> pkg
    
    let pkg =
        if sourceMap then
            pkg |> mapPkg (TransformSourcePositions(assemblyName).TransformStatement)
        else
            pkg |> mapPkg removeSourcePos.TransformStatement

    let jsFiles = ResizeArray()
    let tsFiles = ResizeArray()
    let dtsFiles = ResizeArray()
    let mapFiles = ResizeArray()
    let sourceFiles = ResizeArray()

    let addRes (name: string) (contents: string) = 
        if name.EndsWith ".js" || name.EndsWith ".jsx" then
            jsFiles.Add(name, contents)
        elif name.EndsWith ".d.ts" then
            dtsFiles.Add(name, contents)
        elif name.EndsWith ".ts" || name.EndsWith ".tsx" then
            tsFiles.Add(name, contents)
        elif name.EndsWith ".map" then
            mapFiles.Add(name, contents)
        else
            sourceFiles.Add(name, contents)

    let rMeta =
        match args.Mode, comp with
        | CreateResourcesMode.ProdSitelet (rm, refMetas, _), Some comp
        | CreateResourcesMode.DebugSitelet (_, rm, refMetas), Some comp ->
            let trimmed = comp.ToRuntimeMetadata(rm <> M.DiscardExpressions) |> M.ApplyMetadataOptions rm |> refreshVarFields
            let graph =
                DependencyGraph.Graph.FromData(
                    refMetas |> Seq.map (fun m -> m.Dependencies)
                    |> Seq.append [ trimmed.Dependencies ]
                )
            let meta = 
                refMetas |> Seq.map refreshAllIds
                |> Seq.append (Seq.singleton currentPosFixed)
                |> M.Info.UnionWithoutDependencies

            let deps = 
                if rm = M.DiscardExpressions then
                    JavaScriptPackager.getTrimmedGraph meta graph
                else
                    graph.GetData() 

            logger.TimedStage "Computing metadata"

            let bundles =
                match args.Mode with
                | CreateResourcesMode.ProdSitelet (_, _, refAssemblies) ->
                    let output = if ts then O.TypeScript else O.JavaScript 
                    let ext = if ts then ".ts" else ".js"
                    let sourcesUsed = HashSet()
                    let writeBundle (bname, package) =
                        let sw = System.Diagnostics.Stopwatch.StartNew()
                        let bundleCode, addrMap = package ()
                        let packaged = sw.Elapsed
                        let bundleCode =
                            match closures with
                            | Some moveToTop ->
                                bundleCode |> List.map (Closures.ExamineClosures(logger, comp, moveToTop).TransformStatement)
                            | _ -> bundleCode
                        let program, _, trAddrMap = bundleCode |> WebSharper.Compiler.JavaScriptWriter.transformProgramAndAddrMap output WebSharper.Core.JavaScript.Readable addrMap
                        let converted = sw.Elapsed
                        let codeWriter =
                            WebSharper.Core.JavaScript.Writer.CodeWriter(sourceMap)
                        let js, map, isJsx = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable (fun () -> codeWriter) program false
                        let sources = if sourceMap then codeWriter.GetSourceFiles() else [||]
                        bname, js, map, isJsx, trAddrMap, sources, (packaged, converted - packaged, sw.Elapsed - converted)
                    let bundles =
                        let entryPointBundles = JavaScriptPackager.entryPointBundles meta graph comp.AssemblyName output
                        // closure analysis reports through the logger and the compilation, so it stays serial
                        if closures.IsSome then
                            entryPointBundles |> Array.map writeBundle
                        else
                            entryPointBundles |> Array.Parallel.map writeBundle
                    let bundles =
                        bundles |> Array.map (fun (bname, js, map, isJsx, trAddrMap, sources, (packaging, conversion, writing)) ->
                            let x = if isJsx then "x" else ""
                            if sourceMap then
                                sources |> Array.iter (sourcesUsed.Add >> ignore)
                                addRes (bname + ext + x + ".map") map.Value
                            logger.TimedStageElapsed (sprintf "Prebundle %s.js%s (packaging %.2f s, JS conversion %.2f s, writing)" bname x packaging.TotalSeconds conversion.TotalSeconds, writing)
                            bname, bname + ext + x, js, trAddrMap
                        )
                    logger.TimedStage (sprintf "Writing %d prebundles" bundles.Length)
                    if sourceMap then
                        let allSources = 
                            if sourceMap then
                                refAssemblies |> Seq.collect (fun a ->
                                    a.GetSources() |> Seq.map (fun s -> s.EmbeddedFileName.Replace(EMBEDDED_SOURCES, ""), s.Content)
                                )  
                                |> Seq.append sources
                                |> Array.ofSeq 
                            else [||]

                        allSources |> Array.filter (fun (n, s) -> sourcesUsed.Contains n) 
                        |> Array.iter (fun (name, contents) ->
                            addRes name contents
                        )
                    //if dts then
                    //    JavaScriptPackager.packageEntryPoint meta graph comp.AssemblyName O.TypeScriptDeclaration
                    //    |> Seq.map (fun (bname, (bundleCode, addrMap)) ->
                    //        let program, _, trAddrMap = bundleCode |> WebSharper.Compiler.JavaScriptWriter.transformProgramAndAddrMap O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable addrMap
                    //        let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
                    //        logger.TimedStage (sprintf "Writing prebundle %s.d.ts" bname)
                    //        "", bname + ".d.ts", js, trAddrMap
                    //    )
                    //    |> Seq.append bundles
                    //    |> Array.ofSeq |> Some
                    //else
                    //    Some bundles
                    Some bundles
                | CreateResourcesMode.DebugSitelet _ ->
                    let rootJS, addrMap = JavaScriptPackager.packageEntryPointReexport meta
                    let program, _, trAddrMap = rootJS |> WebSharper.Compiler.JavaScriptWriter.transformProgramAndAddrMap O.JavaScript WebSharper.Core.JavaScript.Readable addrMap
                    let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
                    let trAddrMap = Dict.union [ trAddrMap; dict [ AST.Address.Global(), assemblyName ] ]
                    logger.TimedStage (sprintf "Writing reexports root.js")
                    Some [|
                        "root", "root.js", js, trAddrMap    
                    |]
                | _ ->
                    None
            let trimmed =
                if rm = M.DiscardExpressions then trimmed.DiscardNotRuntimeClassInfo() else trimmed
            let updated =
                {
                    trimmed with
                        Dependencies =
                            deps
                        PreBundle = 
                            match bundles with
                            | Some bundles ->
                                bundles |> Seq.choose (fun (bname, _, _, addrMap) -> if bname <> "" then Some (bname, addrMap :> IDictionary<_,_>) else None) |> dict
                            | _ -> Dictionary()
                }
            Some (updated, bundles)
        | _ -> None

    match rMeta with
    | Some (_, Some bundles) ->
        for bname, bFileName, bundleText, _ in bundles do
            addRes bFileName bundleText
    | _ -> ()

    if pkg.Length > 0 then
        
        //let pu = P.PathUtility.VirtualPaths("/")
        //let ai = P.AssemblyId.Create(assemblyName)
        // code files are converted and written in parallel, they only read the packaged code
        let jss' =
            pkg
            |> Array.Parallel.map (fun (n, p) ->
                let program, jsx = p |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.JavaScript WebSharper.Core.JavaScript.Readable
                n, program, jsx
            )
        let shouldUseJSX = jss' |> Array.choose (fun (n, _, jsx) -> if jsx then Some n else None)
        let jss =
            jss' |> Array.Parallel.map (fun (n, p, jsx) ->
                let jsxModifiedProgram =
                    p
                    |> List.map (fun st ->
                        match st with
                        | JavaScript.Syntax.Statement.Import(di, fi, ni, mn) ->
                            if shouldUseJSX |> Array.exists (fun x -> mn.EndsWith(x + ".js")) then
                                JavaScript.Syntax.Statement.Import(di, fi, ni, mn + "x")
                            else
                                st
                        | _ -> st
                    )
                let codeWriter = 
                    WebSharper.Core.JavaScript.Writer.CodeWriter(sourceMap)

                let js, map, isJSX = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable (fun () -> codeWriter) jsxModifiedProgram jsx
                let sources = if sourceMap then codeWriter.GetSourceFiles() else [||]
                n, js, map, isJSX, sources
            )
        let sourcesUsed = HashSet(jss |> Seq.collect (fun (_, _, _, _, sources) -> sources))
        let jss = jss |> Array.map (fun (n, js, map, isJSX, _) -> n, js, map, isJSX)
        if sourceMap then
            sources |> Array.filter (fun (n, s) -> sourcesUsed.Contains n) 
            |> Array.iter (fun (name, contents) ->
                addRes name contents
            )

        logger.EnterContext()

        for n, js, map, isJSX in jss do
            let x = if isJSX then "x" else ""
            let jsName = n + ".js" + x
            addRes jsName js
            let jsMapName = n + ".js" + x + ".map"
            map |> Option.iter (fun m ->
                addRes jsMapName m)
            logger.TimedStage (if sourceMap then sprintf "Writing %s and %s" jsName jsMapName else sprintf "Writing %s" jsName)
        
        if ts then
            let tspkg = 
                JavaScriptPackager.packageAssembly O.TypeScript refMeta current assemblyName dce (comp |> Option.bind (fun c -> c.EntryPoint)) epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            let tss =
                tspkg |> Array.Parallel.map (fun (n, p) ->
                    let ts, _, isJSX =
                        p
                        |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScript WebSharper.Core.JavaScript.Readable
                        |> fun (prog, jsx) ->
                            WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                    n, ts, isJSX
                )
            for (n, ts, isJSX) in tss do
                let x = if isJSX then "x" else ""
                let tsName = n + ".ts" + x
                addRes tsName ts
                logger.TimedStage (sprintf "Writing %s" tsName)

        if dts then
            let dtspkg = 
                JavaScriptPackager.packageAssembly O.TypeScriptDeclaration refMeta current assemblyName dce None epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            let dtss =
                dtspkg |> Array.Parallel.map (fun (n, p) ->
                    let ts, _, _ =
                        p
                        |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable
                        |> fun (prog, jsx) ->
                            WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                    n, ts
                )
            for (n, ts) in dtss do
                let dTsName = n + ".d.ts"
                addRes dTsName ts
                logger.TimedStage (sprintf "Writing %s" dTsName)

        logger.ExitContext()
        logger.TimedStage "Writing per-class code files"

    match resFor with
    | CreateResourcesFor.Assembly a ->
        let mutable neededHashing = false
        let addHash name (d: byte[]) =
            let p = assemblyName + "/" + name
            currentPosFixed.ResourceHashes.[p] <- AST.StableHash.data d
            rMeta |> Option.iter (fun (rm, _) -> rm.ResourceHashes.[p] <- AST.StableHash.data d)
            neededHashing <- true
        for r in Assembly.GetAllResources a do
            addHash r.FileName (r.GetContentData())
        for name, _, d in args.WebResources do
            addHash name d
        if neededHashing then
            logger.TimedStage "Hashing resources"
    | _ -> ()

    {
        JSFiles = jsFiles.ToArray()
        TSFiles = tsFiles.ToArray()
        DTSFiles = dtsFiles.ToArray()
        MapFiles = mapFiles.ToArray()
        Sources = sourceFiles.ToArray()
        Metadata = currentPosFixed
        RuntimeMetadata = rMeta |> Option.map fst
    }

let private convertResourcesToBinary (logger: LoggerBase option) (res: GeneratedResources) =

    let binaries = ResizeArray()

    for name, content in res.AllTextFiles do
        binaries.Add(name, System.Text.Encoding.UTF8.GetBytes content)

    logger |> Option.iter (fun l -> l.TimedStage "Encoding text resources")

    let endodeMeta label meta =
        use s = new MemoryStream(8 * 1024)
        let encodeTime, compressTime = MIO.EncodeTimed s meta
        let bytes = s.ToArray()
        logger |> Option.iter (fun l ->
            l.TimedStageElapsed(sprintf "Encoding %s" label, encodeTime)
            l.TimedStageElapsed(sprintf "Compressing %s" label, compressTime)
            l.Out(sprintf "Size of %s: %d bytes" label bytes.Length)
        )
        bytes

    binaries.Add(EMBEDDED_METADATA, endodeMeta "metadata" res.Metadata)

    res.RuntimeMetadata |> Option.iter (fun rm -> binaries.Add(EMBEDDED_RUNTIME_METADATA, endodeMeta "runtime metadata" rm))

    binaries.ToArray()

/// Sidecar entries for web resources: the list of names and content types, and their contents.
let WebResourceSidecarEntries (webResources: (string * string * byte[])[]) =
    if Array.isEmpty webResources then [||] else
    [|
        yield Sidecar.WebResourcesEntry, Sidecar.EncodeWebResources (webResources |> Seq.map (fun (n, cT, _) -> n, cT))
        for n, _, c in webResources -> n, c
    |]

let ConvertResourcesToBinary (res: GeneratedResources) =
    convertResourcesToBinary None res

/// Creates the WebSharper output of an assembly: the generated resources, runtime metadata
/// for website, bundle, offline and service projects (embedded when the assembly is emitted),
/// and the sidecar entries.
let CreateOutput (args: CreateResourcesArgs) (resFor: CreateResourcesFor) =
    let res = CreateResources args resFor
    let binaries = convertResourcesToBinary (Some args.Logger) res
    args.Logger.TimedStage "Serializing metadata"
    let runtimeMeta =
        binaries |> Array.tryPick (fun (name, contents) -> if name = EMBEDDED_RUNTIME_METADATA then Some contents else None)
    let sidecarEntries =
        Array.append
            (binaries |> Array.filter (fun (name, _) -> name <> EMBEDDED_RUNTIME_METADATA))
            (WebResourceSidecarEntries args.WebResources)
    res, runtimeMeta, sidecarEntries

/// Creates the WebSharper output of a loaded assembly, see CreateOutput.
let CreateOutputFor (args: CreateResourcesArgs) (assembly: Assembly option) (assemblyName: string) =
    CreateOutput args (match assembly with Some a -> CreateResourcesFor.Assembly a.Image | None -> CreateResourcesFor.AssemblyName assemblyName)

let UnpackLibraryCode (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) dts ts (runtimeMeta: option<M.MetadataOptions * M.Info list>) outputDir =
    match comp, runtimeMeta with
    | Some c, Some (_, refMetas) ->
        let currentL =
            let nodes = GetDepsFromJSExports c.JavaScriptExports M.EntryPointNode c.Graph
            let meta = c.ToRuntimeMetadata(true)
            let graph =
                DependencyGraph.Graph.FromData(
                    refMetas |> Seq.map (fun m -> m.Dependencies)
                    |> Seq.append [ meta.Dependencies ]
                )
            let deps = graph.GetDependencies nodes
            trimMetadata meta deps

        let makeFile name (js: string) =
            let path = Path.Combine(outputDir, name)
            if not (Directory.Exists outputDir) then
                Directory.CreateDirectory outputDir |> ignore
            File.WriteAllText(path, js)
            FileSystem.OutputFiles.Add path

        let jspkg = 
            JavaScriptPackager.packageAssembly O.JavaScript refMeta currentL "" true (comp |> Option.bind (fun c -> c.EntryPoint)) JavaScriptPackager.EntryPointStyle.LibraryBundle
        for name, p in jspkg do
            let program, isJsx = p |> WebSharper.Compiler.JavaScriptWriter.transformProgram O.JavaScript WebSharper.Core.JavaScript.Readable
            let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
            let x = if isJsx then "x" else ""
            makeFile (name + ".js" + x) js

        if dts then
            let dtspkg = 
                JavaScriptPackager.packageAssembly O.TypeScriptDeclaration refMeta currentL "" true (comp |> Option.bind (fun c -> c.EntryPoint)) JavaScriptPackager.EntryPointStyle.LibraryBundle
            for name, p in dtspkg do
                let program, _ = p |> WebSharper.Compiler.JavaScriptWriter.transformProgram O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable
                let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
                makeFile (name + ".d.ts") js

        logger.TimedStage (sprintf "Writing code files for library")

        let rootJS = JavaScriptPackager.packageLibraryBundle current c.JavaScriptExports O.JavaScript 
        let program, _ = rootJS |> WebSharper.Compiler.JavaScriptWriter.transformProgram O.JavaScript WebSharper.Core.JavaScript.Readable
        let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
        makeFile "index.js" js

        if dts then
            let rootJS = JavaScriptPackager.packageLibraryBundle current c.JavaScriptExports O.TypeScriptDeclaration 
            let program, _ = rootJS |> WebSharper.Compiler.JavaScriptWriter.transformProgram O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable
            let dts, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
            makeFile "index.d.ts" dts

        logger.TimedStage (sprintf "Writing index.js for library")

    | _ -> ()

/// The content type of a web resource file.
let WebResourceContentType (file: string) =
    if file.EndsWith(".js") || file.EndsWith(".mjs") || file.EndsWith(".cjs") || file.EndsWith(".ts") || file.EndsWith(".jsx") || file.EndsWith(".tsx") then
        "application/javascript"
    elif file.EndsWith(".css") then
        "text/css"
    else
        ""

/// Processes extra.files: copies files to output folders, and returns the files
/// to embed as web resources (name, content type, contents) if `embed` is set.
let HandleExtraFiles (logger: LoggerBase) (embed: bool) (assemblyName: string) (projectFile: string) (outputDir: string option) (isBundle: bool) = 
    let projectDir = Path.GetDirectoryName projectFile
    
    let asmOutputDir =
        outputDir |> Option.map (fun d ->
            if isBundle then
                d 
            else
                Path.Combine(d, "Scripts", "WebSharper", assemblyName)
        )

    if not embed && Option.isNone outputDir then [||] else
    match Extra.ProcessFiles projectDir outputDir asmOutputDir with
    | Some embeds ->
        logger.TimedStage "Processing extra.files"
        if embed && embeds.Length > 0 then
            let res =
                embeds |> Array.map (fun file ->
                    let contents =             
                        try
                            File.ReadAllBytes (System.IO.Path.Combine (projectDir, file))
                        with e ->
                            raise (exn (sprintf "Failed to read resource file %s: %s" file e.Message, e))
                    file, WebResourceContentType file, contents
                )
            logger.TimedStage "Reading web resources from extra.files"
            res
        else [||]
    | _ ->
        logger.TimedStage "Checking for extra.files"
        [||]

/// Represents a resource content file.
type ResourceContent =
    {
        Content : string
        ContentType : ContentTypes.ContentType
        Name : string
    }

/// A reduced resource context for simplified dependency rendering.
type ResourceContext =
    {
        /// Whether to emit readable JavaScript.
        DebuggingEnabled : bool

        /// Wheter to switch `//` links to `http://` links.
        DefaultToHttp : bool

        /// Reads environment settings.
        GetSetting : string -> option<string>

        /// Decides how to render a resource.
        RenderResource : ResourceContent -> Resources.Rendering

        /// Base URL path for WebSharper scripts.
        ScriptBaseUrl : option<string>
    }

let RenderDependencies(ctx: ResourceContext, writer: HtmlTextWriter, nameOfSelf, selfJS, deps: Resources.IResource list, lookupAssemblyCode, scriptBaseUrl) =
    let pU = WebSharper.PathConventions.PathUtility.VirtualPaths("/")
    let cache = Dictionary()
    let getRendering (content: ResourceContent) =
        match cache.TryGetValue(content) with
        | true, y -> y
        | _ ->
            let y = ctx.RenderResource(content)
            cache.Add(content, y)
            y
    let makeJsUri (name: WebSharper.PathConventions.AssemblyId) js =
        getRendering {
            Content = js
            ContentType = ContentTypes.Text.JavaScript
            Name =
                if ctx.DebuggingEnabled then
                    pU.JavaScriptPath(name)
                else
                    pU.MinifiedJavaScriptPath(name)
        }
    let ctx : Resources.Context =
        {
            DefaultToHttp = ctx.DefaultToHttp
            ScriptBaseUrl = ctx.ScriptBaseUrl
            GetSetting = ctx.GetSetting
            GetWebResourceRendering = fun ty name ->
                let (c, cT) = SidecarWebResources.ReadWebResource ty name
                getRendering {
                    Content = c
                    ContentType = cT
                    Name = name
                }
            WebRoot = "/" 
            RenderingCache = null
            ResourceDependencyCache = null
        }
    for d in deps do
        d.Render ctx (fun _ -> writer)
    Resources.HtmlTextWriter.WriteStartCode(writer, scriptBaseUrl)

/// In BundleOnly mode, output a dummy DLL to please MSBuild
let MakeDummyDll (path: string) (assemblyName: string) =
    StubAssembly.WriteEmpty path assemblyName

/// For Proxy project type, replaces the assembly with one having only its System.Reflection
/// and System.Runtime.Versioning assembly attributes and its embedded resources.
let WriteProxyAssembly (path: string) (keyFile: string option) =
    StubAssembly.WriteAttributesOnly path (fun ns -> ns = "System.Reflection" || ns = "System.Runtime.Versioning") keyFile
