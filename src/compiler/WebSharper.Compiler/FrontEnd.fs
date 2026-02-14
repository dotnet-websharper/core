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
module B = WebSharper.Core.Binary
module P = WebSharper.PathConventions

type Assembly = WebSharper.Compiler.Assembly
type Content = WebSharper.Compiler.Content
type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
type Loader = WebSharper.Compiler.Loader
type Symbols = WebSharper.Compiler.Symbols
type O = WebSharper.Core.JavaScript.Output

let TryReadFromAssembly options (a: Assembly) =
    a.Raw.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = EMBEDDED_METADATA -> Some r 
        | _ -> None
    )
    |> Option.map (fun r ->
        use s = r.GetResourceStream()
        let m = 
            try Ok (M.IO.Decode s)
            with e -> Error (sprintf "Failed to deserialize metadata for %s. Error: %s" a.FullName e.Message)
        m |> Result.map (M.ApplyMetadataOptions options)
    )

let TryReadRuntimeFromAssembly options (a: Assembly) =
    a.Raw.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = EMBEDDED_RUNTIME_METADATA -> Some r 
        | _ -> None
    )
    |> Option.map (fun r ->
        use s = r.GetResourceStream()
        let m = 
            try Ok (M.IO.Decode s)
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

let ModifyWIGAssembly (current: M.Info) (a: Mono.Cecil.AssemblyDefinition) =
    let assemblyName = a.Name.Name
    for r in Assembly.GetAllResources a do
        let p = assemblyName + "/" + r.FileName
        let d = r.GetContentData()
        current.ResourceHashes.Add(p, AST.StableHash.data d)
    let meta =
        use s = new MemoryStream(8 * 1024)
        M.IO.Encode s current
        s.ToArray()
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, meta)
    |> a.MainModule.Resources.Add

let ModifyTSAssembly (current: M.Info) (a: Assembly) =
    ModifyWIGAssembly current a.Raw
    
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
    }

[<RequireQualifiedAccess>]
type CreateResourcesFor =
    | Assembly of Mono.Cecil.AssemblyDefinition
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

//let CreateResources (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap dts ts dce closures (runtimeMeta: option<M.MetadataOptions * M.Info list>) (a: Mono.Cecil.AssemblyDefinition) (refAssemblies: Assembly list) isLibrary prebundle isSitelet =
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
        | CreateResourcesFor.Assembly a -> a.Name.Name
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
                    let bundles =
                        JavaScriptPackager.packageEntryPoint meta graph comp.AssemblyName output
                        |> Seq.map (fun (bname, (bundleCode, addrMap)) ->
                            let bundleCode =
                                match closures with
                                | Some moveToTop ->
                                    let cla = bundleCode |> List.map (Closures.ExamineClosures(logger, comp, moveToTop).TransformStatement)
                                    logger.TimedStage (sprintf "Closure analyzation for %s" bname)
                                    cla
                                | _ -> bundleCode
                            let program, _, trAddrMap = bundleCode |> WebSharper.Compiler.JavaScriptWriter.transformProgramAndAddrMap output WebSharper.Core.JavaScript.Readable addrMap
                            let codeWriter = 
                                WebSharper.Core.JavaScript.Writer.CodeWriter(sourceMap)
                            let js, map, isJsx = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable (fun () -> codeWriter) program false
                            let x = if isJsx then "x" else ""
                            if sourceMap then
                                codeWriter.GetSourceFiles() |> Array.iter (sourcesUsed.Add >> ignore)
                                addRes (bname + ext + x + ".map") map.Value
                            logger.TimedStage (sprintf "Writing prebundle %s.js%s" bname x)
                            bname, bname + ext + x, js, trAddrMap
                        )
                        |> Array.ofSeq
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
        let shouldUseJSX = ResizeArray<string>()
        let jss' = 
            pkg 
            |> Array.map (fun (n, p) ->
                let program, jsx = p |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.JavaScript WebSharper.Core.JavaScript.Readable
                if jsx then
                    shouldUseJSX.Add n
                n, program, jsx
            )
        let sourcesUsed = HashSet()
        let jss = 
            jss' |> Array.map (fun (n, p, jsx) ->
                let jsxModifiedProgram =
                    p
                    |> List.map (fun st ->
                        match st with
                        | JavaScript.Syntax.Statement.Import(di, fi, ni, mn) ->
                            if shouldUseJSX.Exists(fun x -> mn.EndsWith(x + ".js")) then
                                JavaScript.Syntax.Statement.Import(di, fi, ni, mn + "x")
                            else
                                st
                        | _ -> st
                    )
                let codeWriter = 
                    WebSharper.Core.JavaScript.Writer.CodeWriter(sourceMap)

                let js, map, isJSX = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable (fun () -> codeWriter) jsxModifiedProgram jsx
                if sourceMap then
                    codeWriter.GetSourceFiles() |> Array.iter (sourcesUsed.Add >> ignore)
                n, js, map, isJSX
            )
        if sourceMap then
            sources |> Array.filter (fun (n, s) -> sourcesUsed.Contains n) 
            |> Array.iter (fun (name, contents) ->
                addRes name contents
            )

        for n, js, map, isJSX in jss do
            let x = if isJSX then "x" else ""
            addRes (n + ".js" + x) js
            map |> Option.iter (fun m ->
                addRes (n + ".js.map") m)
            logger.TimedStage (if sourceMap then sprintf "Writing %s.js and %s.js.map.js" n n else sprintf "Writing %s.js" n)
            //let minJs, minMap = p |> WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Compact getCodeWriter
            //addRes (n + ".min.js") (Some (pu.MinifiedJavaScriptFileName(ai))) (Some (getBytes minJs))
            //minMap |> Option.iter (fun m ->
            //    addRes (n + ".min.map") None (Some (getBytes m)))        
            //logger.TimedStage (if sourceMap then "Writing .min.js and .min.map.js" else "Writing .min.js")
        
        if ts then
            let tspkg = 
                JavaScriptPackager.packageAssembly O.TypeScript refMeta current assemblyName dce (comp |> Option.bind (fun c -> c.EntryPoint)) epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            for (n, p) in tspkg do
                let ts, _, isJSX =
                    p
                    |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScript WebSharper.Core.JavaScript.Readable
                    |> fun (prog, jsx) ->
                        WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                let x = if isJSX then "x" else ""
                addRes (n + ".ts" + x) ts
            logger.TimedStage "Writing .ts files"

        if dts then
            let dtspkg = 
                JavaScriptPackager.packageAssembly O.TypeScriptDeclaration refMeta current assemblyName dce None epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            for (n, p) in dtspkg do
                let ts, _, _ =
                    p
                    |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable
                    |> fun (prog, jsx) ->
                        WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                addRes (n + ".d.ts") ts
            logger.TimedStage "Writing .d.ts files"

    match resFor with
    | CreateResourcesFor.Assembly a ->
        let mutable neededHashing = false
        for r in Assembly.GetAllResources a do
            let p = assemblyName + "/" + r.FileName
            let d = r.GetContentData()
            currentPosFixed.ResourceHashes.Add(p, AST.StableHash.data d)
            rMeta |> Option.iter (fun (rm, _) -> rm.ResourceHashes.Add(p, AST.StableHash.data d))
            neededHashing <- true
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

let ConvertResourcesToBinary (res: GeneratedResources) =

    let binaries = ResizeArray()

    for name, content in res.AllTextFiles do
        binaries.Add(name, System.Text.Encoding.UTF8.GetBytes content)

    let endodeMeta meta =
        use s = new MemoryStream(8 * 1024)
        M.IO.Encode s meta
        s.ToArray()

    binaries.Add(EMBEDDED_METADATA, endodeMeta res.Metadata)

    res.RuntimeMetadata |> Option.iter (fun rm -> binaries.Add(EMBEDDED_RUNTIME_METADATA, endodeMeta rm))

    binaries.ToArray()

let ModifyCecilAssembly (args: CreateResourcesArgs) (assemblyDef: Mono.Cecil.AssemblyDefinition) =
    let res = CreateResources args (CreateResourcesFor.Assembly assemblyDef)
    let binaries = ConvertResourcesToBinary res
    args.Logger.TimedStage "Serializing metadata"
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    for name, contents in binaries do
        Mono.Cecil.EmbeddedResource(name, pub, contents)
        |> assemblyDef.MainModule.Resources.Add
    res

let ModifyAssembly (args: CreateResourcesArgs) (assembly : Assembly) =
    ModifyCecilAssembly args assembly.Raw

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

let AddExtraAssemblyReferences (wsrefs: Assembly seq) (assembly : Assembly) =
    let a = assembly.Raw
    let currentRefs =
        a.MainModule.AssemblyReferences |> Seq.map (fun r -> r.Name)   
        |> System.Collections.Generic.HashSet
    wsrefs 
    |> Seq.iter (fun r -> 
        let n = r.Raw.Name
        if not (currentRefs.Contains n.Name) then
            a.MainModule.AssemblyReferences.Add n
    )

let AddWebResourceAnnotations (assembly : Assembly) (projectDir: string) (files: string[]) =

    let a = assembly.Raw

    let webResourceCtor =
        lazy
            typeof<WebSharper.WebResourceAttribute>.GetConstructor([| typeof<string>; typeof<string> |])
            |> a.MainModule.ImportReference

    for file in files do
        let attr = Mono.Cecil.CustomAttribute(webResourceCtor.Value)
        attr.ConstructorArguments.Add(Mono.Cecil.CustomAttributeArgument(a.MainModule.TypeSystem.String, file))
        let mime =
            if file.EndsWith(".js") || file.EndsWith(".mjs") || file.EndsWith(".cjs") || file.EndsWith(".ts") || file.EndsWith(".jsx") || file.EndsWith(".tsx") then
                "application/javascript"
            elif file.EndsWith(".css") then
                "text/css"
            else
                ""
        attr.ConstructorArguments.Add(Mono.Cecil.CustomAttributeArgument(a.MainModule.TypeSystem.String, mime))
        a.CustomAttributes.Add(attr)
        
        let contents =             
            try
                File.ReadAllBytes (System.IO.Path.Combine (projectDir, file))
            with e ->
                raise (exn (sprintf "Failed to read resource file %s: %s" file e.Message))

        a.MainModule.Resources.Add(Mono.Cecil.EmbeddedResource(file, Mono.Cecil.ManifestResourceAttributes.Public, contents))

let HandleExtraFiles (assembly : Assembly option) (assemblyName: string) (projectFile: string) (outputDir: string option) (isBundle: bool) = 
    let projectDir = Path.GetDirectoryName projectFile
    
    let asmOutputDir =
        outputDir |> Option.map (fun d ->
            if isBundle then
                Path.Combine(d, assemblyName) 
            else
                Path.Combine(d, "Scripts", "WebSharper", assemblyName)
        )
    match assembly, outputDir with
    | None, None -> ()
    | _ ->
        let embeds = Extra.ProcessFiles projectDir outputDir asmOutputDir
    
        match assembly with
        | Some asm ->
            AddWebResourceAnnotations asm projectDir embeds
        | _ -> ()

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
                let (c, cT) = Utility.ReadWebResource ty name
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
    let aND = Mono.Cecil.AssemblyNameDefinition(assemblyName, System.Version())
    let asm = Mono.Cecil.AssemblyDefinition.CreateAssembly(aND, assemblyName, Mono.Cecil.ModuleKind.Dll)
    asm.Write(path)

/// For Proxy project type, erase all IL assembly contents except System.Reflection attributes
let EraseAssemblyContents (assembly : Assembly) =
    let asm = assembly.Raw
    let attrs = asm.CustomAttributes.ToArray()
    asm.CustomAttributes.Clear()
    asm.EntryPoint <- null
    asm.Modules.Clear()
    asm.MainModule.Types.Clear()
    for a in attrs do
        match a.AttributeType.Namespace with
        | "System.Reflection"
        | "System.Runtime.Versioning" ->
            asm.CustomAttributes.Add(a)
        | _ ->
            ()
