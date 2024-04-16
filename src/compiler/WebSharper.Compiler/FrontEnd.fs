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

let ReadFromAssembly options a =
    match TryReadFromAssembly options a with
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

let GetJSLookup (r: Assembly list, readable) =
    r |> List.choose (fun a ->
        if readable then a.ReadableJavaScript else a.CompressedJavaScript
        |> Option.map (fun js -> a.FullName, js)
    )
    |> dict

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
        let sources = fileNames |> Array.map (fun (fn, key) -> key, File.ReadAllText fn)
        current, sources
    else
        transformAllSourcePositionsInMetadata assemblyName true current |> fst, [||]

let CreateBundleJSOutput (logger: LoggerBase) refMeta current entryPoint =

    //let pkg = 
    //    JavaScriptPackager.packageAssembly refMeta current entryPoint JavaScriptPackager.EntryPointStyle.OnLoadIfExists

    //if pkg = AST.Undefined then None else
    //    let getCodeWriter() = WebSharper.Core.JavaScript.Writer.CodeWriter()    

    //    let js, _ = pkg |> WebSharper.Compiler.JavaScriptPackager.exprToString WebSharper.Core.JavaScript.Readable getCodeWriter
    //    logger.TimedStage "Writing .js for bundle"
    //    let minJs, _ = pkg |> WebSharper.Compiler.JavaScriptPackager.exprToString WebSharper.Core.JavaScript.Compact getCodeWriter
    //    logger.TimedStage "Writing .min.js for bundle"

    //    Some (js, minJs)

    Some ("", "")

let CreateResources (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap dts ts closures (runtimeMeta: option<M.MetadataOptions * M.Info list>) (a: Mono.Cecil.AssemblyDefinition) isLibrary prebundle =
    let assemblyName = a.Name.Name
    let sourceMap = false // TODO what about source mapping with all the small files
    let currentPosFixed, sources =
        TransformMetaSources assemblyName current sourceMap
        
    let currentPosFixed =
        match comp with
        | Some c -> c.HideInternalProxies currentPosFixed
        | _ -> currentPosFixed
    
    logger.TimedStage "Source position transformations"

    let epStyle =
        if isLibrary then
            JavaScriptPackager.EntryPointStyle.LibraryBundle
        else
            JavaScriptPackager.EntryPointStyle.OnLoadIfExists

    let pkg = 
        if prebundle then
            [||]
        else
            JavaScriptPackager.packageAssembly O.JavaScript refMeta current assemblyName (comp |> Option.bind (fun c -> c.EntryPoint)) epStyle

    logger.TimedStage "Packaging assembly"
    
    let mapPkg f pkg =
        pkg |> Array.map (fun (n, p) -> n, p |> List.map f)    

    let pkg =
        match comp, closures with
        | Some comp, Some moveToTop ->
            let clPkg = pkg |> mapPkg (Closures.ExamineClosures(logger, comp, moveToTop).TransformStatement)
            logger.TimedStage "Closure analyzation"
            clPkg
        | _ -> pkg

    let res = ResizeArray()
    let resToHash = ResizeArray()
    
    let pkg =
        if sourceMap then
            pkg |> mapPkg (TransformSourcePositions(assemblyName).TransformStatement)
        else
            pkg |> mapPkg removeSourcePos.TransformStatement

    let addRes name path data = 
        //let rename p =
        //    let rec renameWithTag i =
        //        let name = p + "-" + string i
        //        if current.ResourceHashes.ContainsKey p then  
        //            renameWithTag (i + 1)
        //        else
        //            name
        //    if current.ResourceHashes.ContainsKey p then
        //        renameWithTag 1
        //    else 
        //        p
        
        match data with
        | Some d ->
            res.Add(name, d)
            //match path with
            //| Some p ->
            //    resToHash.Add(p, d)
            //| _ -> ()
        | None ->
            res.Add(name, [||])

    let rMeta =
        match runtimeMeta, comp with
        | Some (rm, refMetas), Some comp ->
            let trimmed = comp.ToRuntimeMetadata() |> M.ApplyMetadataOptions rm |> refreshVarFields
            let graph =
                DependencyGraph.Graph.FromData(
                    refMetas |> Seq.map (fun m -> m.Dependencies)
                    |> Seq.append [ trimmed.Dependencies ]
                )
            let meta = 
                refMetas |> Seq.map refreshAllIds
                |> Seq.append (Seq.singleton current)
                |> M.Info.UnionWithoutDependencies

            let bundles =
                if prebundle then
                    JavaScriptPackager.packageEntryPoint meta graph comp.AssemblyName
                    |> Seq.map (fun (bname, (bundleCode, addrMap)) ->
                        let program, _, trAddrMap = bundleCode |> WebSharper.Compiler.JavaScriptWriter.transformProgramAndAddrMap O.JavaScript WebSharper.Core.JavaScript.Readable addrMap
                        let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter program false
                        bname, js, trAddrMap
                    )
                    |> Array.ofSeq |> Some
                else
                    None
            let updated =
                {
                    trimmed with
                        Dependencies = 
                            graph.GetData() 
                        PreBundle = 
                            match bundles with
                            | Some bundles ->
                                bundles |> Seq.map (fun (bname, _, addrMap) -> bname, addrMap :> IDictionary<_,_>) |> dict
                            | _ -> Dictionary()
                }
            Some (updated, bundles)
        | _ -> None

    let addMeta() =
        
        for r in Assembly.GetAllResources a do
            let p = assemblyName + "/" + r.FileName
            let d = r.GetContentData()
            current.ResourceHashes.Add(p, AST.StableHash.data d)
            rMeta |> Option.iter (fun (rm, _) -> rm.ResourceHashes.Add(p, AST.StableHash.data d))

        for (p, d) in resToHash do
            try                                   
                current.ResourceHashes.Add(p, AST.StableHash.data d)
            with _ ->
                failwithf "Resource name collision: %s" p
            rMeta |> Option.iter (fun (rm, _) -> rm.ResourceHashes.Add(p, AST.StableHash.data d))

        logger.TimedStage "Hashing resources"

        let meta =
            use s = new MemoryStream(8 * 1024)
            M.IO.Encode s currentPosFixed
            s.ToArray()
        
        res.Add(EMBEDDED_METADATA, meta)

        match rMeta with
        | Some (rm, _) ->
            let erMeta = 
                use s = new MemoryStream(8 * 1024)
                M.IO.Encode s rm
                s.ToArray()

            res.Add(EMBEDDED_RUNTIME_METADATA, erMeta)
        | _ -> ()

        logger.TimedStage "Serializing metadata"

    let setAssemblyNode (i: M.Info) =
        i.Dependencies.Nodes |> Array.tryFindIndex (function
            | M.AssemblyNode (n, _, _) when n = assemblyName -> true
            | _ -> false
        ) |> Option.iter (fun asmNodeIndex ->
            i.Dependencies.Nodes.[asmNodeIndex] <- M.AssemblyNode (assemblyName, true, true)
        )
    
    let inline getBytes (x: string) = System.Text.Encoding.UTF8.GetBytes x

    match rMeta with
    | Some (_, Some bundles) ->
        for bname, bundleJs, _ in bundles do
            addRes (bname + ".js") (Some "") (Some (getBytes bundleJs))
    | _ -> ()

    if pkg.Length > 0 then
        
        let getCodeWriter() = 
            if sourceMap then
                WebSharper.Core.JavaScript.Writer.CodeWriter(sources)
            else WebSharper.Core.JavaScript.Writer.CodeWriter()    

        let pu = P.PathUtility.VirtualPaths("/")
        let ai = P.AssemblyId.Create(assemblyName)
        let shouldUseJSX = ResizeArray<string>()
        let jss' = 
            pkg 
            |> Array.map (fun (n, p) ->
                let program, jsx = p |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.JavaScript WebSharper.Core.JavaScript.Readable
                if jsx then
                    shouldUseJSX.Add n
                n, program, jsx
            )
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
                let js, map, isJSX = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable getCodeWriter jsxModifiedProgram jsx
                n, js, map, isJSX
            )
        for n, js, map, isJSX in jss do
            let x = if isJSX then "x" else ""
            addRes (n + ".js" + x) (Some (pu.JavaScriptFileName(ai))) (Some (getBytes js))
            map |> Option.iter (fun m ->
                addRes (n + ".map") None (Some (getBytes m)))
            logger.TimedStage (if sourceMap then sprintf "Writing %s.js and %s.map.js" n n else sprintf "Writing %s.js" n)
            //let minJs, minMap = p |> WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Compact getCodeWriter
            //addRes (n + ".min.js") (Some (pu.MinifiedJavaScriptFileName(ai))) (Some (getBytes minJs))
            //minMap |> Option.iter (fun m ->
            //    addRes (n + ".min.map") None (Some (getBytes m)))        
            //logger.TimedStage (if sourceMap then "Writing .min.js and .min.map.js" else "Writing .min.js")
        
        let resources = 
            match comp with
            | Some c -> c.Graph.GetResourcesOf c.Graph.Nodes
            | _ -> []

        if ts then
            let tspkg = 
                JavaScriptPackager.packageAssembly O.TypeScript refMeta current assemblyName (comp |> Option.bind (fun c -> c.EntryPoint)) epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            for (n, p) in tspkg do
                let ts, _, isJSX =
                    p
                    |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScript WebSharper.Core.JavaScript.Readable
                    |> fun (prog, jsx) ->
                        WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                let x = if isJSX then "x" else ""
                addRes (n + ".ts" + x) (Some (pu.TypeScriptFileName(ai))) (Some (getBytes ts))
            logger.TimedStage "Writing .ts files"

        if dts then
            let dtspkg = 
                JavaScriptPackager.packageAssembly O.TypeScriptDeclaration refMeta current assemblyName None epStyle
                |> Array.map (fun (f, ts) -> f, ts |> List.map removeSourcePos.TransformStatement)
            for (n, p) in dtspkg do
                let ts, _, _ =
                    p
                    |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.TypeScriptDeclaration WebSharper.Core.JavaScript.Readable
                    |> fun (prog, jsx) ->
                        WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter prog jsx
                addRes (n + ".d.ts") (Some (pu.TypeScriptDeclarationFileName(ai))) (Some (getBytes ts))
            logger.TimedStage "Writing .d.ts files"

        // set current AssemblyNode to be a module
        current.Dependencies.Nodes |> Array.tryFindIndex (function
            | M.AssemblyNode (n, _, _) when n = assemblyName -> true
            | _ -> false
        ) |> Option.iter (fun asmNodeIndex ->
            current.Dependencies.Nodes.[asmNodeIndex] <- M.AssemblyNode (assemblyName, true, true)
        )

        addMeta()
        Some jss, currentPosFixed, sources, res.ToArray()
    else
        // set current AssemblyNode to have no js
        current.Dependencies.Nodes |> Array.tryFindIndex (function
            | M.AssemblyNode (n, _, _) when n = assemblyName -> true
            | _ -> false
        ) |> Option.iter (fun asmNodeIndex ->
            current.Dependencies.Nodes.[asmNodeIndex] <- M.AssemblyNode (assemblyName, false, false)
        )

        addMeta()
        None, currentPosFixed, sources, res.ToArray()

let ModifyCecilAssembly (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap dts ts closures runtimeMeta (a: Mono.Cecil.AssemblyDefinition) isLibrary prebundle =
    let jsOpt, currentPosFixed, sources, res = CreateResources logger comp refMeta current sourceMap dts ts closures runtimeMeta a isLibrary prebundle
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    for name, contents in res do
        Mono.Cecil.EmbeddedResource(name, pub, contents)
        |> a.MainModule.Resources.Add
    jsOpt, currentPosFixed, sources, res

let ModifyAssembly (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap dts ts closures runtimeMeta (assembly : Assembly) isLibrary =
    ModifyCecilAssembly logger comp refMeta current sourceMap dts ts closures runtimeMeta assembly.Raw isLibrary

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
            DebuggingEnabled = ctx.DebuggingEnabled
            DefaultToHttp = ctx.DefaultToHttp
            ScriptBaseUrl = ctx.ScriptBaseUrl
            GetAssemblyRendering = fun name ->
                if name = nameOfSelf then
                    selfJS
                    |> makeJsUri (WebSharper.PathConventions.AssemblyId.Create name)
                else
                    match lookupAssemblyCode name with
                    | Some x -> makeJsUri (WebSharper.PathConventions.AssemblyId.Create name) x
                    | None -> Resources.Skip
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
