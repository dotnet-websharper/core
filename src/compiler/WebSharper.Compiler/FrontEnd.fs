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
module M = WebSharper.Core.Metadata
module B = WebSharper.Core.Binary
module P = WebSharper.PathConventions

type Assembly = WebSharper.Compiler.Assembly
type Content = WebSharper.Compiler.Content
type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
type Loader = WebSharper.Compiler.Loader
type Symbols = WebSharper.Compiler.Symbols

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
        let current, fileNames = transformAllSourcePositionsInMetadata assemblyName current
        let sources = fileNames |> Array.map (fun (fn, key) -> key, File.ReadAllText fn)
        current, sources
    else
        removeSourcePositionFromMetadata current, [||]

let CreateBundleJSOutput (logger: LoggerBase) refMeta current entryPoint =

    let pkg = 
        Packager.packageAssembly refMeta current entryPoint Packager.EntryPointStyle.OnLoadIfExists

    if pkg = AST.Undefined then None else
        let getCodeWriter() = WebSharper.Core.JavaScript.Writer.CodeWriter()    

        let js, _ = pkg |> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Readable getCodeWriter
        logger.TimedStage "Writing .js for bundle"
        let minJs, _ = pkg |> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Compact getCodeWriter
        logger.TimedStage "Writing .min.js for bundle"

        Some (js, minJs)

let CreateResources (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap closures (runtimeMeta: option<M.MetadataOptions * M.Info list>) (a: Mono.Cecil.AssemblyDefinition) =
    let assemblyName = a.Name.Name
    let currentPosFixed, sources =
        TransformMetaSources assemblyName current sourceMap
    
    logger.TimedStage "Source position transformations"

    let pkg = 
        Packager.packageAssembly refMeta current (comp |> Option.bind (fun c -> c.EntryPoint)) Packager.EntryPointStyle.OnLoadIfExists

    logger.TimedStage "Packaging assembly"
    
    let pkg =
        match comp, closures with
        | Some comp, Some moveToTop ->
            let clPkg = pkg |> Closures.ExamineClosures(logger, comp, moveToTop).TransformExpression 
            logger.TimedStage "Closure analyzation"
            clPkg
        | _ -> pkg

    let res = ResizeArray()
    let resToHash = ResizeArray()
    
    let pkg =
        if sourceMap then
            TransformSourcePositions(assemblyName).TransformExpression pkg
        else
            removeSourcePos.TransformExpression pkg

    let addRes name path data = 
        match data with
        | Some d ->
            res.Add(name, d)
            match path with
            | Some p ->
                resToHash.Add(p, d)
            | _ -> ()
        | None ->
            res.Add(name, [||])

    let rMeta =
        match runtimeMeta, comp with
        | Some (rm, refMetas), Some comp ->
            let trimmed = comp.ToRuntimeMetadata() |> M.ApplyMetadataOptions rm 
            {
                trimmed with
                    Dependencies = 
                        DependencyGraph.Graph.FromData(
                            refMetas |> Seq.map (fun m -> m.Dependencies)
                            |> Seq.append [ trimmed.Dependencies ]
                        ).GetData()
            }
            
            |> Some
        | _ -> None

    let addMeta() =
        
        for r in Assembly.GetAllResources a do
            let p = assemblyName + "/" + r.FileName
            let d = r.GetContentData()
            current.ResourceHashes.Add(p, AST.StableHash.data d)
            rMeta |> Option.iter (fun rm -> rm.ResourceHashes.Add(p, AST.StableHash.data d))

        for (p, d) in resToHash do
            current.ResourceHashes.Add(p, AST.StableHash.data d)
            rMeta |> Option.iter (fun rm -> rm.ResourceHashes.Add(p, AST.StableHash.data d))

        logger.TimedStage "Hashing resources"

        let meta =
            use s = new MemoryStream(8 * 1024)
            M.IO.Encode s currentPosFixed
            s.ToArray()
        
        res.Add(EMBEDDED_METADATA, meta)

        match rMeta with
        | Some rm ->
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
    
    if pkg <> AST.Undefined then
        
        let getCodeWriter() = 
            if sourceMap then
                WebSharper.Core.JavaScript.Writer.CodeWriter(sources)
            else WebSharper.Core.JavaScript.Writer.CodeWriter()    

        let pu = P.PathUtility.VirtualPaths("/")
        let ai = P.AssemblyId.Create(assemblyName)
        let inline getBytes (x: string) = System.Text.Encoding.UTF8.GetBytes x
        let js, map = pkg |> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Readable getCodeWriter
        addRes EMBEDDED_JS (Some (pu.JavaScriptFileName(ai))) (Some (getBytes js))
        map |> Option.iter (fun m ->
            addRes EMBEDDED_MAP None (Some (getBytes m)))
        logger.TimedStage (if sourceMap then "Writing .js and .map.js" else "Writing .js")
        let minJs, minMap = pkg |> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Compact getCodeWriter
        addRes EMBEDDED_MINJS (Some (pu.MinifiedJavaScriptFileName(ai))) (Some (getBytes minJs))
        minMap |> Option.iter (fun m ->
            addRes EMBEDDED_MINMAP None (Some (getBytes m)))
        logger.TimedStage (if sourceMap then "Writing .min.js and .min.map.js" else "Writing .min.js")

        let isJSModule =
            match pkg with
            | AST.Sequential _ -> true
            | _ -> false

        if isJSModule then
            // set current AssemblyNode to be a module
            current.Dependencies.Nodes |> Array.tryFindIndex (function
                | M.AssemblyNode (n, _, _) when n = assemblyName -> true
                | _ -> false
            ) |> Option.iter (fun asmNodeIndex ->
                current.Dependencies.Nodes.[asmNodeIndex] <- M.AssemblyNode (assemblyName, true, true)
            )

        addMeta()
        Some (js, minJs), currentPosFixed, sources, res.ToArray()
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

let ModifyCecilAssembly (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap closures runtimeMeta (a: Mono.Cecil.AssemblyDefinition) =
    let jsOpt, currentPosFixed, sources, res = CreateResources logger comp refMeta current sourceMap closures runtimeMeta a
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    for name, contents in res do
        Mono.Cecil.EmbeddedResource(name, pub, contents)
        |> a.MainModule.Resources.Add
    jsOpt, currentPosFixed, sources

let ModifyAssembly (logger: LoggerBase) (comp: Compilation option) (refMeta: M.Info) (current: M.Info) sourceMap closures runtimeMeta (assembly : Assembly) =
    ModifyCecilAssembly logger comp refMeta current sourceMap closures runtimeMeta assembly.Raw

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
    asm.MainModule.Resources.Clear()
    asm.MainModule.Types.Clear()
    for a in attrs do
        match a.AttributeType.Namespace with
        | "System.Reflection"
        | "System.Runtime.Versioning" ->
            asm.CustomAttributes.Add(a)
        | _ ->
            ()
