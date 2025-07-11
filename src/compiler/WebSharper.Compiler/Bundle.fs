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

namespace WebSharper.Compiler

open WebSharper.Core.AST
open System.Collections.Generic
open WebSharper.Constants

module CT = WebSharper.Core.ContentTypes
module JS = WebSharper.Core.JavaScript.Syntax
module M = WebSharper.Core.Metadata
module Res = WebSharper.Core.Resources
module W = WebSharper.Core.JavaScript.Writer
module FE = WebSharper.Compiler.FrontEnd
type O = WebSharper.Core.JavaScript.Output
type Graph = WebSharper.Core.DependencyGraph.Graph

[<AutoOpen>]
module BundleUtility =

    type BundleMode =
        | CSS = 0
        | HtmlHeaders = 1
        | JavaScript = 2
        | MinifiedJavaScript = 3
        | TypeScript = 4

    let DocWrite w =
        let str x = JS.Constant (JS.String x)
        JS.Application (JS.Binary (JS.Var (JS.Id.New "document"), JS.BinaryOperator.``.``, str "write"), [], [str w])
        |> W.ExpressionToString WebSharper.Core.JavaScript.Preferences.Compact

module Bundling =
    
    open WebSharper.Compiler.CommandTools
    open ExecuteCommands

    type Bundle =
        {
            Css: option<Content>
            HeadHtml: option<Content>
            HeadJs: option<Content>
            Js: Content
            JsMap: option<Content>
            MinJs: Content
            MinJsMap: option<Content>
            Imports: Lazy<list<string * list<string>>>
            Sources: Lazy<list<string * string>>
        }

    type private BundleOptions =
        {
            Config: WsConfig
            RefAssemblies: list<Assembly>
            RefMetas: list<M.Info>
            CurrentMeta: M.Info
            GetAllDeps: Graph -> list<M.Node>
            CurrentJs: Lazy<option<string * string>>
            Sources: seq<string * string>
            EntryPoint: option<Statement>
            EntryPointStyle: JavaScriptPackager.EntryPointStyle
            IsExtraBundle: bool
            AddError : option<SourcePos> -> string -> unit
        }

    let private CreateBundle (o: BundleOptions) (logger: LoggerBase) =
        let failf format =
            Printf.kprintf (o.AddError None) format

        let sourceMap = o.Config.SourceMap
        let dce = o.Config.DeadCodeElimination
        
        let graph =
            o.RefMetas |> Seq.map (fun m -> m.Dependencies)
            |> Seq.append (Seq.singleton o.CurrentMeta.Dependencies)
            |> WebSharper.Core.DependencyGraph.Graph.FromData

        let allSources = 
            if sourceMap then
                o.RefAssemblies |> Seq.collect (fun a ->
                    a.GetSources() |> Seq.map (fun s -> s.EmbeddedFileName.Replace(EMBEDDED_SOURCES, ""), s.Content)
                )  
                |> Seq.append o.Sources
                |> Array.ofSeq 
            else [||]

        let mutable map = None
        let mutable minmap = None
        let mutable sources = Dictionary<string, string>()

        // if DCE and sourcemapping are both off, opt for quicker way of just concatenating assembly js outputs
        let concatScripts = false //not dce && not sourceMap
        if concatScripts then
            sprintf "Using pre-compiled JavaScript for bundling"
            |> logger.Out

        let htmlHeadersContext : Res.Context =
            {
                DefaultToHttp = false
                ScriptBaseUrl = o.Config.ScriptBaseUrl
                GetSetting = fun _ -> None
                GetWebResourceRendering = fun _ _-> Res.Skip
                WebRoot = "/"
                RenderingCache = null
                ResourceDependencyCache = null
            }

        let nodes = o.GetAllDeps graph

        let pkg =   
            if concatScripts then
                []
            else
                let meta = 
                    o.RefMetas |> Seq.map refreshAllIds
                    |> Seq.append (Seq.singleton o.CurrentMeta)
                    |> M.Info.UnionWithoutDependencies
                try
                    let current = 
                        if dce <> Some false then trimMetadata meta nodes 
                        else meta
                    let asmName = Path.GetFileNameWithoutExtension o.Config.AssemblyFile
                    JavaScriptPackager.bundleAssembly O.JavaScript current current asmName o.EntryPoint o.EntryPointStyle
                with e -> 
                    CommandTools.argError ("Error during bundling: " + e.Message + " at " + e.StackTrace)
        let resources = graph.GetResourcesOf nodes

        let noHtmlWriter = new HtmlTextWriter(TextWriter.Null)

        let assemblyLookup =
            lazy 
            o.RefAssemblies |> Seq.map (fun a -> a.Name, a) |> Map

        let jsImports = ResizeArray()
        let toLoad = ResizeArray()

        let render (mode: BundleMode) (writer: StringWriter) =
            match mode with
            | BundleMode.HtmlHeaders -> 
                use htmlHeadersWriter =
                    match mode with
                    | BundleMode.HtmlHeaders -> new HtmlTextWriter(writer)
                    | _ -> new HtmlTextWriter(TextWriter.Null)
                for d in resources do
                    d.Render htmlHeadersContext (fun _ -> htmlHeadersWriter)

            | _ -> 
                let renderWebResource cType (c: string) =
                    match cType, mode with
                    //| CT.JavaScript, BundleMode.JavaScript
                    //| CT.JavaScript, BundleMode.MinifiedJavaScript ->
                    //    writer.Write(c)
                    //    writer.WriteLine(";")
                    | CT.Css, BundleMode.CSS ->
                        writer.WriteLine(c)
                    | _ -> ()
                let ctx : Res.Context =
                    {
                        DefaultToHttp = false // TODO make configurable
                        ScriptBaseUrl = o.Config.ScriptBaseUrl
                        GetSetting = fun _ -> None
                        GetWebResourceRendering = fun ty name ->
                            if name.ToLower().EndsWith ".js" && (mode = BundleMode.JavaScript || mode = BundleMode.MinifiedJavaScript) then
                                let a = ty.Assembly.GetName().Name
                                let i = a, name 
                                if not (jsImports.Contains(i)) && i <> ("WebSharper.Core.JavaScript", "Runtime.js") then
                                    let url = a + "/" + name
                                        //if o.IsExtraBundle then 
                                        //    "../" + a + "/" + name
                                        //else
                                        //    "./" + a + "/" + name
                                    if not (toLoad.Contains(url)) then
                                        toLoad.Add(url)
                                    //let s = W.ExpressionToString WebSharper.Core.JavaScript.Preferences.Compact !~(JS.String url)                                    
                                    //writer.WriteLine("import {0};", s.Trim())  
                                    jsImports.Add(i)
                            else
                                let (c, cT) = Utility.ReadWebResource ty name
                                renderWebResource cT c
                            Res.Skip
                        WebRoot = "/"
                        RenderingCache = null
                        ResourceDependencyCache = null
                    }
                for d in resources do
                    d.Render ctx (fun _ -> noHtmlWriter)
                    if o.IsExtraBundle then
                        match d with
                        | :? Res.IExternalScriptResource as e ->
                            match e.Urls ctx with
                            | [||] -> ()
                            | urls ->
                                for url in urls do
                                    if not (toLoad.Contains(url)) then
                                        //let s = W.ExpressionToString WebSharper.Core.JavaScript.Preferences.Compact !~(JS.String url)                                    
                                        //writer.WriteLine("import {0};", s.Trim())  
                                        toLoad.Add(url)

                        | _ -> ()

            if concatScripts then 
                match mode with
                | BundleMode.JavaScript -> 
                    o.CurrentJs.Value |> Option.iter (fun (t, _) -> writer.WriteLine(t))
                    Res.HtmlTextWriter.WriteStartCode(writer, o.Config.ScriptBaseUrl, false, o.IsExtraBundle)
                | BundleMode.MinifiedJavaScript ->
                    o.CurrentJs.Value |> Option.iter (fun (_, t) -> writer.WriteLine(t))
                    Res.HtmlTextWriter.WriteStartCode(writer, o.Config.ScriptBaseUrl, false, o.IsExtraBundle)
                | _ -> ()
            else
                match mode with
                | BundleMode.JavaScript | BundleMode.MinifiedJavaScript ->
            
                    let pref =
                        if mode = BundleMode.JavaScript then 
                            WebSharper.Core.JavaScript.Readable
                        else 
                            WebSharper.Core.JavaScript.Compact

                    let codeWriter =
                        if sourceMap then
                            WebSharper.Core.JavaScript.Writer.CodeWriter(true, true)
                        else WebSharper.Core.JavaScript.Writer.CodeWriter()    

                    let scriptBase = o.Config.ScriptBaseUrl |> Option.defaultValue ""

                    let js, m, isJSX = 
                        pkg 
                        |> WebSharper.Compiler.JavaScriptPackager.addLoadedModules (List.ofSeq toLoad) scriptBase o.IsExtraBundle
                        |> WebSharper.Compiler.JavaScriptPackager.transformProgramWithJSX O.JavaScript pref 
                        |> fun (program, jsx) ->
                            WebSharper.Compiler.JavaScriptPackager.programToString pref (fun () -> codeWriter) program jsx
                    if sourceMap then
                        if mode = BundleMode.JavaScript then
                            map <- m
                        else
                            minmap <- m

                    let sourcesFound = codeWriter.GetSourceFiles() |> HashSet

                    for (name, content) in allSources do
                        if sourcesFound.Contains name then
                            sources[name] <- content

                    writer.WriteLine js

                    //Res.HtmlTextWriter.WriteStartCode(writer, o.Config.ScriptBaseUrl, false, o.IsExtraBundle)
                | _ -> ()

        let content mode =
            let t =
                lazy
                use w = new StringWriter()
                render mode w
                w.ToString()
            Content.Create(t)

        let css = content BundleMode.CSS
        let htmlHeaders = content BundleMode.HtmlHeaders
        let javaScript = content BundleMode.JavaScript
        let minifiedJavaScript = content BundleMode.MinifiedJavaScript
        let javaScriptHeaders = htmlHeaders.Map(DocWrite)

        let mapping =
            if sourceMap then 
                let t = 
                    lazy
                    javaScript.Text |> ignore
                    map.Value
                Some (Content.Create(t))
            else None

        let minifiedMapping =
            if sourceMap then 
                let t = 
                    lazy
                    minifiedJavaScript.Text |> ignore
                    minmap.Value
                Some (Content.Create(t))
            else None

        let imports =
            lazy
            pkg 
            |> JavaScriptPackager.getImportedModules 
            |> List.choose (fun i ->
                match i.Split('/') with
                | [| "."; a; js |] ->
                    Some (a, js)
                | _ -> 
                    None
            )
            |> List.append (List.ofSeq jsImports)
            |> List.groupBy fst
            |> List.map (fun (a, jss) -> a, jss |> List.map snd)

        let sources =
            lazy
            if sourceMap then
                javaScript |> ignore
                sources |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList
            else []

        {
            Css = Some css
            HeadHtml = Some htmlHeaders
            HeadJs = Some javaScriptHeaders
            Js = javaScript
            JsMap = mapping
            MinJs = minifiedJavaScript
            MinJsMap = minifiedMapping
            Imports = imports
            Sources = sources
        }

    let private (==) s1 s2 =
        System.String.Equals(s1, s2, System.StringComparison.OrdinalIgnoreCase)

    let AddExtraBundles config (logger: LoggerBase) refMetas (currentMeta: M.Info) (refAssemblies: list<Assembly>) (comp: Compilation) (assem: Choice<string, Assembly>) =
        let config =
            { config with
                SourceMap = false // TODO make SourceMap work with this
                DeadCodeElimination = Some true
                ScriptBaseUrl = None
            }
        let pub = Mono.Cecil.ManifestResourceAttributes.Public
        let addWebResourceAttribute =
            match assem with
            | Choice1Of2 _ -> ignore
            | Choice2Of2 assem ->
            let strTy =
                let std = refAssemblies |> List.find (fun ar -> ar.Name == "netstandard")
                Mono.Cecil.TypeReference("System", "String", std.Raw.MainModule, std.Raw.MainModule) |> assem.Raw.MainModule.ImportReference
            let webResourceTy =
                let wsCoreJs = refAssemblies |> List.find (fun ar -> ar.Name == "WebSharper.Core")
                Mono.Cecil.TypeReference("WebSharper", "WebResourceAttribute", wsCoreJs.Raw.MainModule, wsCoreJs.Raw.MainModule) |> assem.Raw.MainModule.ImportReference
            let ctor = Mono.Cecil.MethodReference(".ctor", webResourceTy, webResourceTy) |> assem.Raw.MainModule.ImportReference
            ctor.Parameters.Add(Mono.Cecil.ParameterDefinition(strTy))
            ctor.Parameters.Add(Mono.Cecil.ParameterDefinition(strTy))
            fun name ->
                let attr = Mono.Cecil.CustomAttribute(ctor)
                attr.ConstructorArguments.Add(Mono.Cecil.CustomAttributeArgument(strTy, name))
                attr.ConstructorArguments.Add(Mono.Cecil.CustomAttributeArgument(strTy, "application/javascript"))
                assem.Raw.CustomAttributes.Add(attr)
        let assemName = match assem with Choice1Of2 n -> n | Choice2Of2 a -> a.Name
        [
            for KeyValue(bname, bundle) in comp.CompiledExtraBundles do
                let bname = assemName + "." + bname
                let jsExports = if bundle.IncludeJsExports then comp.JavaScriptExports else []
                let bundle =
                    logger 
                    |> CreateBundle {
                    Config = config
                    RefMetas = refMetas
                    CurrentMeta = currentMeta
                    GetAllDeps = FrontEnd.GetDepsFromJSExports jsExports bundle.Node
                    EntryPoint = Some bundle.EntryPoint
                    EntryPointStyle = JavaScriptPackager.EntryPointStyle.ForceImmediate
                    CurrentJs = lazy None
                    Sources = []
                    RefAssemblies = refAssemblies
                    IsExtraBundle = true
                    AddError = fun pos msg -> comp.AddError(pos, SourceError msg)
                }
                let bundleFiles =
                    [
                        yield "../workers/" + bname + ".js", bundle.Js
                        if bundle.JsMap.IsSome then
                            yield "../workers/" + bname + ".map", bundle.JsMap.Value
                    ]
                yield! bundleFiles
                match assem with
                | Choice1Of2 _ -> ()
                | Choice2Of2 assem ->
                    bundleFiles
                    |> List.iter (fun (name, contents) ->
                        let bytes = System.Text.Encoding.UTF8.GetBytes contents.Text
                        Mono.Cecil.EmbeddedResource(name, pub, bytes)
                        |> assem.Raw.MainModule.Resources.Add
                        addWebResourceAttribute name
                    )
        ]

    let WriteBundle (config: WsConfig) (bundle: Bundle) =
        let fileName = Path.GetFileNameWithoutExtension config.AssemblyFile
        let mutable hasOutput = false

        match BundleOutputDir config (GetWebRoot config) with
        | Some outputDir ->
            hasOutput <- true
            System.IO.Directory.CreateDirectory outputDir |> ignore
            let write (ext: string) (c: Content) =
                c.WriteFile(Path.Combine(outputDir, fileName + ext))
            let writeMapped (ext: string) (c: Content) m =
                write ext c
                m |> Option.iter (fun mc ->
                    let mapExt = ext + ".map"
                    write mapExt mc
                    File.AppendAllLines(
                        Path.Combine(outputDir, fileName + ext),
                        [| "//# sourceMappingURL=" + fileName + mapExt |]
                    )
                )

            Option.iter (write ".css") bundle.Css
            Option.iter (write ".head.html") bundle.HeadHtml
            Option.iter (write ".head.js") bundle.HeadJs
            writeMapped ".js" bundle.Js bundle.JsMap
            writeMapped ".min.js" bundle.MinJs bundle.MinJsMap
            for (f, s) in bundle.Sources.Value do
                let loc = Path.Combine(outputDir, "Source", f)
                System.IO.Directory.CreateDirectory(Path.GetDirectoryName(loc)) |> ignore 
                File.WriteAllText(loc, s)    

        | None -> ()

        match config.ProjectType, config.JSOutputPath with
        | Some BundleOnly, Some path ->
            hasOutput <- true
            let fullPath = Path.Combine(Path.GetDirectoryName config.ProjectFile, path)
            bundle.Js.WriteFile(fullPath)
        | _ -> ()

        match config.ProjectType, config.MinJSOutputPath with
        | Some BundleOnly, Some path ->
            hasOutput <- true
            let fullPath = Path.Combine(Path.GetDirectoryName config.ProjectFile, path)
            bundle.MinJs.WriteFile(fullPath)
        | _ -> ()

        if not hasOutput then  
            match config.ProjectType with
            | Some Bundle ->
                failwith "WebSharperBundleOutputDir property (or \"outputDir\" in wsconfig.json) is required for Bundle projects"
            | Some BundleOnly ->
                failwith "WebSharperBundleOutputDir property (or \"outputDir\", \"jsOutput\" or \"minJsOutput\" in wsconfig.json) is required for BundleOnly projects"
            | p ->
                failwithf "Bunlding called for unexpected project type: %s. Use with \"Bundle\" or \"BundleOnly\"." (p |> Option.map string |> Option.defaultValue "None")

    let Bundle (config: WsConfig) (logger: LoggerBase) (refMetas: M.Info list) (currentMeta: M.Info) (comp: Compilation) (currentJS: Lazy<option<string * string>>) sources (refAssemblies: Assembly list) (currentExtraBundles: list<string * Content>) =
        let entryPointStyle =
            if List.isEmpty comp.JavaScriptExports
            then JavaScriptPackager.EntryPointStyle.ForceOnLoad
            else JavaScriptPackager.EntryPointStyle.OnLoadIfExists
        let b =
            logger
            |> CreateBundle {
                Config = config
                RefMetas = refMetas
                CurrentMeta = currentMeta
                GetAllDeps = FrontEnd.GetDepsFromJSExports comp.JavaScriptExports M.EntryPointNode 
                EntryPoint = comp.EntryPoint
                EntryPointStyle = entryPointStyle
                CurrentJs = currentJS
                Sources = sources
                RefAssemblies = refAssemblies
                IsExtraBundle = false
                AddError = fun pos msg -> comp.AddError(pos, SourceError msg)
            }
        b |> WriteBundle config
        match BundleOutputDir config (GetWebRoot config) with
        | Some outDir ->
            let extraBundleFiles =
                comp.AllExtraBundles
                |> Seq.map (fun b ->
                    if b.AssemblyName = comp.AssemblyName then
                        b.AssemblyName, currentExtraBundles |> List.map (fun (name, content) -> name, content.Text)
                    else
                        let asm = refAssemblies |> List.find (fun asm -> asm.Name = b.AssemblyName)
                        let scripts = asm.GetScripts WebSharper.Core.JavaScript.Output.JavaScript
                        let findScript name = 
                            name, (scripts |> Seq.find (fun s -> s.FileName = name || s.FileName = name + "x")).Content
                        asm.Name, [findScript ("../workers/" + b.FileName)]
                )
                |> Seq.append (
                    refAssemblies
                    |> Seq.collect (fun asm ->
                        let scripts = asm.GetResScripts() |> List.ofSeq
                        if List.isEmpty scripts then
                            []
                        else
                            [
                                asm.Name,
                                scripts |> List.map (fun js -> js.FileName, js.Content )
                            ]
                    )
                )
            for asmName, bundle in extraBundleFiles do
                let baseDir = Path.Combine(outDir, asmName)
                for filename, content in bundle do
                    let path = Path.Combine(baseDir, filename)
                    System.IO.Directory.CreateDirectory (Path.GetDirectoryName path) |> ignore
                    System.IO.File.WriteAllText(path, content)
        | None -> ()

