// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

open System.Configuration
open WebSharper.Core.AST

module CT = WebSharper.Core.ContentTypes
module JS = WebSharper.Core.JavaScript.Syntax
module M = WebSharper.Core.Metadata
module Res = WebSharper.Core.Resources
module W = WebSharper.Core.JavaScript.Writer
module FE = WebSharper.Compiler.FrontEnd
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
        JS.Application (JS.Binary (JS.Var "document", JS.BinaryOperator.``.``, str "write"), [str w])
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
            EntryPointStyle: Packager.EntryPointStyle
            IsExtraBundle: bool
            AddError : option<SourcePos> -> string -> unit
        }

    let private CreateBundle (o: BundleOptions) =
        let failf format =
            Printf.kprintf (o.AddError None) format

        let sourceMap = o.Config.SourceMap
        let dce = o.Config.DeadCodeElimination
        let appConfig = None
        
        let graph =
            o.RefMetas |> Seq.map (fun m -> m.Dependencies)
            |> Seq.append (Seq.singleton o.CurrentMeta.Dependencies)
            |> WebSharper.Core.DependencyGraph.Graph.FromData

        let mapFileSources = 
            if sourceMap then
                o.RefAssemblies |> Seq.collect (fun a ->
                    match a.MapFileForReadable with
                    | Some mapFile -> WebSharper.Compiler.Packager.readMapFileSources mapFile
                    | _-> []
                )  
                |> Seq.append o.Sources
                |> Array.ofSeq 
            else [||]

        let mutable map = None
        let mutable minmap = None

        let getSetting =
            match appConfig with
            | None -> fun _ -> None
            | Some p ->
                let conf =
                    ConfigurationManager.OpenMappedExeConfiguration(
                        ExeConfigurationFileMap(ExeConfigFilename = p),
                        ConfigurationUserLevel.None)
                fun name ->
                    match conf.AppSettings.Settings.[name] with
                    | null -> None
                    | x -> Some x.Value

        // if DCE and sourcemapping are both off, opt for quicker way of just concatenating assembly js outputs
        let concatScripts = not dce && not sourceMap
        if concatScripts then
            printfn "Using pre-compiled JavaScript for bundling"

        let htmlHeadersContext : Res.Context =
            {
                DebuggingEnabled = false
                DefaultToHttp = false
                ScriptBaseUrl = o.Config.ScriptBaseUrl
                GetSetting = getSetting
                GetAssemblyRendering = fun _ -> Res.Skip
                GetWebResourceRendering = fun _ _-> Res.Skip
                WebRoot = "/"
                RenderingCache = null
                ResourceDependencyCache = null
            }

        let nodes = o.GetAllDeps graph

        let pkg =   
            if concatScripts then
                WebSharper.Core.AST.Undefined
            else
                let meta = 
                    o.RefMetas |> Seq.map refreshAllIds
                    |> Seq.append (Seq.singleton o.CurrentMeta)
                    |> M.Info.UnionWithoutDependencies 
                try
                    let current = 
                        if dce then trimMetadata meta nodes 
                        else meta
                    Packager.packageAssembly current current o.EntryPoint o.EntryPointStyle
                with e -> 
                    CommandTools.argError ("Error during bundling: " + e.Message)
        let resources = graph.GetResourcesOf nodes

        let noHtmlWriter = new HtmlTextWriter(TextWriter.Null)

        let assemblyLookup =
            lazy 
            o.RefAssemblies |> Seq.map (fun a -> a.Name, a) |> Map

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
                let debug =
                    match mode with
                    | BundleMode.MinifiedJavaScript -> false
                    | _ -> true
                let renderWebResource cType (c: string) =
                    match cType, mode with
                    | CT.JavaScript, BundleMode.JavaScript
                    | CT.JavaScript, BundleMode.MinifiedJavaScript ->
                        writer.Write(c)
                        writer.WriteLine(";")
                    | CT.Css, BundleMode.CSS ->
                        writer.WriteLine(c)
                    | _ -> ()
                let ctx : Res.Context =
                    {
                        DebuggingEnabled = debug
                        DefaultToHttp = false // TODO make configurable
                        ScriptBaseUrl = o.Config.ScriptBaseUrl
                        GetAssemblyRendering = 
                            match concatScripts, mode with
                            | true, BundleMode.JavaScript -> 
                                fun name ->
                                    assemblyLookup.Value |> Map.tryFind name
                                    |> Option.bind (fun a -> a.ReadableJavaScript)
                                    |> Option.iter (fun t -> writer.WriteLine(t))
                                    Res.Skip
                            | true, BundleMode.MinifiedJavaScript -> 
                                fun name ->
                                    assemblyLookup.Value |> Map.tryFind name
                                    |> Option.bind (fun a -> a.CompressedJavaScript)
                                    |> Option.iter (fun t -> writer.WriteLine(t))
                                    Res.Skip
                            | _ ->
                                fun _ -> Res.Skip
                        GetSetting = getSetting
                        GetWebResourceRendering = fun ty name ->
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
                                writer.WriteLine("importScripts([{0}]);",
                                    urls |> Seq.map (fun url ->
                                        let s = W.ExpressionToString WebSharper.Core.JavaScript.Preferences.Compact !~(JS.String url)
                                        s.Trim()
                                    )
                                    |> String.concat ","
                                )
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

                    let getCodeWriter() =
                        if sourceMap then
                            WebSharper.Core.JavaScript.Writer.CodeWriter(
                                sources = mapFileSources,
                                offset = (writer.ToString() |> Seq.sumBy (function '\n' -> 1 | _ -> 0))
                            )
                        else WebSharper.Core.JavaScript.Writer.CodeWriter()    

                    let js, m = pkg |> WebSharper.Compiler.Packager.exprToString pref getCodeWriter
                    if sourceMap then
                        if mode = BundleMode.JavaScript then
                            map <- m
                        else
                            minmap <- m

                    writer.WriteLine js

                    Res.HtmlTextWriter.WriteStartCode(writer, o.Config.ScriptBaseUrl, false, o.IsExtraBundle)
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

        {
            Css = Some css
            HeadHtml = Some htmlHeaders
            HeadJs = Some javaScriptHeaders
            Js = javaScript
            JsMap = mapping
            MinJs = minifiedJavaScript
            MinJsMap = minifiedMapping
        }

    let private getDeps (jsExport: JsExport list) extraNodes (graph: Graph) =
        let jsExportNames =
            jsExport |> List.choose (function 
                | ExportByName n -> Some n
                | _ -> None
            )
        let nodes =
            seq {
                yield M.EntryPointNode 
                match jsExportNames with
                | [] -> ()
                | _ ->
                    let e = System.Collections.Generic.HashSet jsExportNames
                    yield! 
                        graph.Nodes |> Seq.filter (function
                            | M.AssemblyNode (a, _) -> e.Contains a
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
                yield! extraNodes
            }
            |> graph.GetDependencies
        nodes

    let private (==) s1 s2 =
        System.String.Equals(s1, s2, System.StringComparison.OrdinalIgnoreCase)

    let AddExtraBundles config refMetas (currentMeta: M.Info) (refAssemblies: list<Assembly>) (comp: Compilation) (assem: Choice<string, Assembly>) =
        let config =
            { config with
                SourceMap = false // TODO make SourceMap work with this
                DeadCodeElimination = true
                ScriptBaseUrl = None
            }
        let pub = Mono.Cecil.ManifestResourceAttributes.Public
        let addWebResourceAttribute =
            match assem with
            | Choice1Of2 _ -> ignore
            | Choice2Of2 assem ->
            let strTy =
                let std = refAssemblies |> List.find (fun ar -> ar.Name == "netstandard" || ar.Name == "mscorlib")
                Mono.Cecil.TypeReference("System", "String", std.Raw.MainModule, std.Raw.MainModule) |> assem.Raw.MainModule.ImportReference
            let webResourceTy =
                let wsCoreJs = refAssemblies |> List.find (fun ar -> ar.Name == "WebSharper.Core.JavaScript")
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
                let bundle = CreateBundle {
                    Config = config
                    RefMetas = refMetas
                    CurrentMeta = currentMeta
                    GetAllDeps = getDeps jsExports [bundle.Node]
                    EntryPoint = Some bundle.EntryPoint
                    EntryPointStyle = Packager.EntryPointStyle.ForceImmediate
                    CurrentJs = lazy None
                    Sources = []
                    RefAssemblies = refAssemblies
                    IsExtraBundle = true
                    AddError = fun pos msg -> comp.AddError(pos, SourceError msg)
                }
                let bundleFiles =
                    [
                        yield bname + ".js", bundle.Js
                        yield bname + ".min.js", bundle.MinJs
                        if bundle.JsMap.IsSome then
                            yield bname + ".map", bundle.JsMap.Value
                        if bundle.MinJsMap.IsSome then
                            yield bname + ".min.map", bundle.MinJsMap.Value
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
                    let mapExt = ext.Replace(".js", ".map")
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

    let Bundle (config: WsConfig) (refMetas: M.Info list) (currentMeta: M.Info) (comp: Compilation) (currentJS: Lazy<option<string * string>>) sources (refAssemblies: Assembly list) (currentExtraBundles: list<string * Content>) =
        let entryPointStyle =
            if List.isEmpty comp.JavaScriptExports
            then Packager.EntryPointStyle.ForceOnLoad
            else Packager.EntryPointStyle.OnLoadIfExists
        CreateBundle {
            Config = config
            RefMetas = refMetas
            CurrentMeta = currentMeta
            GetAllDeps = getDeps comp.JavaScriptExports []
            EntryPoint = comp.EntryPoint
            EntryPointStyle = entryPointStyle
            CurrentJs = currentJS
            Sources = sources
            RefAssemblies = refAssemblies
            IsExtraBundle = false
            AddError = fun pos msg -> comp.AddError(pos, SourceError msg)
        }
        |> WriteBundle config
        match BundleOutputDir config (GetWebRoot config) with
        | Some outDir ->
            let extraBundleFiles =
                comp.AllExtraBundles
                |> Seq.map (fun b ->
                    if b.AssemblyName = comp.AssemblyName then
                        b.AssemblyName, currentExtraBundles |> List.map (fun (name, content) -> name, content.Text)
                    else
                        let asm = refAssemblies |> List.find (fun asm -> asm.Name = b.AssemblyName)
                        let scripts = asm.GetScripts()
                        let findScript name = 
                            name, (scripts |> Seq.find (fun s -> s.FileName = name)).Content
                        asm.Name, [findScript b.FileName; findScript b.MinifiedFileName]
                )
            for asmName, bundle in extraBundleFiles do
                let baseDir = Path.Combine(outDir, asmName)
                System.IO.Directory.CreateDirectory(baseDir) |> ignore
                for filename, content in bundle do
                    let path = Path.Combine(baseDir, filename)
                    System.IO.File.WriteAllText(path, content)
        | None -> ()

