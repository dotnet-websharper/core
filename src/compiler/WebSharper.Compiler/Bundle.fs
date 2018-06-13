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

    let Bundle (config: WsConfig) (refMetas: M.Info list) (currentMeta: M.Info) (jsExport: JsExport list) (currentJS: Lazy<option<string * string>>) sources (refAssemblies: Assembly list) =

        let outputDir = BundleOutputDir config (GetWebRoot config)
        let fileName = Path.GetFileNameWithoutExtension config.AssemblyFile
        let sourceMap = config.SourceMap
        let dce = config.DeadCodeElimination
        let appConfig = None
        
        let graph =
            refMetas |> Seq.map (fun m -> m.Dependencies)
            |> Seq.append (Seq.singleton currentMeta.Dependencies)
            |> WebSharper.Core.DependencyGraph.Graph.FromData

        let mapFileSources = 
            if sourceMap then
                refAssemblies |> Seq.collect (fun a ->
                    match a.MapFileForReadable with
                    | Some mapFile -> WebSharper.Compiler.Packager.readMapFileSources mapFile
                    | _-> []
                )  
                |> Seq.append sources
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
                GetSetting = getSetting
                GetAssemblyRendering = fun _ -> Res.Skip
                GetWebResourceRendering = fun _ _-> Res.Skip
                WebRoot = "/"
                RenderingCache = null
                ResourceDependencyCache = null
            }

        let nodes = 
            let jsExportNames =
                jsExport |> List.choose (function 
                    | ExportByName n -> Some n
                    | _ -> None
                )
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
                                -> e.Contains td.Value.FullName
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
        let pkg =   
            if concatScripts then
                WebSharper.Core.AST.Undefined
            else
                let meta = 
                    refMetas |> Seq.map refreshAllIds
                    |> Seq.append (Seq.singleton currentMeta)
                    |> M.Info.UnionWithoutDependencies 
                let current = 
                    if dce then trimMetadata meta nodes 
                    else meta
                let forceEntryPoint =
                    List.isEmpty jsExport
                try
                    Packager.packageAssembly current current forceEntryPoint
                with e -> 
                    CommandTools.argError ("Error during bundling: " + e.Message)
        let resources = graph.GetResourcesOf nodes

        let noHtmlWriter = new HtmlTextWriter(TextWriter.Null)

        let assemblyLookup =
            lazy 
            refAssemblies |> Seq.map (fun a -> a.Name, a) |> Map

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

            if concatScripts then 
                match mode with
                | BundleMode.JavaScript -> 
                    currentJS.Value |> Option.iter (fun (t, _) -> writer.WriteLine(t))
                    Utility.WriteStartCode false writer
                | BundleMode.MinifiedJavaScript ->
                    currentJS.Value |> Option.iter (fun (_, t) -> writer.WriteLine(t))
                    Utility.WriteStartCode false writer
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

                    Utility.WriteStartCode false writer
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

        System.IO.Directory.CreateDirectory outputDir |> ignore
        let write (c: Content) (ext: string) =
            c.WriteFile(Path.Combine(outputDir, fileName + ext))
        let writeMapped (c: Content) m (ext: string) =
            write c ext
            m |> Option.iter (fun mc ->
                let mapExt = ext.Replace(".js", ".map")
                write mc mapExt
                File.AppendAllLines(
                    Path.Combine(outputDir, fileName + ext),
                    [| "//# sourceMappingURL=" + fileName + mapExt |]
                )
            )
        
        write css ".css"
        write htmlHeaders ".head.html"
        write javaScriptHeaders ".head.js"
        writeMapped javaScript mapping ".js"
        writeMapped minifiedJavaScript minifiedMapping ".min.js"
