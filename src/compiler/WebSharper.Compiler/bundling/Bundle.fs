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

    let GetDependencyNodeForAssembly (a: Assembly) : M.Node =
        M.Node.AssemblyNode (a.FullName, true)

[<Sealed>]
type Bundle(set: list<Assembly>, aR: AssemblyResolver, ?appConfig: string) =

    let meta =
        set
        |> List.choose WebSharper.Compiler.FrontEnd.ReadFromAssembly
        |> WebSharper.Core.DependencyGraph.Graph.UnionOfMetadata 

    let graph = WebSharper.Core.DependencyGraph.Graph.FromData meta.Dependencies

    let htmlHeadersContext getSetting : Res.Context =
        {
            DebuggingEnabled = false
            DefaultToHttp = false
            GetSetting = getSetting
            GetAssemblyRendering = fun _ -> Res.Skip
            GetWebResourceRendering = fun _ _-> Res.Skip
        }

    let render (mode: BundleMode) (writer: TextWriter) =
        aR.Wrap <| fun () ->
        use htmlHeadersWriter =
            match mode with
            | BundleMode.HtmlHeaders -> new HtmlTextWriter(writer)
            | _ -> new HtmlTextWriter(TextWriter.Null)
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
        let ctx : Res.Context =
            {
                DebuggingEnabled = debug
                DefaultToHttp = false // TODO make configurable
                GetAssemblyRendering = fun _ -> Res.Skip
                GetSetting = getSetting
                GetWebResourceRendering = fun ty name ->
                    let (c, cT) = Utility.ReadWebResource ty name
                    renderWebResource cT c
                    Res.Skip
            }
        use htmlWriter = new HtmlTextWriter(TextWriter.Null)
        let htmlHeadersContext = htmlHeadersContext getSetting
        
        let nodes = graph.GetDependencies [ M.EntryPointNode ]        
        let current = trimMetadata meta nodes 

        for d in graph.GetResourcesOf nodes do
            match mode with
            | BundleMode.HtmlHeaders -> d.Render htmlHeadersContext (fun _ -> htmlHeadersWriter)
            | _ -> d.Render ctx (fun _ -> htmlWriter)

        match mode with
        | BundleMode.JavaScript | BundleMode.MinifiedJavaScript ->
            
            let pkg =   
                Packager.packageAssembly current current true

            let pref =
                if mode = BundleMode.JavaScript then 
                    WebSharper.Core.JavaScript.Readable
                else 
                    WebSharper.Core.JavaScript.Compact

            let js, _ = pkg |> WebSharper.Compiler.Packager.exprToString "Bundle" pref false
//            let minJs, minMap = pkg |> WebSharper.Compiler.Packager.exprToString a.Name.Name WebSharper.Core.JavaScript.Compact

            writer.WriteLine js

            Utility.WriteStartCode false writer
        | _ -> ()

//    static let domFix =
//        Utility.ReadResourceFromAssembly typeof<Bundle> "DomFix.d.ts"

    let content (prefix: option<string>) mode =
        let t =
            lazy
            use w = new StringWriter()
            match prefix with
            | None -> ()
            | Some prefix -> w.WriteLine(prefix)
            render mode w
            w.ToString()
        Content.Create(t)

    let css = content None BundleMode.CSS
    let htmlHeaders = content None BundleMode.HtmlHeaders
    let javaScriptHeaders = htmlHeaders.Map(DocWrite)
    let javaScript = content None BundleMode.JavaScript
    let minifedJavaScript = content None BundleMode.MinifiedJavaScript
    let typeScript = content None BundleMode.TypeScript // (Some domFix.Value) BundleMode.TypeScript

    member b.CSS = css
    member b.HtmlHeaders = htmlHeaders
    member b.JavaScript = javaScript
    member b.JavaScriptHeaders = javaScriptHeaders
    member b.MinifiedJavaScript = minifedJavaScript
    member b.TypeScript = typeScript
