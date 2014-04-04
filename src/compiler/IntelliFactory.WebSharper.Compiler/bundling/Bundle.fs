// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Compiler

module JS = IntelliFactory.JavaScript.Syntax
module M = IntelliFactory.WebSharper.Core.Metadata
module Re = IntelliFactory.WebSharper.Core.Reflection
module Res = IntelliFactory.WebSharper.Core.Resources
module W = IntelliFactory.JavaScript.Writer

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
        |> W.ExpressionToString IntelliFactory.JavaScript.Preferences.Compact

    let GetDependencyNodeForAssembly (a: Assembly) : M.Node =
        let name = Re.AssemblyName.Parse(a.FullName)
        M.Node.AssemblyNode(name, M.AssemblyMode.CompiledAssembly)

[<Sealed>]
type Bundle(set: list<Assembly>) =
    let logger = Logger.Create ignore 1000
    let resolver =
        let r = AssemblyResolver.Create()
        set
        |> Seq.choose (fun a -> Option.map Path.GetDirectoryName a.LoadPath)
        |> Seq.distinct
        |> r.SearchDirectories

    let loader = Loader.Create resolver ignore

    let context = lazy Context.Get(set)

    let deps =
        lazy
        resolver.Wrap <| fun () ->
        let context = context.Value
        let mInfo = context.CreateMetadataInfo()
        mInfo.GetDependencies [for a in set -> GetDependencyNodeForAssembly a]

    let htmlHeadersContext : Res.Context =
        {
            DebuggingEnabled = false
            DefaultToHttp = false
            GetSetting = fun _ -> None
            GetAssemblyRendering = fun _ -> Res.Skip
            GetWebResourceRendering = fun _ _-> Res.Skip
        }

    let renderHtmlHeaders (hw: HtmlTextWriter) (res: Res.IResource) =
        res.Render htmlHeadersContext hw

    let render (mode: BundleMode) (writer: TextWriter) =
        resolver.Wrap <| fun () ->
        use htmlHeadersWriter =
            match mode with
            | BundleMode.HtmlHeaders -> new HtmlTextWriter(writer)
            | _ -> new HtmlTextWriter(TextWriter.Null)
        let debug =
            match mode with
            | BundleMode.MinifiedJavaScript -> false
            | _ -> true
        let context = context.Value
        let renderAssembly (a: Assembly) =
            match mode with
            | BundleMode.JavaScript -> a.ReadableJavaScript
            | BundleMode.MinifiedJavaScript -> a.CompressedJavaScript
            | BundleMode.TypeScript -> a.TypeScriptDeclarations
            | _ -> None
            |> Option.iter (fun t -> writer.WriteLine(t))
        let renderWebResource (name: string) (cType: string) (c: string) =
            match cType.ToLower(), mode with
            | "text/javascript", BundleMode.JavaScript
            | "text/javascript", BundleMode.MinifiedJavaScript ->
                writer.Write(c)
                writer.WriteLine(";")
            | "text/css", BundleMode.CSS ->
                writer.WriteLine(c)
            | _ -> ()
        let ctx : Res.Context =
            {
                DebuggingEnabled = debug
                DefaultToHttp = false // TODO make configurable
                GetAssemblyRendering = fun name ->
                    context.LookupAssembly(name)
                    |> Option.iter renderAssembly
                    Res.Skip
                GetSetting = fun name -> None
                GetWebResourceRendering = fun ty name ->
                    let (c, cT) = Utility.ReadWebResource ty name
                    renderWebResource name cT c
                    Res.Skip
            }
        use htmlWriter = new HtmlTextWriter(TextWriter.Null)
        for d in deps.Value do
            match mode with
            | BundleMode.HtmlHeaders -> renderHtmlHeaders htmlHeadersWriter d
            | _ ->
                d.Render ctx htmlWriter
        match mode with
        | BundleMode.JavaScript | BundleMode.MinifiedJavaScript ->
            Utility.WriteStartCode false writer
        | _ -> ()

    static let domFix =
        Utility.ReadResourceFromAssembly typeof<Bundle> "DomFix.d.ts"

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
    let typeScript = content (Some domFix.Value) BundleMode.TypeScript

    member b.CSS = css
    member b.HtmlHeaders = htmlHeaders
    member b.JavaScript = javaScript
    member b.JavaScriptHeaders = javaScriptHeaders
    member b.MinifiedJavaScript = minifedJavaScript
    member b.TypeScript = typeScript

    member b.WithAssembly(assemblyFile) =
        let assem = loader.LoadFile(assemblyFile)
        Bundle(assem :: set)

    member b.WithDefaultReferences() =
        let wsHome = Path.GetDirectoryName(typeof<Bundle>.Assembly.Location)
        [|
            "IntelliFactory.WebSharper.Collections"
            "IntelliFactory.WebSharper.Control"
        |]
        |> Seq.map (fun n -> Path.Combine(wsHome, n + ".dll"))
        |> Seq.fold (fun (b: Bundle) x -> b.WithAssembly(x)) b

    member b.WithTransitiveReferences() =
        let comparer =
            HashIdentity.FromFunctions<Assembly>
                (fun a -> hash a.Raw.FullName)
                (fun a b -> a.Raw.FullName = b.Raw.FullName)
        let pred (a: Assembly) =
            a.Raw.MainModule.AssemblyReferences
            |> Seq.choose (fun r ->
                let n = AssemblyName(r.FullName)
                match resolver.ResolvePath(n) with
                | None -> None
                | Some path ->
                    loader.LoadFile(path)
                    |> Some)
        let completeSet =
            Algorithms.TopSort.Do(set, pred, comparer)
            |> Seq.toList
        Bundle(completeSet)

    static member Empty = Bundle([])
    static member Create() = Bundle.Empty.WithDefaultReferences()

