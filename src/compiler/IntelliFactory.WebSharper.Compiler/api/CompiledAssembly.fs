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

module M = IntelliFactory.WebSharper.Core.Metadata
module P = IntelliFactory.JavaScript.Packager
module PC = IntelliFactory.WebSharper.PathConventions
module R = IntelliFactory.WebSharper.Compiler.ReflectionLayer
module Re = IntelliFactory.WebSharper.Core.Reflection
module Res = IntelliFactory.WebSharper.Core.Resources
module W = IntelliFactory.JavaScript.Writer
type Pref = IntelliFactory.JavaScript.Preferences

[<Sealed>]
type CompiledAssembly
    (
        context: Context,
        source: R.AssemblyDefinition,
        meta: Metadata.T,
        aInfo: M.AssemblyInfo,
        mInfo: M.Info,
        pkg: P.Module,
        typeScript: string
    ) =

    let getJS (pref: Pref) =
        use w = new StringWriter()
        W.WriteProgram pref w (P.Package pkg pref)
        w.ToString()

    let compressedJS = lazy getJS Pref.Compact
    let readableJS = lazy getJS Pref.Readable

    let nameOfSelf = Re.AssemblyName.Convert(source.Name)

    let deps =
        lazy
        let self = M.Node.AssemblyNode(nameOfSelf, M.AssemblyMode.CompiledAssembly)
        mInfo.GetDependencies([self])

    member this.AssemblyInfo = aInfo
    member this.CompressedJavaScript = compressedJS.Value
    member this.Info = mInfo
    member this.Metadata = meta
    member this.Package = pkg
    member this.ReadableJavaScript = readableJS.Value
    member this.TypeScriptDeclarations = typeScript

    member this.Dependencies = deps.Value

    member this.RenderDependencies(ctx: ResourceContext, writer: HtmlTextWriter) =
        let pU = PC.PathUtility.VirtualPaths("/")
        let cache = Dictionary()
        let getRendering (content: ResourceContent) =
            match cache.TryGetValue(content) with
            | true, y -> y
            | _ ->
                let y = ctx.RenderResource(content)
                cache.Add(content, y)
                y
        let makeJsUri (name: PC.AssemblyId) js =
            getRendering {
                Content = js
                ContentType = "text/javascript"
                Name =
                    if ctx.DebuggingEnabled then
                        pU.JavaScriptPath(name)
                    else
                        pU.MinifiedJavaScriptPath(name)
            }
        let ctx : Res.Context =
            {
                DebuggingEnabled = ctx.DebuggingEnabled
                DefaultToHttp = ctx.DefaultToHttp
                GetAssemblyRendering = fun name ->
                    if name = nameOfSelf then
                        (if ctx.DebuggingEnabled then Pref.Readable else Pref.Compact)
                        |> getJS
                        |> makeJsUri (PC.AssemblyId.Create name.FullName)
                    else
                        match context.LookupAssemblyCode(ctx.DebuggingEnabled, name) with
                        | Some x -> makeJsUri (PC.AssemblyId.Create name.FullName) x
                        | None -> Res.Skip
                GetSetting = ctx.GetSetting
                GetWebResourceRendering = fun ty name ->
                    let (c, cT) = Utility.ReadWebResource ty name
                    getRendering {
                        Content = c
                        ContentType = cT
                        Name = name
                    }
            }
        this.RenderDependencies(ctx, writer)

    member this.RenderDependencies(ctx, writer: HtmlTextWriter) =
        for d in this.Dependencies do
            d.Render ctx writer
        Utility.WriteStartCode true writer

    static member Create
            (
                context: Context,
                source: R.AssemblyDefinition,
                meta: Metadata.T,
                aInfo: M.AssemblyInfo,
                mInfo: M.Info,
                pkg: P.Module,
                typeScript: string
            ) =
        CompiledAssembly(context, source, meta, aInfo, mInfo, pkg, typeScript)

    member this.WriteToCecilAssembly(a: Mono.Cecil.AssemblyDefinition) =
        let pub = Mono.Cecil.ManifestResourceAttributes.Public
        let dep =
            use s = new MemoryStream(8 * 1024)
            Metadata.Serialize s meta
            s.ToArray()
        let prog = P.Package pkg
        let js pref =
            use s = new MemoryStream(8 * 1024)
            let () =
                use w = new StreamWriter(s)
                W.WriteProgram pref w (prog pref)
            s.ToArray()
        let rmdata =
            use s = new MemoryStream(8 * 1024)
            aInfo.ToStream(s)
            s.ToArray()
        let rmname = M.AssemblyInfo.EmbeddedResourceName
        Mono.Cecil.EmbeddedResource(rmname, pub, rmdata)
        |> a.MainModule.Resources.Add
        Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, dep)
        |> a.MainModule.Resources.Add
        if not pkg.IsEmpty then
            Mono.Cecil.EmbeddedResource(EMBEDDED_MINJS, pub, js Pref.Compact)
            |> a.MainModule.Resources.Add
            Mono.Cecil.EmbeddedResource(EMBEDDED_JS, pub, js Pref.Readable)
            |> a.MainModule.Resources.Add
        Mono.Cecil.EmbeddedResource
            (
                EMBEDDED_DTS, pub,
                UTF8Encoding(false, true).GetBytes(typeScript)
            )
        |> a.MainModule.Resources.Add
