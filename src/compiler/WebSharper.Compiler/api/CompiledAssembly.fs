// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module CT = WebSharper.Core.ContentTypes
module M = WebSharper.Core.Metadata
module P = WebSharper.Core.JavaScript.Packager
module PC = WebSharper.PathConventions
module R = WebSharper.Compiler.ReflectionLayer
module Re = WebSharper.Core.Reflection
module Res = WebSharper.Core.Resources
module W = WebSharper.Core.JavaScript.Writer
type Pref = WebSharper.Core.JavaScript.Preferences

[<Sealed>]
type CompiledAssembly
    (
        context: Context,
        source: R.AssemblyDefinition,
        meta: Metadata.T,
        aInfo: M.AssemblyInfo,
        mInfo: M.Info,
        pkg: P.Module,
        typeScript: option<string>,
        sourceMap: bool
    ) =

    let nameOfSelf = Re.AssemblyName.Convert(source.Name)
    let shortName = nameOfSelf.Name

    let getJS (pref: Pref) =
        let w = if sourceMap then W.CodeWriter(shortName) else W.CodeWriter()
        W.WriteProgram pref w (P.Package pkg pref)
        w.GetCodeFile(), w.GetMapFile()

    let compressedJS = lazy getJS Pref.Compact
    let readableJS = lazy getJS Pref.Readable

    let deps =
        lazy
        let self = M.Node.AssemblyNode(nameOfSelf, M.AssemblyMode.CompiledAssembly)
        mInfo.GetDependencies([self])

    member this.AssemblyInfo = aInfo
    member this.CompressedJavaScript = fst compressedJS.Value
    member this.MapFileForCompressed = if sourceMap then snd compressedJS.Value else None
    member this.Info = mInfo
    member this.Metadata = meta
    member this.Package = pkg
    member this.ReadableJavaScript = fst readableJS.Value
    member this.MapFileForReadable = if sourceMap then snd readableJS.Value else None
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
                ContentType = CT.Text.JavaScript
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
                        |> getJS |> fst
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

    member this.RenderDependencies(ctx, writer) =
        for d in this.Dependencies do
            d.Render ctx (fun _ -> writer)
        Utility.WriteStartCode true writer

    static member Create
            (
                context: Context,
                source: R.AssemblyDefinition,
                meta: Metadata.T,
                aInfo: M.AssemblyInfo,
                mInfo: M.Info,
                pkg: P.Module,
                typeScript: option<string>,
                sourceMap: bool
            ) =
        CompiledAssembly(context, source, meta, aInfo, mInfo, pkg, typeScript, sourceMap)

    member this.WriteToCecilAssembly(a: Mono.Cecil.AssemblyDefinition) =
        let pub = Mono.Cecil.ManifestResourceAttributes.Public
        let dep =
            use s = new MemoryStream(8 * 1024)
            Metadata.Serialize s meta
            s.ToArray()
        let prog = P.Package pkg
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
            let inline getBytes (x: string) = System.Text.Encoding.UTF8.GetBytes x
            Mono.Cecil.EmbeddedResource(EMBEDDED_MINJS, pub, getBytes this.CompressedJavaScript)
            |> a.MainModule.Resources.Add
            this.MapFileForCompressed |> Option.iter (fun m ->
                Mono.Cecil.EmbeddedResource(EMBEDDED_MINMAP, pub, getBytes m)
                |> a.MainModule.Resources.Add )
            Mono.Cecil.EmbeddedResource(EMBEDDED_JS, pub, getBytes this.ReadableJavaScript)
            |> a.MainModule.Resources.Add
            this.MapFileForReadable |> Option.iter (fun m ->
                Mono.Cecil.EmbeddedResource(EMBEDDED_MAP, pub, getBytes m)
                |> a.MainModule.Resources.Add )
        match typeScript with
        | Some tS ->
            Mono.Cecil.EmbeddedResource
                (
                    EMBEDDED_DTS, pub,
                    UTF8Encoding(false, true).GetBytes(tS)
                )
            |> a.MainModule.Resources.Add
        | _ -> ()
