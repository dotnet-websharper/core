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

module WebSharper.Compiler.FrontEnd

open WebSharper.Core
module M = WebSharper.Core.Metadata
module B = WebSharper.Core.Binary

type Assembly = WebSharper.Compiler.Assembly
type Content = WebSharper.Compiler.Content
type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
type Loader = WebSharper.Compiler.Loader
type Symbols = WebSharper.Compiler.Symbols

type ReadOptions =
    | FullMetadata
    | DiscardExpressions
    | DiscardInlineExpressions
    | DiscardNotInlineExpressions

let ReadFromAssembly options (a: Assembly) =
    a.Raw.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = EMBEDDED_METADATA ->
            try
                use s = r.GetResourceStream()
                let m = M.IO.Decode s
                match options with
                | FullMetadata -> refreshAllIds m
                | DiscardExpressions -> m.DiscardExpressions() 
                | DiscardInlineExpressions -> refreshAllIds (m.DiscardInlineExpressions())
                | DiscardNotInlineExpressions -> m.DiscardNotInlineExpressions()
                |> Some
            with
            | e ->
                failwithf "Failed to deserialize metadata for %s. Error: %s" a.FullName e.Message
        | _ -> None
    )

let ReadFromFile options (path: string) =
    let aR = AssemblyResolver.Create().SearchPaths([path])
    let loader = Loader.Create aR ignore
    loader.LoadFile(path) |> ReadFromAssembly options

let GetJSLookup (r: Assembly list, readable) =
    r |> List.choose (fun a ->
        if readable then a.ReadableJavaScript else a.CompressedJavaScript
        |> Option.map (fun js -> a.FullName, js)
    )
    |> dict

let ModifyWIGAssembly (current: M.Info) (a: Mono.Cecil.AssemblyDefinition) =
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let meta =
        use s = new MemoryStream(8 * 1024)
        M.IO.Encode s current
        s.ToArray()
    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, meta)
    |> a.MainModule.Resources.Add

let ModifyTSAssembly (current: M.Info) (a: Assembly) =
    ModifyWIGAssembly current a.Raw

let ModifyCecilAssembly (merged: M.Info) (current: M.Info) sourceMap (a: Mono.Cecil.AssemblyDefinition) =
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let meta =
        use s = new MemoryStream(8 * 1024)
        M.IO.Encode s current
        s.ToArray()
    let pkg = 
        WebSharper.Compiler.Packager.packageAssembly merged current false

    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, meta)
    |> a.MainModule.Resources.Add
    
    if pkg <> AST.Undefined then
        let js, map = pkg |> WebSharper.Compiler.Packager.exprToString a.Name.Name WebSharper.Core.JavaScript.Readable sourceMap
        let minJs, minMap = pkg |> WebSharper.Compiler.Packager.exprToString a.Name.Name WebSharper.Core.JavaScript.Compact sourceMap
        let inline getBytes (x: string) = System.Text.Encoding.UTF8.GetBytes x
        Mono.Cecil.EmbeddedResource(EMBEDDED_MINJS, pub, getBytes minJs)
        |> a.MainModule.Resources.Add
        minMap |> Option.iter (fun m ->
            Mono.Cecil.EmbeddedResource(EMBEDDED_MINMAP, pub, getBytes m)
            |> a.MainModule.Resources.Add )
        Mono.Cecil.EmbeddedResource(EMBEDDED_JS, pub, getBytes js)
        |> a.MainModule.Resources.Add
        map |> Option.iter (fun m ->
            Mono.Cecil.EmbeddedResource(EMBEDDED_MAP, pub, getBytes m)
            |> a.MainModule.Resources.Add )

        Some js
    else None

let ModifyAssembly (merged: M.Info) (current: M.Info) sourceMap (assembly : Assembly) =
    ModifyCecilAssembly merged current sourceMap assembly.Raw

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
    }

let RenderDependencies(ctx: ResourceContext, writer: HtmlTextWriter, nameOfSelf, selfJS, deps: Resources.IResource list, lookupAssemblyCode) =
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
        }
    for d in deps do
        d.Render ctx (fun _ -> writer)
    Utility.WriteStartCode true writer