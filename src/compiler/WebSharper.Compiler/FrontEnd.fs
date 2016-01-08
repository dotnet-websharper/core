module WebSharper.Compiler.FrontEnd

open WebSharper.Core
module M = WebSharper.Core.Metadata
module B = WebSharper.Core.Binary

type Assembly = WebSharper.Compiler.Assembly
//type Bundle = WebSharper.Compiler.Bundle
//type CompiledAssembly = WebSharper.Compiler.CompiledAssembly
type Content = WebSharper.Compiler.Content
type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
type Loader = WebSharper.Compiler.Loader
//type ResourceContent = WebSharper.Compiler.ResourceContent
//type ResourceContext = WebSharper.Compiler.ResourceContext
type Symbols = WebSharper.Compiler.Symbols

let MetadataEncoding =
    try
        let eP = B.EncodingProvider.Create()
        eP.DeriveEncoding typeof<M.Info>
    with B.NoEncodingException t ->
        failwith "Failed to create binary encoder for type %s" t.FullName

let readFromAssembly (a: Assembly) =
    a.Raw.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = EMBEDDED_METADATA ->
#if DEBUG
#else
            try
#endif
                use s = r.GetResourceStream()
                Some (MetadataEncoding.Decode s :?> M.Info |> M.refreshAllIds)
#if DEBUG
#else
            with _ ->
                failwithf "Failed to deserialize metadata for: %s" a.FullName
#endif
            | _ -> None)

let getJSLookup (r: Assembly list, readable) =
    r |> List.choose (fun a ->
        if readable then a.ReadableJavaScript else a.CompressedJavaScript
        |> Option.map (fun js -> a.FullName, js)
    )
    |> dict

let modifyWIGAssembly (current: M.Info) (a: Mono.Cecil.AssemblyDefinition) =
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let meta =
        use s = new MemoryStream(8 * 1024)
        MetadataEncoding.Encode s current
        s.ToArray()
    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, meta)
    |> a.MainModule.Resources.Add

let modifyTSAssembly (current: M.Info) (a: Assembly) =
    modifyWIGAssembly current a.Raw

let modifyCecilAssembly (merged: M.Info) (current: M.Info) (a: Mono.Cecil.AssemblyDefinition) =
//    let current = M.toSingleMetadata comp 
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let meta =
        use s = new MemoryStream(8 * 1024)
        MetadataEncoding.Encode s current
        s.ToArray()
    let pkg = 
        WebSharper.Compiler.Packager.packageAssembly merged current false
//        |> Option.fill AST.Undefined 

//    let prog = P.Package pkg
//    let rmdata =
//        use s = new MemoryStream(8 * 1024)
//        aInfo.ToStream(s)
//        s.ToArray()
//    let rmname = M.AssemblyInfo.EmbeddedResourceName
//    Mono.Cecil.EmbeddedResource(rmname, pub, rmdata)
//    |> a.MainModule.Resources.Add

    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, meta)
    |> a.MainModule.Resources.Add
    
    if pkg <> AST.Undefined then
//        let pkg = WebSharper.Compiler.ToJavaScriptSyntax.RemoveSourcePositions().TransformExpression pkg
        let js, map = pkg |> WebSharper.Compiler.Packager.exprToString a.Name.Name WebSharper.Core.JavaScript.Readable
        let minJs, minMap = pkg |> WebSharper.Compiler.Packager.exprToString a.Name.Name WebSharper.Core.JavaScript.Compact
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

//    match typeScript with
//    | Some tS ->
//        Mono.Cecil.EmbeddedResource
//            (
//                EMBEDDED_DTS, pub,
//                UTF8Encoding(false, true).GetBytes(tS)
//            )
//        |> a.MainModule.Resources.Add
//    | _ -> ()

let modifyAssembly (merged: M.Info) (current: M.Info) (assembly : Assembly) =
    modifyCecilAssembly merged current assembly.Raw

let renderDependencies(ctx: ResourceContext, writer: HtmlTextWriter, nameOfSelf, selfJS, deps: Resources.IResource list, lookupAssemblyCode) =
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