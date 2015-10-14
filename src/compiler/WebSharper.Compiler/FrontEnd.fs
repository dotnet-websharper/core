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
        eP.DeriveEncoding typeof<M.Metadata>
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
                Some (MetadataEncoding.Decode s :?> M.Metadata)
#if DEBUG
#else
            with _ ->
                failwithf "Failed to deserialize metadata for: %s" a.FullName
#endif
            | _ -> None)

let modifyAssembly (merged: M.Metadata) (current: M.Metadata) (a: Mono.Cecil.AssemblyDefinition) =
//    let current = M.toSingleMetadata comp 
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let meta =
        use s = new MemoryStream(8 * 1024)
        MetadataEncoding.Encode s current
        s.ToArray()
    let pkg = 
        WebSharper.Compiler.Packager.packageAssembly merged current
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

let modifyWSAssembly (merged: M.Metadata) (current: M.Metadata) (assembly : Assembly) =
    modifyAssembly merged current assembly.Raw
