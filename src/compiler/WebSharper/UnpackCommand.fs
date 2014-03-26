namespace IntelliFactory.WebSharper.Commands

open System
open System.IO
open IntelliFactory.Core
open IntelliFactory.WebSharper
module FE = IntelliFactory.WebSharper.Compiler.FrontEnd

[<Sealed>]
type UnpackCommand() =

    static let initAR () =
        let baseDir =
            let pathToSelf = typeof<UnpackCommand>.Assembly.Location
            Path.GetDirectoryName(pathToSelf)
        AssemblyResolver.Create()
            .WithBaseDirectory(baseDir)
            .SearchDirectories([baseDir])

    static let writeTextFile (output, text) =
        Content.Text(text).WriteFile(output)

    static let writeBinaryFile (output, bytes) =
        Binary.FromBytes(bytes).WriteFile(output)

    member cmd.Run() =
        let pc = PathConventions.PathUtility.FileSystem(cmd.RootDirectory)
        let aR = cmd.AssemblyResolver.SearchPaths(cmd.Assemblies)
        let loader = FE.Loader.Create aR stderr.WriteLine
        let emit text path =
            match text with
            | Some text -> writeTextFile (path, text)
            | None -> ()
        let script = PathConventions.ResourceKind.Script
        let content = PathConventions.ResourceKind.Content
        for p in cmd.Assemblies do
            let a = loader.LoadFile p
            let aid = PathConventions.AssemblyId.Create(a.FullName)
            emit a.ReadableJavaScript (pc.JavaScriptPath aid)
            emit a.CompressedJavaScript (pc.MinifiedJavaScriptPath aid)
            emit a.TypeScriptDeclarations (pc.TypeScriptDefinitionsPath aid)
            let writeText k fn c =
                let p = pc.EmbeddedPath(PathConventions.EmbeddedResource.Create(k, aid, fn))
                writeTextFile (p, c)
            let writeBinary k fn c =
                let p = pc.EmbeddedPath(PathConventions.EmbeddedResource.Create(k, aid, fn))
                writeBinaryFile (p, c)
            for r in a.GetScripts() do
                writeText script r.FileName r.Content
            for r in a.GetContents() do
                writeBinary content r.FileName (r.GetContentData())

    member val Assemblies : list<string> = [] with get, set
    member val AssemblyResolver : AssemblyResolver = initAR () with get, set
    member val RootDirectory : string = "." with get, set
