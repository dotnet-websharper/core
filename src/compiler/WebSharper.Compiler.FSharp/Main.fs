module FCSTest.Main

open Microsoft.FSharp.Compiler.SourceCodeServices

open System.IO

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckProject path = 
    checker.GetProjectOptionsFromProjectFile path
    |> checker.ParseAndCheckProject 
    |> Async.RunSynchronously     

type FSIFD = FSharpImplementationFileDeclaration

//[<EntryPoint>]
//let main argv = 
//    let checkFileResults = parseAndCheckProject (__SOURCE_DIRECTORY__ + @"\..\FSharpTestLibrary\FSharpTestLibrary.fsproj")
//
//    let content = checkFileResults.AssemblyContents.ImplementationFiles.[1]
//    let decl = content.Declarations
//
//    let compiledAssembly = System.Reflection.Assembly.LoadFile(__SOURCE_DIRECTORY__ + @"\..\FSharpTestLibrary\bin\Debug\FSharpTestLibrary.dll")
//
//    let reflected = CommonAST.Reflector.reflectAssembly compiledAssembly
//
//    let cat = ToMetadata.catalogueAssembly decl
//
//    let meta = ToMetadata.transformAssembly reflected decl
//
//    Translator.translateAssembly cat meta
//    meta |> CommonAST.Packager.packageAssembly |> CommonAST.Packager.exprToString
//    |> printfn "%s"
//
//
////    match decl with
////    | FSIFD.Entity (_, FSIFD.Entity (class1, _) :: ctor :: getX :: _) ->
////        match getX with
////        | FSIFD.MemberOrFunctionOrValue(getXName, args, body) ->
//////            printfn "%A" body
////            let tr = 
////                ToFSharpAST.transformExpression (ToFSharpAST.Environment.Empty) body
////            printfn "%+A" () 
////
////        | _ -> ()
////    | _ -> ()
//
//    System.Console.ReadKey() |> ignore
//
//    0
let stopwatch message task =
    printfn "%s" message
    let now = System.DateTime.Now
    let res = task()
    let elapsed = System.DateTime.Now - now
    printfn "Done in %A" elapsed
    res

let translateProject prevMeta (path: string) =
    let checkFileResults = 
        stopwatch "Parsing with FCS " <| fun () -> parseAndCheckProject path

    System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ a ->
        checkFileResults.ProjectContext.GetReferencedAssemblies()
        |> Seq.tryPick(fun ra -> 
            if ra.SimpleName = a.Name then
                ra.FileName
                |> Option.map System.Reflection.Assembly.LoadFile
            else None
        )
        |> Option.toObj   
    )

    let decl = 
        checkFileResults.AssemblyContents.ImplementationFiles
        |> Seq.collect (fun f -> f.Declarations)

//    let jsMeta = 
//        stopwatch "Creating WebSharper.JavaScript metadata" <| fun () ->
//        checkFileResults.ProjectContext.GetReferencedAssemblies()
//        |> List.find (fun a -> a.SimpleName = "WebSharper.JavaScript")
//        |> ToMetadata.stringInlines
//
//    let currentMeta = CommonAST.Metadata.union jsMeta (stopwatch "Creating WebSharper.Main metadata" <| fun () -> ToMetadata.transformAssembly decl)

//    let meta = 
//        match prevMeta with
//        | None -> currentMeta
//        | Some m -> CommonAST.Metadata.union m currentMeta

    let meta = stopwatch "Interpreting" <| fun () -> WebSharper.Compiler.FSharp.Translator.transformAssembly decl

    stopwatch "Translating" <| fun () -> 
        for node in meta.Translated.Values do 
            let toJS = WebSharper.Compiler.Common.ToJavaScript.ToJavaScript(meta)
            node.Body <- toJS.TransformExpression node.Body 
    meta 
    