namespace WebSharper.Compiler.CSharp

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
  
module M = WebSharper.Core.Metadata
        
//open WebSharper.Compiler
     
//module S = WebSharper.Core.JavaScript.Syntax

//let translateFile (source: string) = 
//    //let source = System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + @"\..\CSharpTestLibrary\Class1.cs")
//    let tree = CSharpSyntaxTree.ParseText source
//    let root = tree.GetRoot() :?> CompilationUnitSyntax
//    let compilation =
//        CSharpCompilation.Create("HelloWorld")
//            .AddReferences(MetadataReference.CreateFromAssembly(typeof<obj>.Assembly))
//            .AddReferences(MetadataReference.CreateFromAssembly(typeof<WebSharper.Core.Attributes.InlineAttribute>.Assembly))
//            .AddSyntaxTrees(tree)
//    //let model = compilation.GetSemanticModel(tree, ignoreAccessibility = true)
//    
//    let meta = ToMetadata.transformAssembly compilation.Assembly
//
//    Translator.translateAssembly compilation meta
//    
//    meta |> CommonAST.Packager.packageAssembly |> CommonAST.Packager.exprToString 

open Microsoft.CodeAnalysis.MSBuild

//let translateProject (path: string) =
//    async {
//        let workspace = MSBuildWorkspace.Create()
//        let! project = workspace.OpenProjectAsync(path) |> Async.AwaitTask
//        let! compilation = project.GetCompilationAsync() |> Async.AwaitTask
//        let meta = ToMetadata.transformAssembly compilation.Assembly
//        Translator.translateAssembly compilation meta
//        return meta |> Packager.packageAssembly |> Packager.exprToString 
//    }

//let runTask task = task |> Async.AwaitTask |> Async.RunSynchronously

type WebSharperCSharpCompiler(logger) =

    member this.Compile (prevMeta, argv, path: string) =

//        let cArgs = CSharpCommandLineParser.Default.Parse(argv, System.IO.Path.GetDirectoryName path, "")

        let started = System.DateTime.Now

        let workspace = MSBuildWorkspace.Create(LoadMetadataForReferencedProjects = true)
        workspace.AssociateFileExtensionWithLanguage("csproj", LanguageNames.CSharp)
        let project = workspace.OpenProjectAsync(path).Result
        
        let ended = System.DateTime.Now
        logger <| sprintf "Loading project: %A" (ended - started)
        let started = ended 
        
        let compilation = project.GetCompilationAsync().Result :?> CSharpCompilation
        
        let ended = System.DateTime.Now
        logger <| sprintf "Creating compilation: %A" (ended - started)
        let started = ended 

        // TODO : errors
//        if compilation. checkFileResults.HasCriticalErrors then
//            for err in checkFileResults.Errors do
//                printfn "%s" err.Message
//            failwith "FSharp compilation error"
    
//        printfn "No errors. Parsing..."

//        checker.StartBackgroundCompile projectOptions

        let refMeta =   
            match prevMeta with
            | None -> M.empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.CSharp.Translator.transformAssembly refMeta
                compilation

        let ended = System.DateTime.Now
        logger <| sprintf "Parsing with FCS: %A" (ended - started)
        let started = ended 

//        let comp =
//            match prevMeta with
//            | None -> comp
//            | Some dep -> WebSharper.Core.Metadata.union dep comp  


        let ended = System.DateTime.Now
        logger <| sprintf "Metadata union: %A" (ended - started)
        let started = ended 

//        printfn "Done. Compiling..."
//        checker.WaitForBackgroundCompile()

        WebSharper.Compiler.ToJavaScript.ToJavaScript.CompileFull comp
            
        let ended = System.DateTime.Now
        logger <| sprintf "Transforming: %A" (ended - started)
//        let started = ended 

        comp

//let translateProject prevMeta (path: string) =
//    let workspace = MSBuildWorkspace.Create(LoadMetadataForReferencedProjects = true)
//    workspace.AssociateFileExtensionWithLanguage("csproj", LanguageNames.CSharp)
//    let project = workspace.OpenProjectAsync(path).Result
//    let compilation = project.GetCompilationAsync().Result
//
//    let refMeta = ToMetadata.stringInlines compilation
//    let depMeta = 
//        match prevMeta with
//        | None -> refMeta
//        | Some m -> WebSharper.Core.Metadata.union refMeta m
//    let currentMeta = ToMetadata.transformAssembly compilation.Assembly
//    let meta = WebSharper.Core.Metadata.union depMeta currentMeta
//    WebSharper.Compiler.Translator.translateAssembly compilation meta
//    meta //|> CommonAST.Packager.packageAssembly |> CommonAST.Packager.exprToString 
