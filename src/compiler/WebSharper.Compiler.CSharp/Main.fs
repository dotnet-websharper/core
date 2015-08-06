module RoslynTest.Main

#if INTERACTIVE
#r @"..\packages\System.Collections.Immutable.1.1.33-beta\lib\portable-net45+win8+wp8+wpa81\System.Collections.Immutable.dll"
#r @"..\packages\System.Reflection.Metadata.1.0.18-beta\lib\portable-net45+win8\System.Reflection.Metadata.dll"
#r @"..\packages\Microsoft.CodeAnalysis.CSharp.1.0.0-rc2\lib\net45\Microsoft.CodeAnalysis.CSharp.dll"
#r @"..\packages\Microsoft.CodeAnalysis.Common.1.0.0-rc2\lib\net45\Microsoft.CodeAnalysis.dll"

#load "TokenUnions.fs"
#load "InnerTransformers.fs"
#load "Transformer.fs"
#endif

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
        
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

let translateProject prevMeta (path: string) =
    Microsoft.CodeAnalysis.CSharp.Formatting.BinaryOperatorSpacingOptions.Single |> ignore
    let workspace = MSBuildWorkspace.Create(LoadMetadataForReferencedProjects = true)
    workspace.AssociateFileExtensionWithLanguage("csproj", LanguageNames.CSharp)
    let project = workspace.OpenProjectAsync(path).Result
    let compilation = project.GetCompilationAsync().Result
//    compilation.type
    let refMeta = ToMetadata.stringInlines compilation
    let depMeta = 
        match prevMeta with
        | None -> refMeta
        | Some m -> CommonAST.Metadata.union refMeta m
    let currentMeta = ToMetadata.transformAssembly compilation.Assembly
    let meta = CommonAST.Metadata.union depMeta currentMeta
    Translator.translateAssembly compilation meta
    meta //|> CommonAST.Packager.packageAssembly |> CommonAST.Packager.exprToString 
