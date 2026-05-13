// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

//#r @"C:\Program Files\dotnet\sdk\5.0.301\Microsoft\Microsoft.NET.Build.Extensions\net461\lib\System.Runtime.dll"
// "D:\repos\dotnet-websharper\core\packages\includes\NETStandard.Library.Ref\ref\netstandard2.1\System.Runtime.dll"
//#r "../../build\Release/CSharp/netstandard2.0/System.Collections.Immutable.dll"
#r "../../build/Release/CSharp/net10.0/Microsoft.CodeAnalysis.dll"
#r "../../build/Release/CSharp/net10.0/Microsoft.CodeAnalysis.CSharp.dll"
#r "../../build/Release/CSharp/net10.0/Mono.Cecil.dll"
#r "../../build/Release/CSharp/net10.0/Mono.Cecil.Mdb.dll"
#r "../../build/Release/CSharp/net10.0/Mono.Cecil.Pdb.dll"
#r "../../build/Release/CSharp/netstandard2.0/WebSharper.Compiler.CSharp.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Compiler.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.dll"
#r "../../build/Release/netstandard2.0/WebSharper.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.StdLib.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Web.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Sitelets.dll"


fsi.ShowDeclarationValues <- false

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release\netstandard2.0", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.StdLib"
        "WebSharper.Web"
        "WebSharper.Testing"
        //"WebSharper.Sitelets"
        //"WebSharper.Tests"
        //"WebSharper.InterfaceGenerator.Tests"
        //"FSharp.Core"
    ]

let metas =
    wsRefs |> Seq.choose(
        WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Core.Metadata.FullMetadata
    )

let metadata =
    WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas

let csharpRefs = 
    let fwDir = Path.GetDirectoryName(typeof<obj>.Assembly.Location)
    
    List.concat [
        [
            typeof<obj>
            typeof<unit>
        ]
        |> List.map (fun t ->
            let l = t.Assembly.Location
            MetadataReference.CreateFromFile(l) :> MetadataReference
        )
    
        [
            "netstandard.dll"
            "System.Runtime.dll"
        ]
        |> List.map (fun a ->
            let l = Path.Combine(fwDir, a) 
            MetadataReference.CreateFromFile(l) :> MetadataReference
        )

        wsRefs
        |> List.map (fun r ->
            MetadataReference.CreateFromFile(r) :> MetadataReference
        )
    ]

let stExpr s = WebSharper.Core.AST.StatementExpr(s, None)

let translate isBundle (source: string) = 

    let logger = WebSharper.Compiler.ConsoleLogger()

    let fileName = Path.ChangeExtension(Path.GetTempFileName(), ".cs")
    File.WriteAllText(fileName, source)

    let parseOptions = CSharpParseOptions(kind = SourceCodeKind.Regular)

    let syntaxTree = CSharpSyntaxTree.ParseText(source, parseOptions, path = fileName)
    
    let csharpCompilation =
        CSharpCompilation.Create(
            "TestProject",
            [ syntaxTree ],
            csharpRefs,
            CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
        )

    let diag = csharpCompilation.GetDiagnostics()

    for d in diag do
        printfn "%A" d

    let comp = 
        WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
            logger
            (WebSharper.Compiler.Compilation(metadata, UseLocalMacros = false))
            WebSharper.Compiler.CommandTools.WsConfig.Empty
            csharpCompilation

    let expressions =
        Seq.concat [
            comp.CompilingMethods |> Seq.map (fun (KeyValue(_,(_,_,a))) -> a)
            comp.CompilingConstructors |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingImplementations |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingStaticConstructors |> Seq.map (fun (KeyValue(_,a)) -> stExpr a)
        ]
        |> List.ofSeq

    //fsDeclarations |> List.iter (printfn "%s") 
    expressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiling: %s")

    try
        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
    with e ->
        printfn "Compile error: %A" e

    if not (List.isEmpty comp.Errors) then
        for pos, err in comp.Errors do
            printfn "WebSharper Error: %A %O" pos err 
    else

    let currentMeta = comp.ToCurrentMetadata()
    let compiledExpressions = 
        currentMeta.Classes.Values |> Seq.collect (
            function
            | _, _, Some c ->
                c.Methods.Keys |> Seq.iter (printfn "method: %A")
                Seq.concat [
                    c.Methods.Values |> Seq.map (fun a -> a.Expression)
                    c.Constructors.Values |> Seq.map (fun a -> a.Expression)
                    c.Implementations.Values |> Seq.map (fun a -> a.Expression)
                    c.StaticConstructor |> Option.map stExpr |> Option.toList |> Seq.ofList
                ]
            | _ -> Seq.empty
        )
        |> List.ofSeq 
        
    let errors =
        [
            for pos, e in comp.Errors -> pos, string e, true
            for pos, e in comp.Warnings -> pos, string e, false
        ]
    errors |> List.iter (printfn "%A")

    if isBundle then

        let graph =
            metas |> Seq.map (fun m -> m.Dependencies)
            |> Seq.append (Seq.singleton currentMeta.Dependencies)
            |> WebSharper.Core.DependencyGraph.Graph.FromData

        let nodes =
            graph.GetDependencies [ WebSharper.Core.Metadata.EntryPointNode ]
    
        printfn "nodes: %A" (nodes |> List.map string)

        let mergedMeta = 
            WebSharper.Core.Metadata.Info.UnionWithoutDependencies [ metadata; currentMeta ]

        let trimmedMeta = WebSharper.Compiler.CompilationHelpers.trimMetadata mergedMeta nodes
    
        //printfn "trimmedMeta: %A" trimmedMeta

        let pkg = WebSharper.Compiler.JavaScriptPackager.bundleAssembly WebSharper.Core.JavaScript.JavaScript trimmedMeta trimmedMeta "TestProject" comp.EntryPoint WebSharper.Compiler.JavaScriptPackager.EntryPointStyle.OptionalEntryPoint
    
        //printfn "packaged: %s" (WebSharper.Core.AST.Debug.PrintStatement (WebSharper.Core.AST.Block pkg))

        let trPkg, _ = WebSharper.Compiler.JavaScriptWriter.transformProgram WebSharper.Core.JavaScript.JavaScript WebSharper.Core.JavaScript.Readable pkg
        let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter trPkg false
        printfn "%s" js

    else
        
        let pkg = WebSharper.Compiler.JavaScriptPackager.packageAssembly WebSharper.Core.JavaScript.JavaScript metadata currentMeta "TestProject" false None WebSharper.Compiler.JavaScriptPackager.EntryPointStyle.OptionalEntryPoint
    
        let jsFiles = 
            pkg 
            |> Array.map (fun (file, p) ->
                let trP, _ = WebSharper.Compiler.JavaScriptWriter.transformProgram WebSharper.Core.JavaScript.JavaScript WebSharper.Core.JavaScript.Readable p
                let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter trP false
                file, js
            )

        compiledExpressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiled: %s")
        for (name, js) in jsFiles do 
            printfn "File: %s" name
            printfn "%s" js

let toJSFiles source =
    translate false source

let toJSBundle source =
    translate true source
