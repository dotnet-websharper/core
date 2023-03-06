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
#r "../../build/Release/CSharp/net6.0/Microsoft.CodeAnalysis.dll"
#r "../../build/Release/CSharp/net6.0/Microsoft.CodeAnalysis.CSharp.dll"
#r "../../build/Release/CSharp/net6.0/Mono.Cecil.dll"
#r "../../build/Release/CSharp/net6.0/Mono.Cecil.Mdb.dll"
#r "../../build/Release/CSharp/net6.0/Mono.Cecil.Pdb.dll"
#r "../../build/Release/CSharp/netstandard2.0/WebSharper.Compiler.CSharp.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Compiler.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.dll"
#r "../../build/Release/netstandard2.0/WebSharper.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Main.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Collections.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Control.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Web.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Sitelets.dll"


fsi.ShowDeclarationValues <- false

open System
open System.IO
open System.Collections.Generic
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release\netstandard2.0", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.Main"
        "WebSharper.Collections"
        "WebSharper.Control"
        "WebSharper.Web"
        "WebSharper.MathJS"
        "WebSharper.MathJS.Extensions"
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
    { 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies false metas with
            Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
    }

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

let translate (source: string) = 

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = source
    File.WriteAllText(fileName1, fileSource1)

    //let args = mkProjectCommandLineArgs (dllName, [fileName1])
    //let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

    //let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    //if wholeProjectResults.HasCriticalErrors then
    //    for err in wholeProjectResults.Diagnostics |> Seq.filter (fun e -> e.Severity = Diagnostics.FSharpDiagnosticSeverity.Error) do
    //        printfn "F# Error: %d:%d-%d:%d %s" err.StartLine err.StartColumn err.EndLine err.EndColumn err.Message
    //else
    //let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]

    //let fsDeclarations = 
    //     file1.Declarations |> Utils.printDeclarations None |> List.ofSeq

    let logger = WebSharper.Compiler.ConsoleLogger()

    let parseOptions = CSharpParseOptions(kind = SourceCodeKind.Script)

    let syntaxTree = CSharpSyntaxTree.ParseText(source, parseOptions)
    
    let csharpCompilation =
        CSharpCompilation.CreateScriptCompilation("Script", syntaxTree, csharpRefs)

    let diag = csharpCompilation.GetDiagnostics()

    for d in diag do
        printfn "%A" d

    let comp = 
        WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
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

    let graph =
        metas |> Seq.map (fun m -> m.Dependencies)
        |> Seq.append (Seq.singleton currentMeta.Dependencies)
        |> WebSharper.Core.DependencyGraph.Graph.FromData

    let nodes =
        graph.GetDependencies [ WebSharper.Core.Metadata.EntryPointNode ]
    
    printfn "nodes: %A" (nodes |> List.map string)

    let mergedMeta = 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies true [ metadata; currentMeta ]

    let trimmedMeta = WebSharper.Compiler.CompilationHelpers.trimMetadata mergedMeta nodes
    
    //printfn "trimmedMeta: %A" trimmedMeta

    let pkg = WebSharper.Compiler.JavaScriptPackager.bundleAssembly WebSharper.Core.JavaScript.JavaScript trimmedMeta trimmedMeta "TestProject" comp.EntryPoint WebSharper.Compiler.JavaScriptPackager.OnLoadIfExists
    
    //printfn "packaged: %s" (WebSharper.Core.AST.Debug.PrintStatement (WebSharper.Core.AST.Block pkg))

    let js, map = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.JavaScript WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter pkg
    printfn "%s" js

translate """
using System;
using WebSharper;
using WebSharper.JavaScript;

[JavaScript]
interface ITestDefaultImpl
{
    int Foo() => 42;
    int Bar() => this.Foo();
}

[JavaScript]
class TestDefaultImpl : ITestDefaultImpl
{
    [SPAEntryPoint]
    public static void Main() 
    {
        var arr = new[] { 1 };
        Console.Log(arr[0]);
    }
}
"""
