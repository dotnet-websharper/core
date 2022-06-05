module Analyzer.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Testing
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Testing.Verifiers

open WebSharper.CSharp.Analyzer
open Microsoft.CodeAnalysis.Testing
open System.Collections.Immutable

[<SetUp>]
let Setup () =
    ()
 
#if DEBUG
let configuration = "Debug"
let isDebug = true
#else
let configuration = "Release"
let isDebug = false
#endif

let projRefs =
    [
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Collections.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Control.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Core.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Core.JavaScript.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.InterfaceGenerator.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.JavaScript.dll"
        $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Main.dll"
    ]
    |> List.map (fun p -> 
        MetadataReference.CreateFromFile(Path.Combine(__SOURCE_DIRECTORY__, p)) :> MetadataReference
    )

let packageRefs = 
    [
        PackageIdentity("FSharp.Core", "6.0.3")
    ]

[<Test>]
let ``Analyzer test`` () =
    WebSharperCSharpAnalyzer.IsTest <- true
    let test = CSharpAnalyzerTest<WebSharperCSharpAnalyzer, NUnitVerifier>(
        TestCode = """
using System;
using WebSharper;    

[JavaScript]
class TestClass {
    public static void TestBeep() {
        Console.Beep();
    }
}
""",
        ReferenceAssemblies = 
            ReferenceAssemblies(
                "net6.0",
                new PackageIdentity("Microsoft.NETCore.App.Ref", "6.0.0"), 
                @"ref\net6.0"
            )
    )
    test.TestState.AdditionalReferences.AddRange(projRefs);
    let expectedDiagnostic =
        CSharpAnalyzerVerifier<WebSharperCSharpAnalyzer, NUnitVerifier>.Diagnostic("WebSharperError")
            .WithSpan(8, 9, 8, 23)
            .WithArguments("Method name not found in JavaScript compilation: System.Console.(Beep : unit -> unit), Members: WriteLine")
    test.ExpectedDiagnostics.Add(expectedDiagnostic);
    test.RunAsync()