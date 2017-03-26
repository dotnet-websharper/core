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

namespace WebSharper.Compiler.CSharp

open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp  
open WebSharper.Compiler.ErrorPrinting
open WebSharper.Compiler.FrontEnd

module M = WebSharper.Core.Metadata
        
/// Creates WebSharper compilation for a C# project
type WebSharperCSharpCompiler(logger) =

    member val UseGraphs = true with get, set
    member val UseVerifier = true with get, set

    member this.Compile (refMeta, argv: seq<string>, path: string) =

        let parsedArgs =
            CSharpCommandLineParser.Default.Parse(
                argv, 
                System.IO.Path.GetDirectoryName path,
                null
            )

        let syntaxTrees =
            parsedArgs.SourceFiles 
            |> Seq.map (fun s -> 
                CSharpSyntaxTree.ParseText(File.ReadAllText s.Path, path = s.Path)
            )
        
        let references = 
            argv
            |> Seq.choose (fun a ->
                if a.StartsWith "/reference:" then Some a.[11 ..] else None
            ) 
            |> Seq.map (fun r ->
                MetadataReference.CreateFromFile(r) :> MetadataReference
            )

        let compilation = 
            CSharpCompilation.Create(
                System.IO.Path.GetFileNameWithoutExtension path,
                syntaxTrees,
                references,
                parsedArgs.CompilationOptions
            )
        
        let firstError =
            compilation.GetDiagnostics() |> Seq.tryFind (fun d -> d.Severity = DiagnosticSeverity.Error)

        match firstError with
        | Some err ->
            failwithf "C# compilation resulted in errors: %s" (err.GetMessage())
        | _ -> ()

        TimedStage "Creating Roslyn compilation" 
            
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta))
                compilation

        TimedStage "Parsing with Roslyn"

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        if this.UseVerifier then
            comp.VerifyRPCs()

        TimedStage "WebSharper translation"

        comp

    static member Compile (prevMeta, compilation: CSharpCompilation, ?useGraphs) =
        let useGraphs = defaultArg useGraphs true
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta, useGraphs, UseLocalMacros = false))
                compilation

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp
