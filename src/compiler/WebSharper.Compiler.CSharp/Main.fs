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

namespace WebSharper.Compiler.CSharp

open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp  
open WebSharper.Compiler.CommandTools
open WebSharper.Compiler

module M = WebSharper.Core.Metadata
        
/// Creates WebSharper compilation for a C# project
type WebSharperCSharpCompiler() =

    member val UseGraphs = true with get, set
    member val UseVerifier = true with get, set

    /// The command line of the last compilation.
    member val CommandLineArguments : CSharpCommandLineArguments = null with get, set

    /// The Roslyn compilation of the last compilation.
    member val RoslynCompilation : CSharpCompilation = null with get, set

    member this.Compile (refMeta, config: WsConfig, ?logger: LoggerBase) =
        let path = config.ProjectFile
        let projectDir = System.IO.Path.GetDirectoryName path

        // csc's own command line when available (all options, needed for emitting the assembly
        // again), otherwise the one MSBuild passes to WebSharper
        let fullArgs =
            config.CompilerResponseFile |> Option.filter File.Exists |> Option.map (fun rsp ->
                CSharpCommandLineParser.Default.Parse([| "@" + rsp |], projectDir, null))

        let parsedArgs =
            match fullArgs with
            | Some a -> a
            | None ->
                CSharpCommandLineParser.Default.Parse(config.CompilerArgs, projectDir, null)

        // sources MSBuild passes to WebSharper but not to csc: files written by source
        // generators (EmitCompilerGeneratedFiles), csc generates their contents itself
        let extraSources =
            match fullArgs with
            | None -> [||]
            | Some a ->
                let cscSources = a.SourceFiles |> Seq.map (fun s -> s.Path) |> System.Collections.Generic.HashSet
                CSharpCommandLineParser.Default.Parse(config.CompilerArgs, projectDir, null).SourceFiles
                |> Seq.map (fun s -> s.Path)
                |> Seq.filter (cscSources.Contains >> not)
                |> Array.ofSeq

        let syntaxTrees =
            Seq.append (parsedArgs.SourceFiles |> Seq.map (fun s -> s.Path)) extraSources
            |> Seq.map (fun s -> WebSharper.Compiler.CSharp.Emit.ParseSourceFile parsedArgs s)
        
        let references = 
            match fullArgs with
            | Some a ->
                a.MetadataReferences |> Seq.map (fun r ->
                    MetadataReference.CreateFromFile(System.IO.Path.Combine(projectDir, r.Reference), r.Properties) :> MetadataReference)
                |> Array.ofSeq
            | None ->
                config.CompilerArgs
                |> Array.choose (fun a ->
                    if a.StartsWith "/reference:" then Some a.[11 ..] else None
                ) 
                |> Array.map (fun r ->
                    MetadataReference.CreateFromFile(r) :> MetadataReference
                )
        
        let compilation = 
            CSharpCompilation.Create(
                (match parsedArgs.CompilationName with
                 | null -> System.IO.Path.GetFileNameWithoutExtension path
                 | n -> n),
                syntaxTrees,
                references,
                WebSharper.Compiler.CSharp.Emit.CompilationOptions parsedArgs config.KeyFile
            )
        
        let firstError =
            compilation.GetDiagnostics() |> Seq.tryFind (fun d -> d.Severity = DiagnosticSeverity.Error)

        match firstError with
        | Some err ->
            failwithf "C# compilation resulted in errors: %s" (err.GetMessage())
        | _ -> ()

        this.CommandLineArguments <- parsedArgs
        this.RoslynCompilation <- compilation

        let logger = logger |> Option.defaultWith (fun () -> upcast EmptyLogger())
        logger.TimedStage "Creating Roslyn compilation" 
            
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
                logger
                (WebSharper.Compiler.Compilation(refMeta, SingleNoJSErrors = config.SingleNoJSErrors))
                config
                compilation

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        if this.UseVerifier then
            comp.VerifyRPCs()

        logger.TimedStage "WebSharper translation"

        comp

    static member Compile (prevMeta, compilation: CSharpCompilation, ?useGraphs, ?config: WsConfig, ?logger: LoggerBase) =
        let useGraphs = defaultArg useGraphs true
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        let logger = logger |> Option.defaultWith (fun () -> upcast EmptyLogger())
        
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly
                logger
                (WebSharper.Compiler.Compilation(refMeta, useGraphs, UseLocalMacros = false))
                (defaultArg config WsConfig.Empty)
                compilation

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp
