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

namespace WebSharper.Compiler.FSharp

// FSharpChecker.UsesTransparentCompiler is marked experimental in FCS
#nowarn "57"

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open WebSharper.Compiler
open WebSharper.Compiler.CommandTools
open ErrorPrinting

open System.IO

module M = WebSharper.Core.Metadata

type internal FSIFD = FSharpImplementationFileDeclaration

/// Creates WebSharper compilation for an F# project
type WebSharperFSharpCompiler(?checker) =
    let checker = checker |> Option.defaultWith (fun () -> FSharpChecker.Create(keepAssemblyContents = true))

    member val UseGraphs = true with get, set
    member val UseVerifier = true with get, set
    member val WarnSettings = WarnSettings.Default with get, set

    /// Diagnostics of the last type check, when it had errors not printed by Compile, which returned None.
    member val CheckDiagnostics : FSharpDiagnostic[] = [||] with get, set

    /// wrapTranslation runs around the WebSharper translation only, not the F# type check:
    /// type providers must not be loaded while an AssemblyResolver is installed, otherwise
    /// their dependencies can be loaded a second time into its collectible context.
    member this.Compile (prevMeta : System.Threading.Tasks.Task<option<M.Info>>, argv: string[], config: WsConfig, assemblyName, ?logger: LoggerBase, ?wrapTranslation: (unit -> WebSharper.Compiler.Compilation) -> WebSharper.Compiler.Compilation) =
        let path = config.ProjectFile
        let logger = logger |> Option.defaultWith (fun () -> upcast EmptyLogger())
        
        logger.DebugWrite "WebSharper compilation arguments:"
        argv |> Array.iter (sprintf "    %s" >> logger.DebugWrite)

        let argv =
            if argv.Length > 0 && argv.[0] = "fsc.exe" then argv.[1 ..] else argv

        let projectOptionsOpt =
            try
                if checker.UsesTransparentCompiler then
                    // Unlike the IncrementalBuilder, the TransparentCompiler only checks
                    // FSharpProjectOptions.SourceFiles, not source files left in the arguments.
                    let sources, otherArgs = argv.[1 ..] |> Array.partition (fun a -> not (a.StartsWith "-"))
                    { checker.GetProjectOptionsFromCommandLineArgs(path, otherArgs) with SourceFiles = sources } |> Some
                else
                checker.GetProjectOptionsFromCommandLineArgs(path, argv.[1 ..]) |> Some
            with e ->
                None

        match projectOptionsOpt with
        | None -> None
        | Some projectOptions ->

        let checkProjectResults = 
            projectOptions
            |> checker.ParseAndCheckProject 
            |> Async.RunSynchronously

        logger.TimedStage "Checking project"

        // GetResult rethrows the task's exception preserving its stack trace
        prevMeta.GetAwaiter().GetResult()
        |> function
        | None -> None
        | Some refMeta ->

        logger.TimedStage "Waiting on merged metadata"

        if checkProjectResults.Diagnostics |> Array.exists (fun e -> e.Severity = FSharpDiagnosticSeverity.Error && not (this.WarnSettings.NoWarn.Contains e.ErrorNumber)) then
            if assemblyName = "WebSharper.StdLib" || config.ProjectType = Some BundleOnly || config.ProjectType = Some Proxy then
                PrintFSharpErrors this.WarnSettings logger checkProjectResults.Diagnostics
            else
                this.CheckDiagnostics <- checkProjectResults.Diagnostics
            None
        else

        let translate () =
            let comp =
                WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                    logger
                    (WebSharper.Compiler.Compilation(refMeta, this.UseGraphs, SingleNoJSErrors = config.SingleNoJSErrors))
                    assemblyName
                    config
                    checkProjectResults

            WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp

            if this.UseVerifier then
                comp.VerifyRPCs()

            logger.TimedStage "WebSharper translation"

            comp

        match wrapTranslation with
        | Some wrap -> Some (wrap translate)
        | None -> Some (translate ())

    static member Compile (prevMeta, assemblyName, checkProjectResults: FSharpCheckProjectResults, ?useGraphs, ?config: WsConfig, ?logger: LoggerBase) =
        let useGraphs = defaultArg useGraphs true
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        let logger = logger |> Option.defaultWith (fun () -> upcast EmptyLogger())
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                logger
                (WebSharper.Compiler.Compilation(refMeta, useGraphs, UseLocalMacros = false))
                assemblyName
                (defaultArg config WsConfig.Empty)
                checkProjectResults

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp
