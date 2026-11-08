// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

    /// The cache of reading source files, set in the booster, see ReadCache.
    member val ReadCache : ReadCache.Cache option = ReadCache.Cache.Shared with get, set
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

        // argv.[0] is the compiler path or "fsc.exe", skip only that, the first argument is usually -o:
        let args = if argv.Length > 0 then argv.[1 ..] else argv

        let projectOptionsOpt =
            try
                if checker.UsesTransparentCompiler then
                    // Unlike the IncrementalBuilder, the TransparentCompiler only checks
                    // FSharpProjectOptions.SourceFiles, not source files left in the arguments.
                    let sources, otherArgs = args |> Array.partition (fun a -> not (a.StartsWith "-"))
                    { checker.GetProjectOptionsFromCommandLineArgs(path, otherArgs) with SourceFiles = sources } |> Some
                else
                checker.GetProjectOptionsFromCommandLineArgs(path, args) |> Some
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

        let readCacheContext =
            this.ReadCache |> Option.map (fun cache ->
                let projectDir = config.ProjectDir
                let fullPath (f: string) = Path.GetFullPath(Path.Combine(projectDir, f))
                let settings =
                    String.concat "|" [
                        assemblyName
                        config.ProjectFile
                        string config.ProjectType
                        string config.JavaScriptScope
                        config.JavaScriptExport |> Seq.map string |> String.concat ","
                        string config.StubInterfaces
                        string config.RemotingProvider
                        string config.ProxyTargetName
                        string config.UseJavaScriptSymbol
                        string config.SingleNoJSErrors
                        string this.UseGraphs
                        string config.ReferenceReadOptions
                    ]
                let projectKey = ReadCache.ProjectKey projectOptions.OtherOptions config.References settings
                ReadCache.Context(cache, fullPath config.AssemblyFile, projectKey, projectOptions.SourceFiles |> Array.map fullPath)
            )

        let read (readCache: ReadCache.Context option) =
            let comp =
                WebSharper.Compiler.FSharp.ProjectReader.transformAssemblyWith
                    logger
                    (WebSharper.Compiler.Compilation(refMeta, this.UseGraphs, SingleNoJSErrors = config.SingleNoJSErrors))
                    assemblyName
                    config
                    checkProjectResults
                    readCache

            WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            comp

        // WebSharperReadCacheCheck: reads replayed files again, compares the translated results
        let checkReadCache (comp: WebSharper.Compiler.Compilation) =
            match readCacheContext with
            | Some rc when rc.Cache.Check && rc.Hits > 0 ->
                let fresh = read None
                let describe (c: WebSharper.Compiler.Compilation) =
                    let meta = WebSharper.Core.MetadataIO.ContentHash (c.ToCurrentMetadata(true))
                    let errors = c.Errors |> List.map (sprintf "%A") |> String.concat "\n"
                    let warnings = c.Warnings |> List.map (sprintf "%A") |> String.concat "\n"
                    meta, errors, warnings
                let (m1, e1, w1), (m2, e2, w2) = describe comp, describe fresh
                if m1 = m2 && e1 = e2 && w1 = w2 then
                    logger.Out (sprintf "WebSharper read cache check: %d replayed files give the same result" rc.Hits)
                else
                    let parts =
                        [
                            if m1 <> m2 then "metadata"
                            if e1 <> e2 then "errors"
                            if w1 <> w2 then "warnings"
                        ]
                    comp.AddWarning(None, WebSharper.Compiler.SourceWarning (sprintf "WebSharper read cache check: replaying %d files gives different %s than reading them" rc.Hits (String.concat ", " parts)))
                logger.TimedStage "Checking read cache"
            | _ -> ()

        let translate () =
            let comp = read readCacheContext
            checkReadCache comp

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
