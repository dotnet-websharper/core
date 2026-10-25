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
namespace WebSharper.CSharp.Analyzer

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Threading
open System.Reflection
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Diagnostics
open System.IO
open WebSharper.Compiler

module FE = WebSharper.Compiler.FrontEnd

[<DiagnosticAnalyzer(LanguageNames.CSharp)>]
type WebSharperCSharpAnalyzer () =
    inherit DiagnosticAnalyzer()
   
    static let wsWarning = 
        new DiagnosticDescriptor ("WebSharperWarning", "WebSharper warnings", "{0}", "WebSharper", DiagnosticSeverity.Warning, true, null, null)

    static let wsError = 
        new DiagnosticDescriptor ("WebSharperError", "WebSharper errors", "{0}", "WebSharper", DiagnosticSeverity.Error, true, null, null)

    static let mutable testing = false

    // command line builds run the WebSharper compiler itself, analyzing only needed in IDEs
    // (csc.exe, VBCSCompiler.exe or csc.dll/VBCSCompiler.dll on .NET)
    static let isCommandLineCompiler =
        match System.Reflection.Assembly.GetEntryAssembly() with
        | null -> false
        | a ->
            match a.GetName().Name with
            | "csc" | "VBCSCompiler" -> true
            | _ -> false

    // assemblies loaded with the analyzer, references to these must not be loaded again,
    // so that for example macros from referenced assemblies inherit the same WebSharper.Core.Macro
    static let analyzerAssemblies =
        [
            typeof<option<_>>.Assembly
            typeof<WebSharper.WebResourceAttribute>.Assembly
            typeof<WebSharper.Core.Macro>.Assembly
            typeof<WebSharper.Compiler.Compilation>.Assembly
            typeof<WebSharper.Compiler.CSharp.WebSharperCSharpCompiler>.Assembly
        ]
        |> List.map (fun a -> a.GetName().Name, a)
        |> Map.ofList

    let mutable lastRefKey = [ box "" ]
    let mutable cachedRefErrorsAndMeta = None

    let cachedRefMeta = Dictionary()
    let compilationRefMeta = System.Runtime.CompilerServices.ConditionalWeakTable<CSharpCompilation, WebSharper.Core.Metadata.Info option ref>()

    let mutable referencedAsmPaths = Map.empty
    let loadedRefAsms = Dictionary<string, System.Reflection.Assembly>()

    let resolveHandler =
        ResolveEventHandler(fun _ e ->
            let assemblyName = AssemblyName(e.Name).Name
            match Map.tryFind assemblyName analyzerAssemblies with
            | Some a -> a
            | None ->
            match Map.tryFind assemblyName referencedAsmPaths with
            | None -> null
            | Some p ->
                lock loadedRefAsms <| fun () ->
                match loadedRefAsms.TryGetValue p with
                | true, a -> a
                | _ ->
                    // loading from bytes to not lock build outputs inside the IDE
                    let a = System.Reflection.Assembly.Load(File.ReadAllBytes p)
                    loadedRefAsms.Add(p, a)
                    a
        )

    do AppDomain.CurrentDomain.add_AssemblyResolve resolveHandler

    static member IsTest
        with get() = testing
        and set v = testing <- v

    member this.GetRefMeta(path) =
        lock cachedRefMeta <| fun () ->
        match cachedRefMeta.TryGetValue path with
        | true, (m, err, _) -> m, err
        | _ ->
            let mutable err = None
            let m = 
                try path |> FE.ReadFromFile WebSharper.Core.Metadata.MetadataOptions.DiscardNotInlineExpressions
                with e ->
                    err <- Some e.Message
                    None

            let watcher =
                try
                    let watcher = new FileSystemWatcher(Path.GetDirectoryName path, Path.GetFileName path)
                    let onChange (_: FileSystemEventArgs) =
                        lock cachedRefMeta <| fun () ->
                            lastRefKey <- [ box "" ]
                            cachedRefMeta.Remove(path) |> ignore
                            watcher.Dispose()

                    watcher.Changed |> Event.add onChange
                    watcher.Created |> Event.add onChange
                    watcher.Renamed |> Event.add onChange
                    watcher.Deleted |> Event.add onChange
                    watcher.EnableRaisingEvents <- true
                    Some watcher
                with _ -> None // directory does not exist

            cachedRefMeta.Add(path, (m, err, watcher))
            m, err

    interface IDisposable with
        member this.Dispose() =
            AppDomain.CurrentDomain.remove_AssemblyResolve resolveHandler
            lock cachedRefMeta <| fun () ->
                for _, _, watcher in cachedRefMeta.Values do
                    watcher |> Option.iter (fun w -> w.Dispose())
                cachedRefMeta.Clear()

    override this.SupportedDiagnostics =
        ImmutableArray.Create(wsWarning, wsError)

    override this.Initialize(initCtx) =
        initCtx.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        // A compilation action can't be used to detect full builds, Roslyn runs them
        // at compilation start, also when IDE analyzes a single document.
        if testing || not isCommandLineCompiler then
            initCtx.RegisterSemanticModelAction(fun ctx ->
                this.Analyze(ctx.SemanticModel.Compilation :?> CSharpCompilation, ctx)
            )

    // IDEs pass referenced C# projects as in-memory compilations, not as files
    member this.GetCompilationRefMeta(compilation: CSharpCompilation) =
        lock compilationRefMeta <| fun () ->
        match compilationRefMeta.TryGetValue compilation with
        | true, m -> m.Value
        | _ ->
            let m =
                try
                    let refErrors, refMeta = this.GetReferencesMeta(compilation)
                    if not (List.isEmpty refErrors) then None else
                    let comp = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler.Compile(refMeta, compilation, false)
                    Some (comp.ToCurrentMetadata(true))
                with _ -> None
            compilationRefMeta.Add(compilation, ref m)
            m

    member this.GetReferencesMeta(compilation: CSharpCompilation) =
        let metas =
            compilation.ExternalReferences |> Seq.choose (fun r ->
                match r with
                | :? PortableExecutableReference as r -> Some (this.GetRefMeta r.FilePath)
                | :? CompilationReference as r ->
                    match r.Compilation with
                    | :? CSharpCompilation as c -> Some (this.GetCompilationRefMeta c, None)
                    | _ -> None
                | _ -> None
            )
            |> List.ofSeq
        let refErrors = metas |> List.choose snd
        let refMeta =
            if List.isEmpty metas || not (List.isEmpty refErrors) then None
            else Some (WebSharper.Core.Metadata.Info.UnionWithoutDependencies (metas |> List.choose fst))
        refErrors, refMeta

    member this.Analyze(compilation: CSharpCompilation, ctx: SemanticModelAnalysisContext) =
        let refKey =
            compilation.ExternalReferences |> Seq.choose (fun r ->
                match r with
                | :? PortableExecutableReference as r -> Some (box r.FilePath)
                | :? CompilationReference as r -> Some (box r.Compilation)
                | _ -> None
            )
            |> List.ofSeq

        let refErrors, refMeta =
            if refKey = lastRefKey then
                cachedRefErrorsAndMeta.Value
            else
                referencedAsmPaths <-
                    refKey
                    |> Seq.choose (function
                        | :? string as p -> Some (Path.GetFileNameWithoutExtension(p), p)
                        | _ -> None
                    )
                    |> Map.ofSeq

                cachedRefErrorsAndMeta <- Some (this.GetReferencesMeta(compilation))
                lastRefKey <- refKey
                cachedRefErrorsAndMeta.Value

        if not (List.isEmpty refErrors) then
            // IDEs do not show diagnostics without location from semantic model actions
            let docStart = Location.Create(ctx.SemanticModel.SyntaxTree, Text.TextSpan(0, 0))
            for err in refErrors do
                ctx.ReportDiagnostic(Diagnostic.Create(wsError, docStart, "WebSharper analysis skipped, failed to read reference: " + err))
        else
            try
                if compilation.GetDiagnostics() |> Seq.exists (fun d -> d.Severity = DiagnosticSeverity.Error) then () else

                WebSharper.Compiler.CSharp.ProjectReader.SaveTextSpans()

                let comp = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler.Compile(refMeta, compilation, false)

                if ctx.CancellationToken.IsCancellationRequested then () else

                let loc (pos: WebSharper.Core.AST.SourcePos option) =
                    match pos with
                    | None -> Location.None
                    | Some p -> 
                        match WebSharper.Compiler.CSharp.ProjectReader.TextSpans.TryGetValue(p) with
                        | true, textSpan ->
                            let syntaxTree =
                                compilation.SyntaxTrees |> Seq.find (fun t -> t.FilePath = p.FileName)
                            Location.Create(syntaxTree, !textSpan)
                        | _ ->
                            Location.None

                for pos, wrn in comp.Warnings do
                    ctx.ReportDiagnostic(Diagnostic.Create(wsWarning, loc pos, string wrn))

                for pos, err in comp.Errors do
                    ctx.ReportDiagnostic(Diagnostic.Create(wsError, loc pos, string err))

            with e ->
                ctx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "WebSharper analyzer failed: %s at %s" e.Message e.StackTrace))
