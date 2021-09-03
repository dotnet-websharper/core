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

    let mutable compiling = false;
    let mutable lastRefPaths = [ "" ]
    let mutable cachedRefErrorsAndMeta = None

    let cachedRefMeta = Dictionary() 

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

            let watcher = new FileSystemWatcher(Path.GetDirectoryName path, Path.GetFileName path)
            let onChange (_: FileSystemEventArgs) =
                lock cachedRefMeta <| fun () ->
                    lastRefPaths <- [ "" ]
                    cachedRefMeta.Remove(path) |> ignore
                    watcher.Dispose() 

            watcher.Changed |> Event.add onChange
            watcher.Renamed |> Event.add onChange
            watcher.Deleted |> Event.add onChange

            cachedRefMeta.Add(path, (m, err, watcher))    
            m, err        

    interface IDisposable with
        member this.Dispose() =
            lock cachedRefMeta <| fun () ->
                for _, _, watcher in cachedRefMeta.Values do
                    watcher.Dispose()
                cachedRefMeta.Clear()

    override this.SupportedDiagnostics =
        ImmutableArray.Create(wsWarning, wsError)

    override this.Initialize(initCtx) =
        initCtx.RegisterCompilationAction(fun startCtx ->
            compiling <- true
        )
        initCtx.RegisterSemanticModelAction(fun ctx ->
            if compiling then () else
                this.Analyze(ctx.SemanticModel.Compilation :?> CSharpCompilation, ctx)
        )

    member this.Analyze(compilation: CSharpCompilation, ctx: SemanticModelAnalysisContext) =
            let refPaths =
                compilation.ExternalReferences |> Seq.choose (fun r -> 
                    match r with
                    | :? PortableExecutableReference as cr -> Some cr.FilePath
                    | _ -> None
                )
                |> List.ofSeq
            
            let refErrors, refMeta =
                if refPaths = lastRefPaths then
                    cachedRefErrorsAndMeta.Value
                else
                
                    let referencedAsmNames =
                        refPaths
                        |> Seq.map (fun i -> 
                            let n = Path.GetFileNameWithoutExtension(i)
                            n, i
                        )
                        |> Map.ofSeq

                    System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ e ->
                        let assemblyName = AssemblyName(e.Name).Name
                        if assemblyName = "FSharp.Core" then
                            typeof<option<_>>.Assembly
                        else
                        match Map.tryFind assemblyName referencedAsmNames with
                        | None -> null
                        | Some p -> Assembly.LoadFrom(p)
                    )

                    let metas = refPaths |> List.map this.GetRefMeta
                    let refErrors = metas |> List.choose snd

                    let refMeta =
                        if List.isEmpty metas || not (List.isEmpty refErrors) then None 
                        else Some (WebSharper.Core.Metadata.Info.UnionWithoutDependencies (metas |> List.choose fst))

                    cachedRefErrorsAndMeta <- Some (refErrors, refMeta)
                    cachedRefErrorsAndMeta.Value

            if not (List.isEmpty refErrors) then
                for err in refErrors do    
                    ctx.ReportDiagnostic(Diagnostic.Create(wsError, Location.None, err))
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
