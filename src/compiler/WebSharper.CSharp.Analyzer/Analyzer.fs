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

[<DiagnosticAnalyzer(LanguageNames.CSharp)>]
type WebSharperCSharpAnalyzer () =
    inherit DiagnosticAnalyzer()
   
    static let wsWarning = 
        new DiagnosticDescriptor ("WebSharperWarning", "WebSharper warnings", "{0}", "WebSharper", DiagnosticSeverity.Warning, true, null, null)

    static let wsError = 
        new DiagnosticDescriptor ("WebSharperError", "WebSharper errors", "{0}", "WebSharper", DiagnosticSeverity.Error, true, null, null)

    let cachedRefMeta = Dictionary() 

    member this.GetRefMeta(path) =
        lock cachedRefMeta <| fun () ->
        match cachedRefMeta.TryGetValue path with
        | true, (m, err, _) -> m, err
        | _ ->
            let mutable err = None
            let m = 
                try WebSharper.Compiler.FrontEnd.ReadFromFile(path)
                with e ->
                    err <- Some e.Message
                    None

            let watcher = new FileSystemWatcher(Path.GetDirectoryName path, Path.GetFileName path)
            let onChange (_: FileSystemEventArgs) =
                lock cachedRefMeta <| fun () ->
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
        initCtx.RegisterCompilationStartAction(fun startCtx ->
            let compilation = startCtx.Compilation :?> CSharpCompilation

            let refPaths =
                compilation.ExternalReferences |> Seq.choose (fun r -> 
                    match r with
                    | :? PortableExecutableReference as cr -> Some cr.FilePath
                    | _ -> None
                )
                |> List.ofSeq
                
            let metas = refPaths |> List.map this.GetRefMeta
            let errors = metas |> List.choose snd

            let refMeta =
                if List.isEmpty metas || not (List.isEmpty errors) then None 
                else Some (WebSharper.Core.DependencyGraph.Graph.UnionOfMetadata (metas |> List.choose fst))

            startCtx.RegisterCompilationEndAction(fun endCtx ->
                if not (List.isEmpty errors) then
                    for err in errors do    
                        endCtx.ReportDiagnostic(Diagnostic.Create(wsError, Location.None, err))
                else
                try
                    let compilation = endCtx.Compilation :?> CSharpCompilation

                    if compilation.GetDiagnostics() |> Seq.exists (fun d -> d.Severity = DiagnosticSeverity.Error) then () else

                    let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler(ignore)

                    let referencedAsmNames =
                        refPaths
                        |> Seq.map (fun i -> 
                            let n = Path.GetFileNameWithoutExtension(i)
                            n, i
                        )
                        |> Map.ofSeq

                    let assemblyResolveHandler = ResolveEventHandler(fun _ e ->
                            let assemblyName = AssemblyName(e.Name).Name
                            match Map.tryFind assemblyName referencedAsmNames with
                            | None -> null
                            | Some p -> 
                                if assemblyName = "FSharp.Core" then
                                    typeof<option<_>>.Assembly
                                else
                                    Assembly.LoadFrom(p)
                        )

                    System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)

                    let comp = compiler.Compile(refMeta, compilation)

                    if endCtx.CancellationToken.IsCancellationRequested then () else

                    let loc (pos: WebSharper.Core.AST.SourcePos option) =
                        match pos with
                        | None -> Location.None
                        | Some p -> 
                            match WebSharper.Compiler.CSharp.ProjectReader.textSpans.TryGetValue(p) with
                            | true, textSpan ->
                                let syntaxTree =
                                    compilation.SyntaxTrees |> Seq.find (fun t -> t.FilePath = p.FileName)
                                Location.Create(syntaxTree, !textSpan)
                            | _ ->
                                Location.None

                    for pos, wrn in comp.Warnings do
                        endCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, loc pos, string wrn))

                    for pos, err in comp.Errors do
                        endCtx.ReportDiagnostic(Diagnostic.Create(wsError, loc pos, string err))

                with e ->
                    endCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "WebSharper analyzer failed: %s at %s" e.Message e.StackTrace))            
            )
        )

