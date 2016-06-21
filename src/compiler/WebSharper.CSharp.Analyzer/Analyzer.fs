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

    let mutable assemblyResolveHandler =
        ResolveEventHandler(fun _ e ->
            if AssemblyName(e.Name).Name = "FSharp.Core" then
                typeof<option<_>>.Assembly
            else null
        )

    let mutable compiling = false;

    do  System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)

    let mutable assemblyResolveHandler = null
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
                try WebSharper.Compiler.FrontEnd.ReadFromFile(path)
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

            let compilation = ctx.SemanticModel.Compilation :?> CSharpCompilation

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

                    System.AppDomain.CurrentDomain.remove_AssemblyResolve(assemblyResolveHandler)

                    assemblyResolveHandler <- ResolveEventHandler(fun _ e ->
                            let assemblyName = AssemblyName(e.Name).Name
                            if assemblyName = "FSharp.Core" then
                                typeof<option<_>>.Assembly
                            else
                                match Map.tryFind assemblyName referencedAsmNames with
                                | None -> null
                                | Some p -> Assembly.LoadFrom(p)
                        )

                    System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)

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

                let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler(ignore, UseGraphs = false)

                let comp = compiler.Compile(refMeta, compilation)

                if ctx.CancellationToken.IsCancellationRequested then () else

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
                    ctx.ReportDiagnostic(Diagnostic.Create(wsWarning, loc pos, string wrn))

                for pos, err in comp.Errors do
                    ctx.ReportDiagnostic(Diagnostic.Create(wsError, loc pos, string err))

            with e ->
                ctx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "WebSharper analyzer failed: %s at %s" e.Message e.StackTrace))            
        )

