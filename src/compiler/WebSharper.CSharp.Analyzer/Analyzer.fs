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

    let mutable cachedRefMeta = None

    override this.SupportedDiagnostics =
        ImmutableArray.Create(wsWarning, wsError)

    override this.Initialize(initCtx) =
        let mutable count = 0

//        initCtx.RegisterCompilationStartAction(fun startCtx ->
//            startCtx.RegisterCompilationEndAction(fun endCtx ->
//            )
//        )

        initCtx.RegisterCompilationAction(fun compCtx ->
            
            try
                let compilation = compCtx.Compilation :?> CSharpCompilation
            
                let refMeta =
                    match cachedRefMeta with
                    | Some res -> res
                    | _ ->
                    let refPaths =
                        compilation.ExternalReferences |> Seq.choose (fun r -> 
                            match r with
                            | :? PortableExecutableReference as cr -> Some cr.FilePath
                            | _ -> None
                        )
                    let aR =
                        AssemblyResolver.Create()
                            .SearchPaths(refPaths)
                    let loader = WebSharper.Compiler.FrontEnd.Loader.Create aR ignore
                    let refs = [ for r in refPaths -> loader.LoadFile(r) ]
                    let metas = refs |> List.choose (fun r -> WebSharper.Compiler.FrontEnd.ReadFromAssembly r)
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
                    
                    let res =
                        if List.isEmpty metas then None 
                        else Some (WebSharper.Core.DependencyGraph.Graph.UnionOfMetadata metas)
                    
                    cachedRefMeta <- Some res
                    res

                if compCtx.CancellationToken.IsCancellationRequested then () else

                let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler(ignore)

                let comp =
                    compiler.Compile(refMeta, compilation)

                if compCtx.CancellationToken.IsCancellationRequested then () else

                let loc (pos: WebSharper.Core.AST.SourcePos option) =
                    match pos with
                    | None -> Location.None
                    | Some p -> 
//                        Text.TextSpan(snd p.Start - 1, snd p.End - 1)
//                        let lp (line, col) = Text.LinePosition(line - 1, col - 1) 
//                        let lineSpan = Text.LinePositionSpan(lp p.Start, lp p.End)
                        match WebSharper.Compiler.CSharp.ProjectReader.textSpans.TryGetValue(p) with
                        | true, textSpan ->
                            let syntaxTree =
                                compilation.SyntaxTrees |> Seq.find (fun t -> t.FilePath = p.FileName)
                            Location.Create(syntaxTree, !textSpan)
                        | _ ->
                            Location.None

                for pos, wrn in comp.Warnings do
                    compCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, loc pos, string wrn))

                for pos, err in comp.Errors do
                    compCtx.ReportDiagnostic(Diagnostic.Create(wsError, loc pos, string err))

//                let files =
//                    compilation.SyntaxTrees |> Seq.map (fun t -> System.IO.Path.GetFileNameWithoutExtension t.FilePath)
//                    |> String.concat " "

                count <- count + 1
                compCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "WebSharper analyzer finished (%d): %d errors, %d warnings" count comp.Errors.Length comp.Warnings.Length))
                compCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "Another diagnostic"))
            with e ->
                count <- count + 1
                compCtx.ReportDiagnostic(Diagnostic.Create(wsWarning, Location.None, sprintf "WebSharper analyzer failed (%d): %s at %s" count e.Message e.StackTrace))            
        )

