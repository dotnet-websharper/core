namespace WebSharper.Compiler.FSharp

open Microsoft.FSharp.Compiler.SourceCodeServices
open WebSharper.Compiler.ErrorPrinting

open System.IO

module M = WebSharper.Core.Metadata

type internal FSIFD = FSharpImplementationFileDeclaration

type WebSharperFSharpCompiler(logger) =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
//    let service = SimpleSourceCodeServices()

    let fullpath cwd nm = 
        let p = if Path.IsPathRooted(nm) then nm else Path.Combine(cwd,nm)
        try Path.GetFullPath(p) with 
        | :? System.ArgumentException 
        | :? System.ArgumentNullException 
        | :? System.NotSupportedException 
        | :? System.IO.PathTooLongException 
        | :? System.Security.SecurityException -> p

    member this.PrintErrors(errors : Microsoft.FSharp.Compiler.FSharpErrorInfo[], path) =
        let projDir = Path.GetDirectoryName path
        for err in errors do
            let pos =
                let fn = err.FileName
                if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                    let file = (fullpath projDir fn).Replace("/","\\")
                    sprintf "%s(%d,%d,%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                else ""
            let info =
                sprintf "%s %s FS%04d: " err.Subcategory 
                    (if err.Severity = Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning then "warning" else "error") err.ErrorNumber
                        
            eprintfn "%s%s%s" pos info (NormalizeErrorString err.Message)

    member this.Compile (prevMeta, argv, path: string, warnOnly) = 

        let started = System.DateTime.Now

        let projectOptions =
            try
                checker.GetProjectOptionsFromCommandLineArgs(path, argv)
            with e ->
//                printfn "Error: %s" e.Message
                failwithf "Error reading project options: %s" path

        let checkFileResults = 
            projectOptions
            |> checker.ParseAndCheckProject 
            |> Async.RunSynchronously

        let ended = System.DateTime.Now
        logger <| sprintf "Checking project: %A" (ended - started)
        let started = ended 

        let projDir = Path.GetDirectoryName path

        for err in checkFileResults.Errors do
            let pos =
                let fn = err.FileName
                if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                    let file = (fullpath projDir fn).Replace("/","\\")
//                    let m = mkRange m.FileName (mkPos m.StartLine (m.StartColumn + 1)) (mkPos m.EndLine (m.EndColumn + 1) )
                    sprintf "%s(%d,%d,%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                else ""
            let info =
                sprintf "%s %s FS%04d: " err.Subcategory 
                    (if err.Severity = Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning then "warning" else "error") err.ErrorNumber
                        
            eprintfn "%s%s%s" pos info (WebSharper.Compiler.ErrorPrinting.NormalizeErrorString err.Message)
            //printfn "%s" err.Message

        if checkFileResults.HasCriticalErrors then
            let comp = M.Compilation(M.empty)
            for err in checkFileResults.Errors do
                let pos =
                    let fn = err.FileName
                    if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                        let file = (fullpath projDir fn).Replace("/","\\")
                        sprintf "%s (%d,%d)-(%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                    else ""
                comp.AddError(None, M.SourceError (pos + err.Message))
            comp
        else

        let refMeta =   
            match prevMeta with
            | None -> M.empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.FSharp.Translator.transformAssembly refMeta
                (Path.GetFileNameWithoutExtension path)
                checkFileResults

        let ended = System.DateTime.Now
        logger <| sprintf "Parsing with FCS: %A" (ended - started)
        let started = ended 

        WebSharper.Compiler.ToJavaScript.ToJavaScript.CompileFull comp
        
        let winfo = "WebSharper warning: "
        for posOpt, err in comp.Warnings do
            let pos =
                match posOpt with
                | Some p ->
                    let file = (fullpath projDir p.FileName).Replace("/","\\")
                    sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
                | None -> ""
            eprintfn "%s%s%s" pos winfo (NormalizeErrorString (err.ToString()))

        let einfo = if warnOnly then "WebSharper warning: ERROR " else "WebSharper error: "
        for posOpt, err in comp.Errors do
            let pos =
                match posOpt with
                | Some p ->
                    let file = (fullpath projDir p.FileName).Replace("/","\\")
                    sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
                | None -> ""
            eprintfn "%s%s%s" pos einfo (NormalizeErrorString (err.ToString()))
            
        let ended = System.DateTime.Now
        logger <| sprintf "Transforming: %A" (ended - started)

        comp

