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

namespace WebSharper.Compiler.FSharp

open Microsoft.FSharp.Compiler.SourceCodeServices
open WebSharper.Compiler.ErrorPrinting
open WebSharper.Compiler.FrontEnd

open System.IO

module M = WebSharper.Core.Metadata

type internal FSIFD = FSharpImplementationFileDeclaration

/// Creates WebSharper compilation for an F# project
type WebSharperFSharpCompiler(logger, ?checker) =
    let checker =
        match checker with
        | Some c -> c
        | _ -> FSharpChecker.Create(keepAssemblyContents = true)

    let fullpath cwd nm = 
        let p = if Path.IsPathRooted(nm) then nm else Path.Combine(cwd,nm)
        try Path.GetFullPath(p) with 
        | :? System.ArgumentException 
        | :? System.ArgumentNullException 
        | :? System.NotSupportedException 
        | :? System.IO.PathTooLongException 
        | :? System.Security.SecurityException -> p

    member val PrintEnabled = true with get, set
    member val UseGraphs = true with get, set
    member val UseVerifier = true with get, set

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
                               
    member this.PrintWarnings(comp: WebSharper.Compiler.Compilation, path: string) =
        let projDir = Path.GetDirectoryName path
        let winfo = "WebSharper warning: "
        for posOpt, err in comp.Warnings do
            let pos =
                match posOpt with
                | Some p ->
                    let file = (fullpath projDir p.FileName).Replace("/","\\")
                    sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
                | None -> ""
            eprintfn "%s%s%s" pos winfo (NormalizeErrorString (err.ToString()))

    member this.Compile (prevMeta : System.Threading.Tasks.Task<option<M.Info>>, argv, path: string, warnOnly) = 

        let projectOptions =
            try
                checker.GetProjectOptionsFromCommandLineArgs(path, argv)
            with e ->
                failwithf "Error reading project options: %s" path

        let checkProjectResults = 
            projectOptions
            |> checker.ParseAndCheckProject 
            |> Async.RunSynchronously

        TimedStage "Checking project"

        prevMeta.Wait()

        let refMeta = 
            match prevMeta.Result with
            | Some r -> r
            | _ -> failwith "Error reading referenced metadata"

        TimedStage "Waiting on merged metadata"

        let projDir = Path.GetDirectoryName path

        if this.PrintEnabled then
            for err in checkProjectResults.Errors do
                let pos =
                    let fn = err.FileName
                    if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                        let file = (fullpath projDir fn).Replace("/","\\")
                        sprintf "%s(%d,%d,%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                    else ""
                let info =
                    sprintf "%s %s FS%04d: " err.Subcategory 
                        (if err.Severity = Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning then "warning" else "error") err.ErrorNumber
                        
                eprintfn "%s%s%s" pos info (WebSharper.Compiler.ErrorPrinting.NormalizeErrorString err.Message)

        if checkProjectResults.HasCriticalErrors then
            let comp = WebSharper.Compiler.Compilation(M.Info.Empty)
            if this.PrintEnabled then
                for err in checkProjectResults.Errors do
                    let pos =
                        let fn = err.FileName
                        if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                            let file = (fullpath projDir fn).Replace("/","\\")
                            sprintf "%s (%d,%d)-(%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                        else ""
                    comp.AddError(None, WebSharper.Compiler.SourceError (pos + err.Message))
            comp
        else
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta, this.UseGraphs))
                (Path.GetFileNameWithoutExtension path)
                checkProjectResults

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
        
        comp.VerifyRPCs()

        if this.PrintEnabled then

            let einfo = if warnOnly then "WebSharper warning: ERROR " else "WebSharper error: "
            for posOpt, err in comp.Errors do
                let pos =
                    match posOpt with
                    | Some p ->
                        let file = (fullpath projDir p.FileName).Replace("/","\\")
                        sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
                    | None -> ""
                eprintfn "%s%s%s" pos einfo (NormalizeErrorString (err.ToString()))
            
        TimedStage "WebSharper translation"

        comp

    static member Compile (prevMeta, assemblyName, checkProjectResults: FSharpCheckProjectResults, ?useGraphs) =
        let useGraphs = defaultArg useGraphs true
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta, useGraphs, UseLocalMacros = false))
                assemblyName
                checkProjectResults

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp
