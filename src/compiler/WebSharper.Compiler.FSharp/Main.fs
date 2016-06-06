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

open System.IO

module M = WebSharper.Core.Metadata

type internal FSIFD = FSharpImplementationFileDeclaration

/// Creates WebSharper compilation for an F# project
type WebSharperFSharpCompiler(logger) =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

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
                    sprintf "%s(%d,%d,%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                else ""
            let info =
                sprintf "%s %s FS%04d: " err.Subcategory 
                    (if err.Severity = Microsoft.FSharp.Compiler.FSharpErrorSeverity.Warning then "warning" else "error") err.ErrorNumber
                        
            eprintfn "%s%s%s" pos info (WebSharper.Compiler.ErrorPrinting.NormalizeErrorString err.Message)

        if checkFileResults.HasCriticalErrors then
            let comp = WebSharper.Compiler.Compilation(M.Info.Empty)
            for err in checkFileResults.Errors do
                let pos =
                    let fn = err.FileName
                    if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                        let file = (fullpath projDir fn).Replace("/","\\")
                        sprintf "%s (%d,%d)-(%d,%d): " file err.StartLineAlternate err.StartColumn err.EndLineAlternate err.EndColumn
                    else ""
                comp.AddError(None, WebSharper.Compiler.SourceError (pos + err.Message))
            comp
        else

        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta))
                (Path.GetFileNameWithoutExtension path)
                checkFileResults

        let ended = System.DateTime.Now
        logger <| sprintf "Parsing with FCS: %A" (ended - started)
        let started = ended 

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
        
        comp.VerifyRPCs()

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

    member this.Compile (prevMeta, assemblyName, checkFileResults: FSharpCheckProjectResults) =
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta, UseMacros = false))
                assemblyName
                checkFileResults

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp
