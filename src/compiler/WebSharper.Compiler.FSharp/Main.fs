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

    member val UseGraphs = true with get, set
    member val UseVerifier = true with get, set

    member this.Compile (prevMeta : System.Threading.Tasks.Task<option<M.Info>>, argv, path, assemblyName) = 

        let projectOptionsOpt =
            try
                checker.GetProjectOptionsFromCommandLineArgs(path, argv) |> Some
            with e ->
                None

        match projectOptionsOpt with
        | None -> None
        | Some projectOptions ->

        let checkProjectResults = 
            projectOptions
            |> checker.ParseAndCheckProject 
            |> Async.RunSynchronously

        TimedStage "Checking project"

        prevMeta.Wait()

        match prevMeta.Result with
        | None -> None
        | Some refMeta ->

        TimedStage "Waiting on merged metadata"

        if checkProjectResults.HasCriticalErrors then
            None
        else
        
        let comp = 
            WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
                (WebSharper.Compiler.Compilation(refMeta, this.UseGraphs))
                assemblyName
                checkProjectResults

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
        
        comp.VerifyRPCs()
            
        TimedStage "WebSharper translation"

        Some comp

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
