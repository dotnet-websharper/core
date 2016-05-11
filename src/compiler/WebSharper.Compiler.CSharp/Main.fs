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

namespace WebSharper.Compiler.CSharp

open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp  
open WebSharper.Compiler.ErrorPrinting

module M = WebSharper.Core.Metadata
        
/// Creates WebSharper compilation for a C# project
type WebSharperCSharpCompiler(logger) =

    let fullpath cwd nm = 
        let p = if Path.IsPathRooted(nm) then nm else Path.Combine(cwd,nm)
        try Path.GetFullPath(p) with 
        | :? System.ArgumentException 
        | :? System.ArgumentNullException 
        | :? System.NotSupportedException 
        | :? System.IO.PathTooLongException 
        | :? System.Security.SecurityException -> p

    member this.Compile (prevMeta, argv, path: string, warnOnly) =

        let started = System.DateTime.Now

        let parsedArgs =
            CSharpCommandLineParser.Default.Parse(
                argv, 
                System.IO.Path.GetDirectoryName path,
                null
            )

        let syntaxTrees =
            parsedArgs.SourceFiles 
            |> Seq.map (fun s -> 
                CSharpSyntaxTree.ParseText(File.ReadAllText s.Path, path = s.Path)
            )

        let references = 
            argv
            |> Seq.choose (fun a ->
                if a.StartsWith "/reference:" then Some a.[11 ..] else None
            ) |> Seq.map (fun r ->
                MetadataReference.CreateFromFile(r, MetadataReferenceProperties.Assembly) :> MetadataReference
            )

        let compilation = 
            CSharpCompilation.Create(
                System.IO.Path.GetFileNameWithoutExtension path,
                syntaxTrees,
                references,
                parsedArgs.CompilationOptions
            )
        
        let ended = System.DateTime.Now
        logger <| sprintf "Creating compilation: %A" (ended - started)
        let started = ended 
    
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly refMeta
                compilation

        let ended = System.DateTime.Now
        logger <| sprintf "Parsing with Roslyn: %A" (ended - started)
        let started = ended 

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
        comp.VerifyRPCs()

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
