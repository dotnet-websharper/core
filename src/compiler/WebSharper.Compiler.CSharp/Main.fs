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

    member this.Compile (prevMeta, argv: seq<string>, path: string, warnOnly) =

//        let argv =
//            """/noconfig /nowarn:1701,1702 /nostdlib+ /warn:3 /doc:C:\repo\websharper.csharp\msbuild\\..\build\Release\WebSharper.CSharp.Tests.xml /define:TRACE /highentropyva+ /reference:C:\repo\websharper.csharp\build\Release\FSharp.Core.dll /reference:'C:\Program_Files_(x86)\Reference_Assemblies\Microsoft\Framework\.NETFramework\v4.5\Microsoft.CSharp.dll' /reference:'C:\Program_Files_(x86)\Reference_Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll' /reference:'C:\Program_Files_(x86)\Reference_Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll' /reference:'C:\Program_Files_(x86)\Reference_Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll' /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Collections.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Collections.Tests.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Control.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Core.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Core.JavaScript.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.InterfaceGenerator.Tests.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.JavaScript.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Main.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Testing.dll /reference:C:\repo\websharper.csharp\build\Release\WebSharper.Web.dll /debug:pdbonly /filealign:512 /optimize+ /out:obj\Release\WebSharper.CSharp.Tests.dll /ruleset:'C:\Program_Files_(x86)\Microsoft_Visual_Studio_14.0\Team_Tools\Static_Analysis_Tools\\Rule_Sets\MinimumRecommendedRules.ruleset' /subsystemversion:6.00 /target:library /utf8output Arithmetic.cs Delegate.cs Macro.cs Object.cs Remoting.cs Interop.cs Linq.cs Properties\AssemblyInfo.cs String.cs Syntax.cs Tests.cs 'C:\Users\András\AppData\Local\Temp\.NETFramework,Version=v4.5.AssemblyAttributes.cs'"""
//                .Split([|' '|]) |> Array.map (fun s -> s.Replace('_', ' ').Replace(''', '"'))

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
        
        let refs = ResizeArray()

        let references = 
            argv
            |> Seq.choose (fun a ->
                //if a.StartsWith "/reference:\"" then Some a.[12 .. a.Length - 2] 
                if a.StartsWith "/reference:" then Some a.[11 ..] else None
            ) 
            |> Seq.map (fun r ->
                refs.Add r
                MetadataReference.CreateFromFile(r) :> MetadataReference
            )

        let compilation = 
            CSharpCompilation.Create(
                System.IO.Path.GetFileNameWithoutExtension path,
                syntaxTrees,
                references,
                parsedArgs.CompilationOptions
            )
        
        let firstError =
            compilation.GetDiagnostics() |> Seq.tryFind (fun d -> d.Severity = DiagnosticSeverity.Error)

        match firstError with
        | Some err ->
            failwithf "C# compilation resulted in errors: %s" (err.GetMessage())
        | _ -> ()

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

        for r in refs do
            comp.AddWarning(None, WebSharper.Compiler.SourceWarning("added reference: " + r))

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

    member this.Compile (prevMeta, compilation: CSharpCompilation) =
        let refMeta =   
            match prevMeta with
            | None -> M.Info.Empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.CSharp.ProjectReader.transformAssembly refMeta
                compilation

        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
            
//        comp.VerifyRPCs()

//        let projDir = Path.GetDirectoryName path

//        let winfo = "WebSharper warning: "
//        for posOpt, err in comp.Warnings do
//            let pos =
//                match posOpt with
//                | Some p ->
//                    let file = (fullpath projDir p.FileName).Replace("/","\\")
//                    sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
//                | None -> ""
//            eprintfn "%s%s%s" pos winfo (NormalizeErrorString (err.ToString()))
//
//        let einfo = if warnOnly then "WebSharper warning: ERROR " else "WebSharper error: "
//        for posOpt, err in comp.Errors do
//            let pos =
//                match posOpt with
//                | Some p ->
//                    let file = (fullpath projDir p.FileName).Replace("/","\\")
//                    sprintf "%s(%d,%d,%d,%d): " file (fst p.Start) (snd p.Start) (fst p.End) (snd p.End)   
//                | None -> ""
//            eprintfn "%s%s%s" pos einfo (NormalizeErrorString (err.ToString()))

        comp
