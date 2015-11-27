// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
//
/// The main entry-point module of WebSharper.
module WebSharper.FSharp.Program

open System
open System.IO
open System.Reflection
open WebSharper
open WebSharper.Compiler

type ProjectType =
    | Bundle
    | Website
    | Html
    | WIG

type WsConfig =
    {
        SourceMap   : bool
        TypeScript  : bool
        ProjectType : ProjectType option
        OutputDir  : string option
        AssemblyFile : string
        References  : string[] 
//        FscPath     : string
        FscArgs     : string[]        
        ProjectFile : string
        Documentation : string option
        VSStyleErrors : bool
        PrintJS : bool
    }

    static member Empty =
        {                 
             SourceMap   = false
             TypeScript  = false
             ProjectType = None
             OutputDir  = None
             AssemblyFile = null
             References  = [||]
//             FscPath     = null
             FscArgs     = [||]
             ProjectFile = null
             Documentation = None
             VSStyleErrors = false
             PrintJS  = false
        }
    
module ExecuteCommands =
    
    let GetWebRoot settings =
        match settings.OutputDir with
        | None ->
            let dir = Path.GetDirectoryName settings.ProjectFile
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None
        | Some out -> Some out

    let BundleOutputDir settings webRoot =
        match settings.OutputDir with
        | None ->
            match webRoot with
            | Some webRoot ->
                let d = Path.Combine(webRoot, "Content")
                let di = DirectoryInfo(d)
                if not di.Exists then
                    di.Create()
                d
            | None -> failwith "WebSharperBundleOutputDir property is required"
        | Some dir -> dir


    let SendResult result =
        match result with
        | Compiler.Commands.Ok -> true
        | Compiler.Commands.Errors errors ->
            for e in errors do
                eprintf "%s" e
            true

    let getWebRoot settings =
        match settings.OutputDir with
        | None ->
            let dir = Path.GetDirectoryName settings.ProjectFile
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None
        | Some out -> Some out

    let Bundle settings =
        let outputDir = BundleOutputDir settings (getWebRoot settings)
        let fileName = "Bundle"
//            match settings.Name with
//            | null | "" -> "Bundle"
//            | name -> name
        let cfg =
            {
                Compiler.BundleCommand.Config.Create() with
                    AssemblyPaths = List.ofArray settings.References
                    FileName = fileName
                    OutputDirectory = outputDir
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.BundleCommand.Instance.Execute(env, cfg)
        |> SendResult

    let Unpack settings =
        let webRoot = getWebRoot settings |> Option.get
        let assemblies =
            let dir =
                match settings.OutputDir with
                | None -> Path.Combine(webRoot, "bin")
                | Some p -> p
//                sprintf "Unpacking with WebSharper: %s -> %s" dir webRoot
            [
                yield! Directory.EnumerateFiles(dir, "*.dll")
                yield! Directory.EnumerateFiles(dir, "*.exe")
            ]
        for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
            let dir = DirectoryInfo(Path.Combine(webRoot, d))
            if not dir.Exists then
                dir.Create()
        let cfg =
            {
                Compiler.UnpackCommand.Config.Create() with
                    Assemblies = assemblies
                    RootDirectory = webRoot
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScript
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.UnpackCommand.Instance.Execute(env, cfg)
        |> SendResult

    let HtmlOutputDirectory settings =
        match settings.OutputDir with
        | None -> Path.Combine(Path.GetDirectoryName settings.ProjectFile, "bin", "html")
        | Some dir -> dir

    let Html settings =
        let main = settings.AssemblyFile
        let refs = List.ofArray settings.References
        let cfg =
            {
                Compiler.HtmlCommand.Config.Create(main) with
                    Mode = Compiler.HtmlCommand.Release
//                        match settings.Configuration with
//                        | x when x.ToLower().Contains("debug") -> Compiler.HtmlCommand.Debug
//                        | x when x.ToLower().Contains("release") -> Compiler.HtmlCommand.Release
//                        | _ -> Compiler.HtmlCommand.Debug
                    OutputDirectory = HtmlOutputDirectory settings
                    ProjectDirectory = Path.GetDirectoryName settings.ProjectFile
                    ReferenceAssemblyPaths = refs
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScript
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.HtmlCommand.Instance.Execute(env, cfg)
        |> SendResult

let LoadInterfaceGeneratorAssembly (aR: AssemblyResolver) (file: string) =
        let asm = Assembly.Load(File.ReadAllBytes(file))
        let name = AssemblyName.GetAssemblyName(file)
        match Attribute.GetCustomAttribute(asm, typeof<InterfaceGenerator.Pervasives.ExtensionAttribute>) with
        | :? InterfaceGenerator.Pervasives.ExtensionAttribute as attr ->
            name, attr.GetAssembly(), asm
        | _ ->
            failwith "No ExtensionAttribute set on the input assembly"

let logf x = 
    Printf.kprintf ignore x
//    Printf.kprintf (fun s -> File.AppendAllLines(@"C:\repo\websharper.csharp\wsfscruns.txt", [s])) x

let RunInterfaceGenerator aR snk config =
        let (name, asmDef, asm) = LoadInterfaceGeneratorAssembly aR config.AssemblyFile
        let cfg =
            {
                InterfaceGenerator.CompilerOptions.Default(name.Name) with
                    AssemblyResolver = Some aR
                    AssemblyVersion = name.Version
                    DocPath = None //input.DocumentationFile
                    EmbeddedResources = [] //input.EmbeddedResources
                    ProjectDir = Path.GetDirectoryName(config.ProjectFile) //input.ProjectDir
                    ReferencePaths = config.References //input.References
                    StrongNameKeyPair = snk
            }

        let cmp = InterfaceGenerator.Compiler.Create()
        let out = cmp.Compile(cfg, asmDef, asm)
        out.Save config.AssemblyFile
        let assem = Mono.Cecil.AssemblyDefinition.ReadAssembly config.AssemblyFile
        let meta =
            WebSharper.Compiler.Reflector.transformAssembly assem
//        let methodNames = comp.Classes.Values |> Seq.collect (fun c -> c.Methods.Keys |> Seq.map (fun m -> m.Value.MethodName)) |> Array.ofSeq
        WebSharper.Compiler.FrontEnd.modifyAssembly WebSharper.Core.Metadata.empty meta assem |> ignore
        assem.Write config.AssemblyFile

//let CompileFSharp config =
//    use proc =
//        new System.Diagnostics.Process(
//            StartInfo = 
//                System.Diagnostics.ProcessStartInfo(
//                    config.FscPath,
//                    config.FscArgs |> Seq.map (fun a -> "\"" + a + "\"") |> String.concat " ",
//                    CreateNoWindow = true,
//                    UseShellExecute = false,
//                    RedirectStandardOutput = true,
//                    RedirectStandardError = true
//                )
//        )
////    proc.OutputDataReceived.Add(fun e ->
////        printfn "%s" e.Data
////    )
////    proc.ErrorDataReceived.Add(fun e ->
////        eprintfn "%s" e.Data
////    )
//    Path.GetDirectoryName config.AssemblyFile |> Directory.CreateDirectory |> ignore
//    proc.Start() |> ignore
//    proc.WaitForExit()
//    proc.StandardOutput.ReadToEnd() |> printfn "%s"
//    let errors = proc.StandardError.ReadToEnd() 
//    if not (String.IsNullOrEmpty errors) then
//        logf "F# Errors:"
//        logf "%s" errors
//    eprintfn "%s" errors
//    if proc.ExitCode <> 0 then
//        Environment.Exit proc.ExitCode
////        failwith "F# compilation error"   

open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

let Compile config =
    let started = System.DateTime.Now
   
    let errors, exitCode = SimpleSourceCodeServices().Compile(config.FscArgs)

//    for error in errors do 
////        if config.VSStyleErrors then
////            eprintfn "%s" (error.ToString())
////        else
//            eprintfn "%s" (error.ToString())

    if exitCode <> 0 then 
        WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(ignore).PrintErrors(errors, config.ProjectFile)

        failwith "F# compilation failed"

//    CompileFSharp config
    
    let ended = System.DateTime.Now
    logf "F# compilation: %A" (ended - started)
    let started = ended 
    
    //compiler.CompileFSharp(config.FscArgs, config.AssemblyFile)
//    if config.VSStyleErrors then () else
    if config.ProjectFile = null then
        failwith "You must provide project file path."
    if config.AssemblyFile = null then
        failwith "You must provide assembly output path."
//    let objPath = Path.Combine(Path.GetDirectoryName config.ProjectFile, config.AssemblyFile)
//    File.AppendAllLines(@"C:\repo\websharper.csharp\builderrors.txt", [| "objPath: " + objPath |])
//    let origPath = Path.ChangeExtension(objPath, ".orig" + Path.GetExtension objPath)
//    File.Copy(objPath, origPath, true)
    let paths =
        [
            for r in config.References -> Path.GetFullPath r
            if File.Exists config.AssemblyFile then yield Path.GetFullPath config.AssemblyFile
        ]        
    let aR =
        AssemblyResolver.Create()
            .SearchPaths(paths)
    aR.Wrap <| fun () ->
//    let t2 = System.Type.GetType("WebSharper.Macro+LT, WebSharper.Main")
    if config.ProjectType = Some WIG then  
        RunInterfaceGenerator aR None config // snk

        let ended = System.DateTime.Now
        logf "WIG running time: %A" (ended - started)

    else    
    let loader = WebSharper.Compiler.FrontEnd.Loader.Create aR (logf "%s") //(fun msg -> out.Add(CompilerMessage.Warn msg))
    let refs = [ for r in config.References -> loader.LoadFile(r) ]
    let refMeta =
        let metas = refs |> List.choose (fun r -> WebSharper.Compiler.FrontEnd.readFromAssembly r)
        if List.isEmpty metas then None else Some (WebSharper.Core.Metadata.union metas)

    let ended = System.DateTime.Now
    logf "Loading referenced metadata: %A" (ended - started)
    let started = ended 

    let compiler = WebSharper.Compiler.FSharp.WebSharperFSharpCompiler(logf "%s")

    let ended = System.DateTime.Now
    logf "Initializing compiler: %A" (ended - started)
    let started = ended 
//
//    let classNames =
//        refMeta|> Option.map (fun m -> m.Classes |> Seq.map (fun c -> c.Key.Value.AssemblyQualifiedName) |> List.ofSeq)

    let referencedAsmNames =
        paths
//        |> Seq.append (Directory.GetFiles(BaseDir, "*.dll"))
        |> Seq.map (fun i -> 
            let n = Path.GetFileNameWithoutExtension(i)
            n, i
        )
        |> Map.ofSeq

    let thisName = Path.GetFileNameWithoutExtension config.AssemblyFile

    let assemblyResolveHandler = ResolveEventHandler(fun _ e ->
            let assemblyName = AssemblyName(e.Name).Name
            match Map.tryFind assemblyName referencedAsmNames with
            | None -> null
            | Some p -> 
                if assemblyName = "FSharp.Core" then
                    typeof<option<_>>.Assembly
                elif assemblyName = thisName then
                    Assembly.Load(File.ReadAllBytes(p))
                else
                    Assembly.LoadFrom(p)
        )

    System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)
    
    let comp =
        compiler.Compile(refMeta, config.FscArgs, config.ProjectFile)

//    System.AppDomain.CurrentDomain.remove_AssemblyResolve(assemblyResolveHandler)

    let ended = System.DateTime.Now
    logf "WebSharper compilation full: %A" (ended - started)
    let started = ended 

//    for pos, w in comp.Warnings do
//        match pos with
//        | Some pos ->
//            printfn "%s %d:%d %d:%d %O" pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) w
////            out.Add(CMWarn2 (pos.FileName, fst pos.Start, snd pos.Start, fst pos.End, snd pos.End, string w))
//        | _ ->
//            printfn "%O" w
////            out.Add(CMWarn1 (string w))

    if not (List.isEmpty comp.Errors) then
        File.AppendAllLines(@"C:\repo\websharper.csharp\builderrors.txt", 
            [| 
                for pos, e in comp.Errors ->
                    match pos with
                    | Some pos ->
                        sprintf "%s (%d,%d)-(%d,%d) WebSharper error %s" 
                            pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) (e.ToString())
                    | _ ->
                        sprintf "WebSharper error %s" (e.ToString())
            |]
        )
        
        for pos, e in comp.Errors do
            match pos with
            | Some pos ->

                logf "%s (%d,%d)-(%d,%d) WebSharper error %s" 
                    pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) (e.ToString())
                
                //fileName (int s.Line) (s.Column + 1) (int e.Line) (e.Column + 1) subcategory (if severity=FSharpErrorSeverity.Warning then "warning" else "error")  message

//                eprintfn "%s %d:%d %d:%d %O" pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) e
//                out.Add(CMErr2 (pos.FileName, fst pos.Start, snd pos.Start, fst pos.End, snd pos.End, string e))
            | _ ->
                logf "WebSharper error %s" (e.ToString())
//                out.Add(CMErr1 (string e))
        failwith "WebSharper errors"
    else

    let thisMeta = comp.ToCurrentMetadata()
    let merged = 
        WebSharper.Core.Metadata.union 
            [
                (match refMeta with Some m -> m | _ -> WebSharper.Core.Metadata.empty)
                thisMeta
            ]

    let rp = 
        let mcAr = Mono.Cecil.DefaultAssemblyResolver()
        config.References 
        |> Seq.map System.IO.Path.GetDirectoryName
        |> Seq.distinct
        |> Seq.iter mcAr.AddSearchDirectory
        Mono.Cecil.ReaderParameters(AssemblyResolver = mcAr)

    let assem = Mono.Cecil.AssemblyDefinition.ReadAssembly(config.AssemblyFile, rp)
    let js = WebSharper.Compiler.FrontEnd.modifyAssembly merged thisMeta assem
            
    if config.PrintJS then
        match js with 
        | Some js ->
            printfn "%s" js
            logf "%s" js
        | _ -> ()

//    let rec tryWrite attempt =
//        if attempt = 10 then
//            assem.Write config.AssemblyFile
//        else
//            try assem.Write config.AssemblyFile
//            with _ ->
//                System.Threading.Thread.Sleep 200
//                tryWrite (attempt + 1)
//
//    tryWrite 0

    assem.Write(config.AssemblyFile, Mono.Cecil.WriterParameters())

    let ended = System.DateTime.Now
    logf "Serializing and writing metadata: %A" (ended - started)

    match config.ProjectType with
    | Some Bundle ->
        ExecuteCommands.Bundle config |> ignore
    | Some Website ->
        ExecuteCommands.Unpack config |> ignore
    | Some Html ->
        ExecuteCommands.Html config |> ignore
    | _ -> ()

//    printfn "WebSharper metadata written"

let (|StartsWith|_|) start (input: string) =    
    if input.StartsWith start then
        Some input.[start.Length ..]
    else None 

let (|Cmd|_|) (cmd: Commands.ICommand) argv =
    match cmd.Parse argv with
    | Commands.NotRecognized -> None
    | Commands.Parsed f ->
        match f (Commands.Environment.Create()) with
        | Commands.Ok -> Some 0
        | Commands.Errors errors ->
            for e in errors do
                stderr.WriteLine(e)
            Some 1
    | Commands.ParseFailed err ->
        for e in err do
            stderr.WriteLine(e)
        stderr.WriteLine()
        stderr.WriteLine(cmd.Usage)
        Some 1

//[<EntryPoint>]
let main argv =

    logf "%s" Environment.CommandLine            
    logf "Started at: %A" System.DateTime.Now

    match List.ofArray argv with
    | Cmd BundleCommand.Instance r -> r
    | Cmd HtmlCommand.Instance r -> r
    | Cmd UnpackCommand.Instance r -> r
    | _ ->

    let wsArgs = ref WsConfig.Empty
    let refs = ResizeArray()
    let fscArgs = ResizeArray()

    let cArgv =
        [|
            let isRNext = ref false
            for a in argv do
                match a with
                | "-r" ->
                    isRNext := true
                | _ ->
                    if !isRNext then
                        isRNext := false   
                        yield "-r:" + a
                    else
                        yield a
        |]

    for a in cArgv do
        let setProjectType t =
            match (!wsArgs).ProjectType with
            | None -> wsArgs := { !wsArgs with ProjectType = Some t }
            | _ -> failwith "Conflicting WebSharper project types set."
        match a with
        | "--jsmap" -> wsArgs := { !wsArgs with SourceMap = true } 
        | "--dts" -> wsArgs := { !wsArgs with TypeScript = true } 
        | "--wig" -> setProjectType WIG
        | "--bundle" -> setProjectType Bundle
        | "--html" -> setProjectType Html
        | "--site" -> setProjectType Website
        | StartsWith "--ws:" wsProjectType ->
            match wsProjectType.ToLower() with
            | "ignore" -> ()
            | "bundle" -> setProjectType Bundle //(GetWebRoot settings)
            | "extension" | "interfacegenerator" -> setProjectType WIG
            | "html" -> setProjectType Html
            | "library" -> ()
            | "site" | "web" | "website" | "export" -> setProjectType Website
//                match GetWebRoot settings with
//                | None -> Library
//                | Some dir -> Website dir
            | _ -> invalidArg "type" ("Invalid project type: " + wsProjectType)
        | "--printjs" -> wsArgs := { !wsArgs with PrintJS = true }
        | "--vserrors" ->
            wsArgs := { !wsArgs with VSStyleErrors = true }
            fscArgs.Add a
        | StartsWith "--wsoutput:" o ->
            wsArgs := { !wsArgs with OutputDir = Some o }
//        | StartsWith "--fsc:" p ->
//            wsArgs := { !wsArgs with FscPath = p }
        | StartsWith "--project:" p ->
            wsArgs := { !wsArgs with ProjectFile = p }
        | StartsWith "--doc:" d ->
            wsArgs := { !wsArgs with Documentation = Some d }
            fscArgs.Add a
        | StartsWith "-o:" o | StartsWith "--out:" o ->
            wsArgs := { !wsArgs with AssemblyFile = o }
            fscArgs.Add a
        | StartsWith "-r:" r | StartsWith "--reference:" r ->
            refs.Add r
            fscArgs.Add a
        | _ -> 
            fscArgs.Add a  
    wsArgs := { !wsArgs with References = refs.ToArray(); FscArgs = fscArgs.ToArray() }

#if DEBUG
    Compile !wsArgs
    0 
#else
    try
        Compile !wsArgs
        logf "Stopped at: %A" System.DateTime.Now
        0       
    with e ->
//        File.AppendAllLines(@"C:\repo\websharper.csharp\builderrors.txt", [| argv |> String.concat " "; e.Message |])
        (!wsArgs).Documentation |> Option.iter (fun d -> if File.Exists d then File.Delete d)
        let intermediaryOutput = (!wsArgs).AssemblyFile
        if File.Exists intermediaryOutput then 
            File.Move (intermediaryOutput, intermediaryOutput + ".failed")

//        eprintfn "Failed at: %A" System.DateTime.Now
//        eprintfn "Error: %A" e
        reraise()
#endif
