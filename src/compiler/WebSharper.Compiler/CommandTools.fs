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

module WebSharper.Compile.CommandTools

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
        IsDebug : bool
        ProjectType : ProjectType option
        OutputDir  : string option
        AssemblyFile : string
        References  : string[] 
        Resources : string[]
        KeyFile : string option
        CompilerArgs : string[]        
        ProjectFile : string
        Documentation : string option
        VSStyleErrors : bool
        PrintJS : bool
        WarnOnly : bool
    }

    member this.ProjectDir =
        match Path.GetDirectoryName this.ProjectFile with
        | "" -> Directory.GetCurrentDirectory()
        | p -> p

    static member Empty =
        {                 
             SourceMap   = false
             TypeScript  = false
             IsDebug = false
             ProjectType = None
             OutputDir  = None
             AssemblyFile = null
             References  = [||]
             Resources = [||]
             KeyFile = None
             CompilerArgs = [||]
             ProjectFile = null
             Documentation = None
             VSStyleErrors = false
             PrintJS  = false
             WarnOnly = false
        }

let readStrongNameKeyPair p = StrongNameKeyPair(File.ReadAllBytes(p))
    
module ExecuteCommands =
    
    let TryGetOutputDir settings = 
        settings.OutputDir |> Option.map (fun o ->
            Path.Combine(Path.GetDirectoryName settings.ProjectFile, o)
        )

    let GetWebRoot settings =
        match TryGetOutputDir settings with
        | None ->
            let dir = settings.ProjectDir
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None
        | Some out -> Some out

    let BundleOutputDir settings webRoot =
        match TryGetOutputDir settings with
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
        match TryGetOutputDir settings with
        | None ->
            let dir = settings.ProjectDir
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None
        | Some out -> Some out

    let Bundle settings =
        let outputDir = BundleOutputDir settings (getWebRoot settings)
        let fileName = Path.GetFileNameWithoutExtension settings.AssemblyFile
        let cfg =
            {
                Compiler.BundleCommand.Config.Create() with
                    AssemblyPaths = settings.AssemblyFile :: List.ofArray settings.References
                    FileName = fileName
                    OutputDirectory = outputDir
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.BundleCommand.Instance.Execute(env, cfg)
        |> SendResult

    let Unpack settings =
        let webRoot = getWebRoot settings |> Option.get
        printfn "unpacking into %s" webRoot
        for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
            let dir = DirectoryInfo(Path.Combine(webRoot, d))
            if not dir.Exists then
                dir.Create()
        let assemblies =
            let dir =
                match settings.OutputDir with
                | None | Some "" -> Path.Combine(webRoot, "bin")
                | Some p -> p
            [
                yield! Directory.EnumerateFiles(dir, "*.dll")
                yield! Directory.EnumerateFiles(dir, "*.exe")
                yield settings.AssemblyFile
                yield! settings.References
            ]        
            |> List.distinct
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
        match TryGetOutputDir settings with
        | None -> Path.Combine(settings.ProjectDir, "bin", "html")
        | Some dir -> dir

    let Html settings =
        let main = settings.AssemblyFile
        let refs = List.ofArray settings.References
        let cfg =
            {
                Compiler.HtmlCommand.Config.Create(main) with
                    Mode = if settings.IsDebug then Compiler.HtmlCommand.Debug else Compiler.HtmlCommand.Release
                    OutputDirectory = HtmlOutputDirectory settings
                    ProjectDirectory = settings.ProjectDir
                    ReferenceAssemblyPaths = refs
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScript
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.HtmlCommand.Instance.Execute(env, cfg)
        |> SendResult

let LoadInterfaceGeneratorAssembly (aR: AssemblyResolver) (file: string) =
        let genFile = Path.ChangeExtension(file, ".Generator.dll")
        if File.Exists genFile then File.Delete genFile
        File.Copy(file, genFile)
        let asm = Assembly.Load(File.ReadAllBytes(genFile))
        let name = asm.GetName()
        match Attribute.GetCustomAttribute(asm, typeof<InterfaceGenerator.Pervasives.ExtensionAttribute>) with
        | :? InterfaceGenerator.Pervasives.ExtensionAttribute as attr ->
            name, attr.GetAssembly(), asm
        | _ ->
            failwith "No ExtensionAttribute set on the input assembly"

let RunInterfaceGenerator aR snk config =
        let (name, asmDef, asm) = LoadInterfaceGeneratorAssembly aR config.AssemblyFile
        let cfg =
            {
                InterfaceGenerator.CompilerOptions.Default(name.Name) with
                    AssemblyResolver = Some aR
                    AssemblyVersion = name.Version
                    DocPath = config.Documentation
                    EmbeddedResources = config.Resources
                    ProjectDir = config.ProjectDir
                    ReferencePaths = config.References
                    StrongNameKeyPair = snk
            }

        let cmp = InterfaceGenerator.Compiler.Create()
        let out = cmp.Compile(cfg, asmDef, asm)
        out.Save config.AssemblyFile

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
