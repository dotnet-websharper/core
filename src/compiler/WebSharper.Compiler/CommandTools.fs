// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Compiler.CommandTools

open System
open System.IO
open System.Reflection
open WebSharper
open WebSharper.Compiler
open WebSharper.Core

exception ArgumentError of msg: string with
    override this.Message = this.msg
let argError msg = raise (ArgumentError msg)

type ProjectType =
    | Bundle
    | BundleOnly
    | Website
    | Html
    | WIG
    | Proxy
    | Service

    static member Parse(wsProjectType: string) =
        match wsProjectType.ToLower() with
        | "" | "ignore" | "library" -> None
        | "web" | "site" | "website" | "export" -> Some Website
        | "spa" | "bundle" -> Some Bundle
        | "html" | "static" -> Some Html
        | "binding" | "extension" | "wig" | "interfacegenerator" -> Some WIG
        | "proxy" -> Some Proxy
        | "bundleonly" -> Some BundleOnly
        | "microservice" -> Some Service
        | _ -> argError ("Invalid project type: " + wsProjectType)

    static member GetWarning(wsProjectType: string, parsed: ProjectType) =
        let defValue =
            match parsed with
            | Website -> "web"
            | Bundle -> "spa"
            | Html -> "html"
            | WIG -> "binding"
            | Proxy -> "proxy"
            | BundleOnly -> "bundleonly"
            | Service -> "microservice"
        if wsProjectType.ToLower() = defValue then
            None
        else
            Some (sprintf """Please use "%s" instead of "%s" for your project type""" defValue wsProjectType)

type JavaScriptScope =
    | JSDefault
    | JSAssembly
    | JSFilesOrTypes of string[]
 
type WsConfig =
    {
        SourceMap : bool
        TypeScriptOutput : bool
        TypeScriptDeclaration : bool
        IsDebug : bool
        ProjectType : ProjectType option
        OutputDir : string option
        ScriptBaseUrl : string option
        AssemblyFile : string
        References : string[] 
        Resources : (string * string option)[]
        KeyFile : string option
        CompilerArgs : string[]  
        Configuration : string
        ProjectFile : string
        Documentation : string option
        PrintJS : bool
        WarnOnly : bool
        DeadCodeElimination : bool option
        DownloadResources : bool option
        AnalyzeClosures : bool option
        JavaScriptScope : JavaScriptScope
        JavaScriptExport : JsExport[]
        StubInterfaces : bool
        JSOutputPath : string option
        MinJSOutputPath : string option
        SingleNoJSErrors : bool
        ProxyTargetName : string option
        UseJavaScriptSymbol : bool option
        TargetProfile : string
        Standalone : bool
        BuildServiceLogging : bool
        RuntimeMetadata : Metadata.MetadataOptions
        ArgWarnings : string list
        PreBundle : bool
    }

    member this.ProjectDir =
        match Path.GetDirectoryName this.ProjectFile with
        | "" -> Directory.GetCurrentDirectory()
        | p -> p

    static member Empty =
        {                 
            SourceMap = false
            TypeScriptOutput = false
            TypeScriptDeclaration = true
            IsDebug = false
            ProjectType = None
            OutputDir = None
            ScriptBaseUrl = None
            AssemblyFile = null
            References = [||]
            Resources = [||]
            KeyFile = None
            CompilerArgs = [||]
            Configuration = null
            ProjectFile = null
            Documentation = None
            PrintJS = false
            WarnOnly = false
            DeadCodeElimination = None
            DownloadResources = None       
            AnalyzeClosures = None
            JavaScriptScope = JSDefault
            JavaScriptExport = [||]
            StubInterfaces = false
            JSOutputPath = None
            MinJSOutputPath = None
            SingleNoJSErrors = false
            ProxyTargetName = None
            UseJavaScriptSymbol = None
            TargetProfile = "mscorlib"
            Standalone = 
                let envVar = System.Environment.GetEnvironmentVariable("WebSharperBuildService")
                if isNull envVar then
                    false
                else
                    match bool.TryParse(envVar) with
                    | true, v -> not v
                    | _ -> false
            BuildServiceLogging =
                let envVar = System.Environment.GetEnvironmentVariable("WebSharperBuildServiceLogging")
                if isNull envVar then
                    true
                else
                    match bool.TryParse(envVar) with
                    | true, v -> v
                    | _ -> true                
            RuntimeMetadata = Metadata.MetadataOptions.DiscardExpressions
            ArgWarnings = []
            PreBundle = false
        }

    static member ParseAnalyzeClosures(c: string) =
        match c.ToLower() with
        | "true" -> Some false
        | "movetotop" -> Some true
        | _ -> argError "Invalid value for AnalyzeClosures, value must be true or movetotop."
    
    member this.AddJson(jsonString, fileName, configuration) =
        let json =
            try Json.Parse jsonString 
            with _ -> argError (sprintf "Failed to parse %s, not a valid json." fileName)
        let settings = 
            match json with
            | Json.Object values -> 
                let defValues = values |> List.filter (fun (k, v) -> v <> Json.Null)
                let configOverrides = 
                    defValues |> List.tryPick (
                        function
                        | v, Json.Object o when 
                            System.String.Equals(v, configuration, StringComparison.InvariantCultureIgnoreCase) 
                            || System.String.Equals(v, "config-" + configuration, StringComparison.InvariantCultureIgnoreCase) ->
                            Some o
                        | _ -> None
                    )
                    |> Option.defaultValue []
                defValues @ configOverrides
            | _ -> argError (sprintf "Failed to parse %s, not a json object." fileName)
        let getString k v =
            match v with
            | Json.String s -> s
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting a string." fileName k)
        let projectDir = Path.GetDirectoryName this.ProjectFile
        let getPath k v =
            Path.Combine(projectDir, getString k v)
        let getBool k v = 
            match v with
            | Json.True -> true
            | Json.False -> false
            | Json.String s ->
                match bool.TryParse s with
                | true, b -> b
                | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false." fileName k)
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false." fileName k)
        let getDlRes k v = 
            match v with
            | Json.True -> Some true
            | Json.False -> None
            | Json.String s ->
                match bool.TryParse s with
                | true, b -> if b then Some true else None
                | _ ->
                    match s.ToLower() with
                    | "warnonly" -> Some false
                    | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false or 'warnonly'." fileName k)   
            | _ -> argError (sprintf "Invalid value in %s for %s, expecting true or false or 'warnonly'." fileName k)
        let mutable res = this
        for k, v in settings do
            match k.ToLower() with
            | "project" ->
                let s = getString k v
                let pt = ProjectType.Parse s
                let w = pt |> Option.bind (fun pt -> ProjectType.GetWarning(s, pt))
                match w with
                | Some w ->
                    res <- { res with ProjectType = pt; ArgWarnings = w :: res.ArgWarnings }
                | None ->
                    res <- { res with ProjectType = pt }
            | "outputdir" ->
                res <- { res with OutputDir = Some (getPath k v) }
            | "scriptbaseurl" ->
                res <- { res with ScriptBaseUrl = Some (getString k v) }
            | "dce" ->
                res <- { res with DeadCodeElimination = Some (getBool k v) }
            | "sourcemap" ->
                res <- { res with SourceMap = getBool k v }
            | "dts" ->
                res <- { res with TypeScriptDeclaration = getBool k v }
            | "ts" ->
                res <- { res with TypeScriptOutput = getBool k v }
            | "warnonly" ->
                res <- { res with WarnOnly = getBool k v }
            | "downloadresources" ->
                res <- { res with DownloadResources = getDlRes k v }
            | "analyzeclosures" ->
                let a =
                    match v with
                    | Json.True -> Some false
                    | Json.False -> None
                    | Json.String s -> WsConfig.ParseAnalyzeClosures s
                    | _ -> argError (sprintf "Invalid value in %s for AnalyzeClosures, value must be true, false or \"movetotop\"." fileName)    
                res <- { res with AnalyzeClosures = a }
            | "javascript" ->
                let j =
                    match v with
                    | Json.True -> JSAssembly
                    | Json.False -> JSDefault
                    | Json.Array a ->
                        a |> Seq.map (
                            function
                            | Json.String s -> s
                            | _ -> argError (sprintf "Invalid value in %s for JavaScript, expecting true or false or an array of strings." fileName)
                        ) |> Array.ofSeq |> JSFilesOrTypes
                    | _ -> argError (sprintf "Invalid value in %s for JavaScript, expecting true or false or an array of strings." fileName) 
                res <- { res with JavaScriptScope = j }
            | "javascriptexport" ->
                let j =
                    match v with
                    | Json.True -> [| ExportCurrentAssembly |]
                    | Json.False -> [||]
                    | Json.Array a ->
                        a |> Seq.map (
                            function
                            | Json.True -> ExportCurrentAssembly
                            | Json.String s -> ExportByName s
                            | _ -> argError (sprintf "Invalid value in %s for JavaScriptExport, expecting true or false or an array of strings." fileName)
                        ) |> Array.ofSeq
                    | _ -> argError (sprintf "Invalid value in %s for JavaScriptExport, expecting true or false or an array of strings." fileName) 
                res <- { res with JavaScriptExport = Array.append this.JavaScriptExport j }
            | "stubinterfaces" ->
                res <- { res with StubInterfaces = getBool k v }
            | "jsoutput" ->
                let path = getPath k v
                //if path.EndsWith(".js") then
                //    argError (sprintf "Invalid value in %s for JsOutput, expecting a folder path." fileName)     
                res <- { res with JSOutputPath = Some path }
            | "minjsoutput" ->
                res <- { res with MinJSOutputPath = Some (getPath k v) }
            | "singlenojserrors" ->
                res <- { res with SingleNoJSErrors = getBool k v }
            | "proxytargetname" ->
                res <- { res with ProxyTargetName = Some (getString k v) }
            | "usejavascriptsymbol" ->
                res <- { res with UseJavaScriptSymbol = Some (getBool k v) }
            | "standalone" ->
                res <- { res with Standalone = getBool k v }
            | "buildservice" ->
                res <- { res with Standalone = not (getBool k v) }
            | "buildservicelogging" ->
                res <- { res with BuildServiceLogging = not (getBool k v) }
            | "runtimemetadata" ->
                let runtimeMetadata =
                    match (getString k v).ToLower() with
                    | "inlines" -> Metadata.MetadataOptions.DiscardNotInlineExpressions 
                    | "notinlines" -> Metadata.MetadataOptions.DiscardInlineExpressions 
                    | "full" -> Metadata.MetadataOptions.FullMetadata 
                    | "noexpressions" -> Metadata.MetadataOptions.DiscardExpressions
                    | _ -> argError (sprintf "Invalid value in %s for RuntimeMetadata, expecting 'noexpressions'/'inlines'/'notinlines'/'full'." fileName) 
                res <- { res with RuntimeMetadata = runtimeMetadata }
            | "prebundle" ->
                res <- { res with PreBundle = res.PreBundle || getBool k v }
            | "$schema" | "debug" | "release" -> ()
            | c when c.StartsWith("config-") -> ()
            | _ -> failwithf "Unrecognized setting in %s: %s" fileName k 
        if res.ProjectType <> Some ProjectType.Bundle && res.ProjectType <> Some ProjectType.BundleOnly then
            if res.JSOutputPath |> Option.map (fun x -> x.EndsWith(".js")) |> Option.defaultValue false then
                argError (sprintf "Invalid value in %s for JsOutput, expecting a folder path." fileName)     
        res
    
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
                Some d
            | None -> None
        | Some out -> Some out

    let Unpack webRoot settings loader (logger: LoggerBase) =
        sprintf "Unpacking into %s" webRoot
        |> logger.Out
        for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
            let dir = DirectoryInfo(Path.Combine(webRoot, d))
            if not dir.Exists then
                dir.Create()
        let assemblies, currentAssembly =
            let dir =
                match settings.OutputDir with
                | None | Some "" -> Path.Combine(webRoot, "bin")
                | Some p -> p
            let rootDir = Path.GetDirectoryName(settings.ProjectFile)
            let currentAssembly = Path.Combine(rootDir, settings.AssemblyFile)
            let fullDir = Path.Combine(rootDir, dir)
            [
                yield! Directory.EnumerateFiles(fullDir, "*.dll")
                yield! Directory.EnumerateFiles(fullDir, "*.exe")
                yield currentAssembly
                yield! settings.References
            ]        
            |> List.distinct
            , currentAssembly
        let cfg =
            {
                Compiler.UnpackCommand.Config.Create() with
                    Assemblies = assemblies
                    RootDirectory = webRoot
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScriptOutput
                    UnpackTypeScriptDeclaration = settings.TypeScriptDeclaration
                    DownloadResources = Option.isSome settings.DownloadResources
                    WebResourcesOnly = settings.PreBundle
                    ScriptsFromCurrentAssembly = if settings.PreBundle then Some currentAssembly else None
                    Loader = Some loader
                    Logger = logger
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.UnpackCommand.Instance.Execute(env, cfg)

    let HtmlOutputDirectory settings =
        match TryGetOutputDir settings with
        | None -> Path.Combine(settings.ProjectDir, "bin", "html")
        | Some dir -> dir

    let Html settings meta (logger: LoggerBase) =
        let outputDir = HtmlOutputDirectory settings
        sprintf "Generating static site into %s" (Uri(outputDir).LocalPath)
        |> logger.Out
        let main = settings.AssemblyFile
        let refs = List.ofArray settings.References
        let cfg =
            {
                Compiler.HtmlCommand.Config.Create(main) with
                    Mode = if settings.IsDebug then Compiler.HtmlCommand.Debug else Compiler.HtmlCommand.Release
                    OutputDirectory = outputDir
                    ProjectDirectory = settings.ProjectDir
                    ReferenceAssemblyPaths = refs
                    UnpackSourceMap = settings.SourceMap
                    UnpackTypeScript = settings.TypeScriptOutput
                    UnpackTypeScriptDeclaration = settings.TypeScriptDeclaration
                    DownloadResources = settings.DownloadResources |> Option.defaultValue false
                    Metadata = meta
                    Logger = logger
            }
        let env = Compiler.Commands.Environment.Create()
        Compiler.HtmlCommand.Instance.Execute(env, cfg)

let LoadInterfaceGeneratorAssembly (file: string) (logger: LoggerBase) =
    let asm = WebSharper.Core.Reflection.LoadAssembly(file)
    let genFile = Path.ChangeExtension(file, ".Generator.dll")
    if File.Exists genFile then File.Delete genFile
    // Instead of removing the original dll, we are going to keep the original F# dll as is,
    // so we can properly carry over the win32 resources, so that the file properties are populated
    File.Copy(file, genFile)
    let name = asm.GetName()
    let typedArg =
        asm.CustomAttributes |> Seq.tryPick (fun a ->
            if a.AttributeType.FullName = "WebSharper.InterfaceGenerator.Pervasives+ExtensionAttribute" then
                Some a.ConstructorArguments.[0]
            else
                None
        )
    match typedArg with 
    | Some a ->
        let typeName = (a.Value :?> Type).FullName
        let t = asm.GetType(typeName)
        let e = Activator.CreateInstance(t)
        logger.TimedStage "Loading IExtension implementation"
        let assemblyGetter =
            let intfMap = t.GetInterfaceMap(typeof<WebSharper.InterfaceGenerator.Pervasives.IExtension>)
            intfMap.InterfaceMethods.[0]
        let asmDefObj = assemblyGetter.Invoke(e, [||])
        let asmDef = asmDefObj :?> WebSharper.InterfaceGenerator.CodeModel.Assembly
        logger.TimedStage "Getting Assembly definition"
        name, asmDef, asm
    | None ->
        failwith "No ExtensionAttribute set on the input assembly"

let RunInterfaceGenerator (aR: AssemblyResolver) snk config (logger: LoggerBase) =
    let (name, asmDef, asm) = LoadInterfaceGeneratorAssembly config.AssemblyFile logger        
    let cfg =
        {
            InterfaceGenerator.CompilerOptions.Default(name.Name) with
                AssemblyResolver = Some aR
                AssemblyVersion = name.Version
                DocPath = config.Documentation
                EmbeddedResources = config.Resources |> Seq.map fst
                ProjectDir = config.ProjectDir
                ReferencePaths = config.References
                StrongNameKeyPath = snk
        }
    let cmp = InterfaceGenerator.Compiler.Create(logger)
    // Passing in the original assembly file location, so that we can extend it, instead of creating a new assembly from scratch
    let out = cmp.Compile(cfg, asmDef, config.AssemblyFile, asm)
    FrontEnd.HandleExtraFiles (Some out.Assembly) name.Name config.ProjectFile config.OutputDir false
    out.Save config.AssemblyFile
    logger.TimedStage "Writing final dll"

let (|StartsWith|_|) (start: string) (input: string) =    
    if input.StartsWith start then
        Some input.[start.Length ..]
    else None 

let (|Flag|_|) name (input: string) =
    if input = name
        || input = name + "+"
        || input = name + ":true"
    then Some true
    elif input = name + "-"
        || input = name + ":false"
    then Some false
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

type private HelpKind =
    | NoHelp
    | WSHelp
    | UnpackHelp
    | HtmlHelp

let HandleDefaultArgsAndCommands (logger: LoggerBase) argv isFSharp =

    let printInfo (logger: LoggerBase) helpKind = 
        let lang = if isFSharp then "F#" else "C#"
        let exe = if isFSharp then "wsfsc.exe" else "wscsc.exe"
        let compiler = if isFSharp then "fsc.exe" else "csc.exe"
        sprintf "WebSharper %s compiler version %s" lang AssemblyVersionInformation.AssemblyFileVersion
        |> logger.Out
        if isFSharp then
            sprintf "(F# Compiler Service version %s)" AssemblyVersionInformation.FcsVersion
            |> logger.Out
        else
            sprintf "(Roslyn version %s)" AssemblyVersionInformation.RoslynVersion
            |> logger.Out
        logger.Out ""
        sprintf "Usage: %s [WebSharper options] [%s options]" exe compiler
        |> logger.Out
        logger.Out ""
        match helpKind with
        | NoHelp ->
            sprintf "WebSharper options help: %s --help" exe
            |> logger.Out
            sprintf "Unpack command help: %s unpack --help" exe
            |> logger.Out
            sprintf "Html command help: %s html --help" exe
            |> logger.Out
        | WSHelp ->
            sprintf """WebSharper options:
  --ws:<type>           Set WebSharper project type; one of:
                          library, site, bundle, bundleonly, html, extension
  --wig                 InterfaceGenerator project
                          same as --ws:binding
  --library             Library project
                          same as --ws:library
  --bundle              Single-page application project
                          same as --ws:spa
  --bundleonly          SPA project with .js/.css outputs only
                          same as --ws:bundleonly
  --html                Static site generator project
                          same as --ws:html
  --site                Client-server application project
                          same as --ws:web
  --wswarnonly[+|-]     Print WebSharper-specific errors as warnings
                          default: false
  --dce[+|-]            Turn off dead code elimination for SPA projects
                          available for --ws:bundle,bundleonly
                          default: true
  --dlres[+|-]          Download remote js/css resources
                          available for --ws:site
                          default: false
  --printjs[+|-]        Print .js output
                          default: false
  --wsoutput:<dir>      Specify output directory for WebSharper-generated files
                          available for --ws:html,bundle,bundleOnly
  --project:<path>      Location of project file
  --closures[+|-]       Enable JS closure analysis
                          default: false
  --closures:movetotop  Enable JS closure optimization
  --standalone[+|-]     Use WebSharper compilation outside the service
                          default: false"""
            |> logger.Out
        | UnpackHelp ->
            sprintf "%s" (UnpackCommand.Instance.Usage.Replace("WebSharper.exe", exe))    
            |> logger.Out
        | HtmlHelp ->
            sprintf "%s" (HtmlCommand.Instance.Usage.Replace("WebSharper.exe", exe))    
            |> logger.Out
        Some 0

    match List.ofArray argv with
    | [] -> printInfo logger NoHelp
    | [ "--help" ] -> printInfo logger WSHelp
    | [ "unpack"; "--help" ] -> printInfo logger UnpackHelp
    | [ "html"; "--help" ] -> printInfo logger HtmlHelp
    | Cmd HtmlCommand.Instance r -> Some r
    | Cmd UnpackCommand.Instance r -> Some r
    | _ -> None

let RecognizeWebSharperArg a wsArgs =
    match a with
    | Flag "--jsmap" v -> Some { wsArgs with SourceMap = v }
    | "--ts" -> Some { wsArgs with TypeScriptOutput = true } 
    | "--dts" -> Some { wsArgs with TypeScriptDeclaration = true } 
    | "--wig" -> Some { wsArgs with ProjectType = Some WIG }
    | "--bundle" -> Some { wsArgs with ProjectType =  Some Bundle }
    | "--bundleonly" -> Some { wsArgs with ProjectType = Some BundleOnly }
    | "--html" -> Some { wsArgs with ProjectType = Some Html }
    | "--site" -> Some { wsArgs with ProjectType = Some Website }
    | Flag "--wswarnonly" v -> Some { wsArgs with WarnOnly = v }
    | Flag "--dce" v -> Some { wsArgs with DeadCodeElimination = Some v }
    | StartsWith "--ws:" wsProjectType ->
        let pt = ProjectType.Parse wsProjectType
        let w = pt |> Option.bind (fun pt -> ProjectType.GetWarning(wsProjectType, pt))
        match w with
        | Some w -> 
            Some { wsArgs with ProjectType = pt; ArgWarnings = w :: wsArgs.ArgWarnings }
        | None ->
            Some { wsArgs with ProjectType = pt }
    | Flag "--dlres" v -> Some { wsArgs with DownloadResources = if v then Some true else None }
    | Flag "--printjs" v -> Some { wsArgs with PrintJS = v }
    | StartsWith "--wsoutput:" o ->
        Some { wsArgs with OutputDir = Some o }
    | StartsWith "--configuration:" c ->
        Some { wsArgs with Configuration = c }
    | StartsWith "--project:" p ->
        Some { wsArgs with ProjectFile = Path.Combine(Directory.GetCurrentDirectory(), p) }
    | StartsWith "--jsoutput:" f ->
        Some { wsArgs with JSOutputPath = Some f }
    | StartsWith "--minjsoutput:" f ->
        Some { wsArgs with MinJSOutputPath = Some f }
    | Flag "--closures" v ->
        Some { wsArgs with AnalyzeClosures = Some v }
    | StartsWith "--closures:" c ->
        match c.ToLower() with
        | "movetotop" ->
            Some { wsArgs with AnalyzeClosures = Some true }
        | _ ->
            printfn "--closures:%s argument unrecognized, must be one of: true, false, movetotop" c
            Some wsArgs
    | StartsWith "--scriptbaseurl" u -> Some { wsArgs with ScriptBaseUrl = Some u }
    | StartsWith "--wsconfig:" c ->
        if File.Exists c then
            Some (wsArgs.AddJson(File.ReadAllText c, Path.GetFileName c, wsArgs.Configuration))
        else 
            argError (sprintf "Cannot find WebSharper configuration file %s" c)    
    | Flag "--standalone" v -> Some { wsArgs with Standalone = wsArgs.Standalone || v }
    | Flag "--prebundle" v -> Some { wsArgs with PreBundle = wsArgs.PreBundle || v }
    | _ -> 
        None

let SetDefaultProjectFile isFSharp wsArgs =
    let ext = if isFSharp then ".fsproj" else ".csproj"
    match wsArgs.ProjectFile with
    | null ->
        let projFiles = Directory.GetFiles(Directory.GetCurrentDirectory(), "*" + ext)
        match projFiles with
        | [| p |] -> { wsArgs with ProjectFile = p }
        | [| |] -> argError "Cannot find project file, specify argument --project"
        | _ -> argError "Multiple project files in folder, specify argument --project"
    | _ -> wsArgs

let SetScriptBaseUrl wsArgs =
    match wsArgs.ProjectType, wsArgs.ScriptBaseUrl with
    | Some (Bundle | BundleOnly | Html), None -> { wsArgs with ScriptBaseUrl = Some "/Scripts/" }
    | _ -> wsArgs

let SetDefaultOutputDir wsArgs =
    match wsArgs.ProjectType, wsArgs.OutputDir with
    | Some Website, None -> { wsArgs with OutputDir = Some "wwwroot" }
    | Some Service, Some _ -> { wsArgs with OutputDir = None }
    | _ -> wsArgs

let SetDefaultUseJavaScriptSymbol wsArgs =
    match wsArgs.ProjectType, wsArgs.UseJavaScriptSymbol with
    | Some Proxy, None -> { wsArgs with UseJavaScriptSymbol = Some true }
    | _ -> wsArgs

let SetDefaults isFSharp wsArgs =
    wsArgs        
    |> SetDefaultProjectFile isFSharp
    |> SetScriptBaseUrl
    |> SetDefaultOutputDir
    |> SetDefaultUseJavaScriptSymbol