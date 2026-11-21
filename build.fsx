#r "System.Xml.Linq"
#r "nuget: FAKE.Core"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Tools.Git"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Paket"
#r "nuget: Fake.JavaScript.Npm"
#r "nuget: Paket.Core, 12.0.0-alpha001"
#r "nuget: MSBuild.StructuredLogger"
#r "nuget: NUglify"
#r "nuget: NuGet.Packaging"
#r "nuget: NuGet.Protocol"

open System.IO
open System.Diagnostics
open System.Threading
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.JavaScript

System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"

open WebSharper.Fake
let version = "11.0"
let pre = Some "preview.1.booster-packaging"

let baseVersion =
    version + match pre with None -> "" | Some x -> "-" + x
    |> Paket.SemVer.Parse

let computedVersion = ComputeVersion (Some baseVersion)

/// Platforms of the Booster tools, each a ReadyToRun publish of the compilers: the 64-bit platforms .NET
/// supports for development machines and CI. 32-bit hosts (win-x86, linux-arm, linux-musl-arm) use the
/// portable compiler of WebSharper.FSharp / WebSharper.CSharp. Same list as RuntimeIdentifiers in the
/// compiler projects.
let boosterRids =
    [
        "win-x64"; "win-arm64"
        "linux-x64"; "linux-arm64"
        "linux-musl-x64"; "linux-musl-arm64"
        "osx-x64"; "osx-arm64"
    ]

/// Compiler executables, in the order they publish for one platform: wsfsc references wsfscservice, wscsc
/// references wscscservice.
let compilerExes =
    [
        "src/compiler/WebSharper.FSharp.Service/WebSharper.FSharp.Service.fsproj", "FSharp"
        "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj", "FSharp"
        "src/compiler/WebSharper.CSharp.Service/WebSharper.CSharp.Service.fsproj", "CSharp"
        "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj", "CSharp"
    ]

/// Runs dotnet with its output in a log file, printed when it fails.
let runDotnetLogged (args: string list) (logFile: string) =
    let psi = ProcessStartInfo(DotNet.Options.Create().DotNetCliPath)
    for a in args do psi.ArgumentList.Add a
    psi.WorkingDirectory <- __SOURCE_DIRECTORY__
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    use p = new Process(StartInfo = psi)
    let output = System.Text.StringBuilder()
    let add (e: DataReceivedEventArgs) = if not (isNull e.Data) then lock output (fun () -> output.AppendLine e.Data |> ignore)
    p.OutputDataReceived.Add add
    p.ErrorDataReceived.Add add
    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    let text = lock output (fun () -> output.ToString())
    Directory.CreateDirectory(Path.GetDirectoryName logFile) |> ignore
    File.WriteAllText(logFile, text)
    if p.ExitCode <> 0 then
        Trace.traceError text
    p.ExitCode

/// Publishes the compilers to build/<mode>/<FSharp|CSharp>/net11.0/[<rid>/]deploy: None is the portable
/// compiler, Some rid a ReadyToRun build. The platforms publish in parallel after one restore with
/// ReadyToRun (which brings the runtime and crossgen packs of every platform), without restoring or
/// building the referenced libraries again (the portable publish or the solution build did): each platform
/// has its own obj/ and bin/ folders, so the parallel publishes share no output.
let publish rids (mode: BuildMode) =
    let fw = "net11.0"
    let logDir = __SOURCE_DIRECTORY__ </> "build" </> string mode </> "publish-logs"
    let deploy output (rid: string option) =
        __SOURCE_DIRECTORY__ </> "build" </> string mode </> output </> fw </> (rid |> Option.defaultValue "") </> "deploy"
    let publishArgs (proj: string) output (rid: string option) extra =
        [
            yield "publish"; yield proj
            yield "-f"; yield fw
            yield "-c"; yield string mode
            yield "-o"; yield deploy output rid
            yield "--self-contained"; yield "false"
            yield "-nr:false"
            match rid with
            | Some r -> yield "-r"; yield r
            | None -> ()
            yield! extra
        ]
    let check what code = if code <> 0 then failwithf "%s failed (exit code %d), see %s" what code logDir
    // portable compiler first: it also builds the referenced libraries if needed
    if rids |> List.contains None then
        for (proj, output) in compilerExes do
            let name = Path.GetFileNameWithoutExtension proj
            Trace.log (sprintf "Publishing %s (portable)" name)
            runDotnetLogged (publishArgs proj output None []) (logDir </> sprintf "%s.portable.log" name)
            |> check (sprintf "Publishing %s" name)
    let platforms = rids |> List.choose id
    if not platforms.IsEmpty then
        for (proj, _) in compilerExes do
            let name = Path.GetFileNameWithoutExtension proj
            Trace.log (sprintf "Restoring %s for %s" name (String.concat ", " platforms))
            runDotnetLogged [ "restore"; proj; "-p:PublishReadyToRun=true"; "-nr:false" ] (logDir </> sprintf "%s.restore.log" name)
            |> check (sprintf "Restoring %s" name)
        // one job per platform and output folder: the F# job publishes wsfscservice, then wsfsc
        let jobs =
            [
                for rid in platforms do
                    for output in compilerExes |> List.map snd |> List.distinct do
                        yield async {
                            for (proj, _) in compilerExes |> List.filter (fun (_, o) -> o = output) do
                                let name = Path.GetFileNameWithoutExtension proj
                                let sw = Stopwatch.StartNew()
                                let code =
                                    runDotnetLogged
                                        (publishArgs proj output (Some rid) [ "--no-restore"; "--no-dependencies" ])
                                        (logDir </> sprintf "%s.%s.log" name rid)
                                Trace.log (sprintf "Published %s for %s in %.0f s (exit code %d)" name rid sw.Elapsed.TotalSeconds code)
                                check (sprintf "Publishing %s for %s" name rid) code
                        }
            ]
        Async.Parallel(jobs, maxDegreeOfParallelism = max 2 System.Environment.ProcessorCount)
        |> Async.RunSynchronously
        |> ignore

/// msbuild/WebSharper.CompilerVersion.props, packed next to the props of WebSharper.FSharp and
/// WebSharper.CSharp: the WebSharper task uses a Booster tool only at exactly this version
let writeVersionProps (version: string) =
    let versionProps = "msbuild/WebSharper.CompilerVersion.props"
    let vp =
        String.concat "\r\n" [
            "<Project>"
            "  <PropertyGroup>"
            sprintf "    <WebSharperCompilerVersion>%s</WebSharperCompilerVersion>" version
            "  </PropertyGroup>"
            "</Project>"
            ""
        ]
    if not (File.Exists(versionProps) && vp = File.ReadAllText(versionProps)) then
        File.WriteAllText(versionProps, vp)

Target.create "Prepare" <| fun _ ->
    // make netstandardtypes.txt
    let f = FileInfo("src/compiler/WebSharper.Core/netstandardtypes.txt")
    if not f.Exists then
        let asm =
            "packages/includes/NETStandard.Library.Ref/ref/netstandard2.1/netstandard.dll"
            |> Mono.Cecil.AssemblyDefinition.ReadAssembly
        use s = f.OpenWrite()
        use w = new StreamWriter(s)
        w.WriteLine(asm.FullName)
        let rec writeType (t: Mono.Cecil.TypeDefinition) =
            w.WriteLine(t.FullName.Replace('/', '+'))
            Seq.iter writeType t.NestedTypes
        Seq.iter writeType asm.MainModule.Types

    // make msbuild/AssemblyInfo.fs
    let lockFile =
        Paket.LockFile.LoadFrom(__SOURCE_DIRECTORY__ </> "paket.lock")
    let roslynVersion = 
        lockFile
            .GetGroup(Paket.Domain.GroupName "main")
            .GetPackage(Paket.Domain.PackageName "Microsoft.CodeAnalysis.CSharp")
            .Version.AsString
    let fcsVersion = 
        lockFile
            .GetGroup(Paket.Domain.GroupName "fcs")
            .GetPackage(Paket.Domain.PackageName "FSharp.Compiler.Service")
            .Version.AsString
    let inFile = "build/AssemblyInfo.fs" // Generated by WS-GenAssemblyInfo
    let outFile = "msbuild/AssemblyInfo.fs"
    let t =
        String.concat "\r\n" [
            yield File.ReadAllText(inFile)
            yield sprintf "    let [<Literal>] FcsVersion = \"%s\"" fcsVersion
            yield sprintf "    let [<Literal>] RoslynVersion = \"%s\"" roslynVersion
            yield sprintf "    let [<Literal>] WSVersion = \"%s\"" computedVersion.AsString
        ]
    if not (File.Exists(outFile) && t = File.ReadAllText(outFile)) then
        File.WriteAllText(outFile, t)

    writeVersionProps computedVersion.AsString

    //// make minified scripts
    //let needsBuilding input output =
    //    let i = FileInfo(input)
    //    let o = FileInfo(output)
    //    not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc
    //let minify (path: string) =
    //    let out = Path.ChangeExtension(path, ".min.js")
    //    if needsBuilding path out then
    //        let raw = File.ReadAllText(path)
    //        let mjs = NUglify.Uglify.Js(raw).Code
    //        File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
    //        stdout.WriteLine("Written {0}", out)
    //minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"

    //// install TypeScript
    //Npm.install <| fun o -> 
    //    { o with 
    //        WorkingDirectory = "./src/compiler/WebSharper.TypeScriptParser/"
    //    }

    //Npm.install <| fun o -> 
    //    { o with 
    //        WorkingDirectory = "./tests/Web/"
    //    }

let stopService() =
    try
        Process.GetProcessesByName("wsfscservice")
        |> Array.iter (fun x -> 
            if x.MainModule.FileName.StartsWith(__SOURCE_DIRECTORY__) then
                x.Kill()
        )
        |> ignore
    with
    | _ -> ()

let targets =
    MakeTargets {
        WSTargets.Default (fun () -> computedVersion) with
            HasDefaultBuild = false
            BuildAction =
                BuildAction.Multiple [
                    BuildAction.Projects ["WebSharper.Compiler.sln"]
                    //BuildAction.Custom (publish [ Some "win-x64" ])
                    // portable compiler for WebSharper.FSharp / WebSharper.CSharp, ReadyToRun builds for the Booster tools (PackBoosters)
                    BuildAction.Custom (publish (None :: List.map Some boosterRids))
                    BuildAction.Projects ["WebSharper.sln"]
                    BuildAction.Custom (fun _ -> stopService())
                ]
            Attributes =
                [
                    AssemblyInfo.Company "IntelliFactory"
                    AssemblyInfo.Copyright "(c) IntelliFactory 2025"
                    
                    AssemblyInfo.Description "https://github.com/dotnet-websharper/core"
                    AssemblyInfo.Product "WebSharper"
                ]
    }

targets.AddPrebuild "Prepare"

Target.create "WS-Stop" <| fun _ -> stopService()
    
"WS-Stop"
    ?=> "WS-Clean"

"WS-Stop"
    ==> "WS-Update"

Target.create "Build" <| fun o ->
    BuildAction.Multiple [
        BuildAction.Projects ["WebSharper.Compiler.sln"]
    ]
    |> build o (buildModeFromFlag o) 

"WS-Restore"
    ==> "Prepare"
    ==> "Build"

Target.create "Publish" <| fun o ->
    publish [ None ] (buildModeFromFlag o)  
    
Target.create "BuildAll" <| fun o ->
    BuildAction.Multiple [
        BuildAction.Projects ["WebSharper.Compiler.sln"]
        //BuildAction.Custom publish
        BuildAction.Projects ["WebSharper.NoTests.sln"]
    ]
    |> build o (buildModeFromFlag o) 

"Prepare"
    ==> "BuildAll"

Target.create "Tests" <| fun o ->
   BuildAction.Multiple [
       BuildAction.Projects ["WebSharper.Compiler.sln"]
       //BuildAction.Custom publish
       BuildAction.Projects ["WebSharper.sln"]
   ]
   |> build o (buildModeFromFlag o)

"Prepare"
    ==> "Tests"

Target.create "RunCompilerTestsRelease" <| fun _ ->
    if Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false then
        Trace.log "Compiler testing skipped"
    else

    [
        //"tests/WebSharper.Compiler.FSharp.Tests/WebSharper.Compiler.FSharp.Tests.fsproj"
        yield "tests/WebSharper.Core.JavaScript.Tests/WebSharper.Core.JavaScript.Tests.fsproj"
        if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform System.Runtime.InteropServices.OSPlatform.Windows then 
            yield "tests/WebSharper.CSharp.Analyzer.Tests/WebSharper.CSharp.Analyzer.Tests.fsproj"
    ]
    |> List.iter (
        DotNet.test (fun t ->
            { t with
                NoRestore = true
                Configuration = DotNet.BuildConfiguration.Release
            }
        ) 
    ) 

// The metadata serializer is generated from the metadata types, fail if it was not regenerated after a change
Target.create "CheckGeneratedSerializer" <| fun _ ->
    let res = Shell.Exec("dotnet", "fsi src/compiler/WebSharper.Core/genSerializer.fsx --check build/Release/netstandard2.0/WebSharper.Core.dll")
    if res <> 0 then
        failwith "src/compiler/WebSharper.Core/Serialization.g.fs is out of date, run genSerializer.fsx in that folder"

"WS-BuildRelease"
    ?=> "CheckGeneratedSerializer"
    ?=> "RunCompilerTestsRelease"
    ?=> "WS-Package"

"CheckGeneratedSerializer"
    ==> "CI-Release"

"RunCompilerTestsRelease"
    ==> "CI-Release"

Target.create "RunSPATestsRelease" <| fun _ ->
    if Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false then
        Trace.log "QUnit Puppeteer testing for SPA skipped"
    else
    // TODO resolve cross site issues for automatic testing
    ()

    //let res =
    //    Shell.Exec(
    //        "packages/test/Chutzpah/tools/chutzpah.console.exe", 
    //        "tests/WebSharper.SPA.Tests/index.html /engine Chrome /parallelism 1 /silent /failOnError /showFailureReport"
    //    )
    //if res <> 0 then
    //    failwith "Chutzpah test run failed for SPA tests"

let runMainTestsRelease () =
    if Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false then
        Trace.log "QUnit Puppeteer testing skipped"
    else
        Trace.log "Starting Web test project"
        let mutable startedOk = false
        let started = new EventWaitHandle(false, EventResetMode.ManualReset)

        let webDll = __SOURCE_DIRECTORY__ </> "build/Release/Tests/net11.0/Web.dll"
        let webDir = __SOURCE_DIRECTORY__ </> "tests/Web"

        if not (File.Exists webDll) then
            failwithf "Release Web test project not found: %s. Build it first with WS-BuildRelease or `dotnet build tests/Web/Web.csproj -c Release`." webDll

        use webTestsProc = new Process()
        webTestsProc.StartInfo.FileName <- "dotnet"
        webTestsProc.StartInfo.Arguments <- $"exec \"{webDll}\" --server.urls https://localhost:44336"
        webTestsProc.StartInfo.WorkingDirectory <- webDir
        webTestsProc.StartInfo.UseShellExecute <- false
        webTestsProc.StartInfo.RedirectStandardOutput <- true
        
        webTestsProc.OutputDataReceived.Add(fun d -> 
            if not (isNull d) then
                if not startedOk then            
                    Trace.log d.Data
                if d.Data.Contains("Application started.") then
                    startedOk <- true   
                    started.Set() |> ignore
        )
        webTestsProc.Exited.Add(fun _ -> 
            if not startedOk then
                failwith "Starting Web test project failed."    
        )

        webTestsProc.Start() |> ignore
        webTestsProc.BeginOutputReadLine()
        try
            started.WaitOne() |> ignore
            
            Thread.Sleep(1000)

            Npm.install id

            let node =
                if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform System.Runtime.InteropServices.OSPlatform.Windows then
                    @"C:\Program Files\nodejs\node.exe"
                else
                    "node"

            let res =
                Shell.Exec(node, "runtests.js")

            if res <> 0 then
                failwith "QUnit Puppeteer test run failed"
        finally
            if not webTestsProc.HasExited then
                webTestsProc.Kill()

Target.create "RunMainTestsNoBuildRelease" <| fun _ ->
    runMainTestsRelease()

Target.create "RunMainTestsRelease" <| fun _ ->
    runMainTestsRelease()


//region ** Booster tool packages **
// WebSharper.FSharp.Booster and WebSharper.CSharp.Booster: the ReadyToRun compilers and their boosters as RID-specific .NET tools
// (SDK 10+). A pointer package (type DotnetTool) lists one <id>.<rid> package (type DotnetToolRidPackage) per
// platform, the CLI downloads only the one for the current platform. The WebSharper targets use an installed
// Booster of the same version instead of the portable compiler of WebSharper.FSharp / WebSharper.CSharp.


type BoosterTool =
    {
        Id: string
        Command: string
        Folder: string
        Executables: string list
        Description: string
    }

let boosterTools =
    [
        {
            Id = "WebSharper.FSharp.Booster"
            Command = "wsfsc"
            Folder = "FSharp"
            Executables = [ "wsfsc"; "wsfscservice" ]
            Description = "ReadyToRun build of the WebSharper F# compiler and its booster service for the current platform. Install it as a .NET tool at the version of WebSharper.FSharp to use it instead of the portable compiler."
        }
        {
            Id = "WebSharper.CSharp.Booster"
            Command = "wscsc"
            Folder = "CSharp"
            Executables = [ "wscsc"; "wscscservice" ]
            Description = "ReadyToRun build of the WebSharper C# compiler and its booster for the current platform. Install it as a .NET tool at the version of WebSharper.CSharp to use it instead of the portable compiler."
        }
    ]

let packBoosters (mode: BuildMode) (rids: string list) (version: string) (outputPath: string) =
    let workDir = __SOURCE_DIRECTORY__ </> "build" </> string mode </> "booster"
    Directory.CreateDirectory outputPath |> ignore
    let save (b: NuGet.Packaging.PackageBuilder) =
        let path = outputPath </> sprintf "%s.%s.nupkg" b.Id (b.Version.ToNormalizedString())
        use fs = File.Create path
        b.Save fs
        Trace.log (sprintf "Created %s" path)
    let builder (id: string) (description: string) (packageType: string) =
        let b = NuGet.Packaging.PackageBuilder()
        b.Id <- id
        b.Version <- NuGet.Versioning.NuGetVersion.Parse version
        b.Authors.Add "IntelliFactory" |> ignore
        b.Description <- description
        b.ProjectUrl <- System.Uri "https://websharper.com/"
        b.Repository <- NuGet.Packaging.Core.RepositoryMetadata("git", "https://github.com/dotnet-websharper/core/", null, null)
        b.LicenseMetadata <-
            NuGet.Packaging.LicenseMetadata(
                NuGet.Packaging.LicenseType.Expression, "Apache-2.0",
                NuGet.Packaging.Licenses.NuGetLicenseExpression.Parse "Apache-2.0", null,
                NuGet.Packaging.LicenseMetadata.CurrentVersion)
        b.Tags.Add "WebSharper" |> ignore
        b.PackageTypes.Add(NuGet.Packaging.Core.PackageType(packageType, NuGet.Packaging.Core.PackageType.EmptyVersion))
        b
    let addFile (b: NuGet.Packaging.PackageBuilder) (src: string) (target: string) =
        b.Files.Add(NuGet.Packaging.PhysicalPackageFile(SourcePath = src, TargetPath = target.Replace('\\', '/')))
    let writeSettings (path: string) (lines: string list) =
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllLines(path, "<?xml version=\"1.0\" encoding=\"utf-8\"?>" :: lines)
    for tool in boosterTools do
        // platform packages first, see https://learn.microsoft.com/dotnet/core/tools/rid-specific-tools
        for rid in rids do
            let deploy = __SOURCE_DIRECTORY__ </> "build" </> string mode </> tool.Folder </> "net11.0" </> rid </> "deploy"
            if not (Directory.Exists deploy) then
                failwithf "%s is missing: publish the compiler for %s first" deploy rid
            let exe name = if rid.StartsWith "win-" then name + ".exe" else name
            let target = sprintf "tools/net11.0/%s" rid
            let b = builder (tool.Id + "." + rid) tool.Description "DotnetToolRidPackage"
            let settings = workDir </> tool.Id </> rid </> "DotnetToolSettings.xml"
            writeSettings settings [
                "<DotNetCliTool Version=\"2\">"
                "  <Commands>"
                sprintf "    <Command Name=\"%s\" EntryPoint=\"%s\" Runner=\"executable\" />" tool.Command (exe tool.Command)
                "  </Commands>"
                "</DotNetCliTool>"
            ]
            addFile b settings (target + "/DotnetToolSettings.xml")
            for file in Directory.EnumerateFiles(deploy, "*.dll", SearchOption.AllDirectories) do
                addFile b file (target + "/" + Path.GetRelativePath(deploy, file))
            for name in tool.Executables do
                for file in [ exe name; name + ".runtimeconfig.json"; name + ".dll.config" ] do
                    if File.Exists(deploy </> file) then
                        addFile b (deploy </> file) (target + "/" + file)
            if File.Exists(deploy </> "NLog.config") then
                addFile b (deploy </> "NLog.config") (target + "/NLog.config")
            // fsc looks for it next to the compiler when compiling an executable
            addFile b (__SOURCE_DIRECTORY__ </> "src/compiler/WebSharper.FSharp/default.win32manifest") (target + "/default.win32manifest")
            save b
        let b = builder tool.Id tool.Description "DotnetTool"
        let settings = workDir </> tool.Id </> "any" </> "DotnetToolSettings.xml"
        writeSettings settings [
            yield "<DotNetCliTool Version=\"2\">"
            yield "  <Commands>"
            yield sprintf "    <Command Name=\"%s\" />" tool.Command
            yield "  </Commands>"
            yield "  <RuntimeIdentifierPackages>"
            for rid in rids do
                yield sprintf "    <RuntimeIdentifierPackage RuntimeIdentifier=\"%s\" Id=\"%s.%s\" />" rid tool.Id rid
            yield "  </RuntimeIdentifierPackages>"
            yield "</DotNetCliTool>"
        ]
        addFile b settings "tools/any/any/DotnetToolSettings.xml"
        save b

Target.create "PackBoosters" <| fun _ ->
    let outputPath = Environment.environVarOrNone "WSPackageFolder" |> Option.defaultValue "build"
    packBoosters Release boosterRids computedVersion.AsString outputPath

// Local compiler packages (version x.y.z.999) in build/: WebSharper.FSharp and WebSharper.CSharp with the
// portable compiler, and with --runtimeID:<rid> also the Booster tools for that platform.
let RUNTIMEIDPARAM = "--runtimeID:"

let obtainRuntimeId () =
    fsi.CommandLineArgs
    |> Array.tryPick (fun arg ->
        if arg.StartsWith RUNTIMEIDPARAM then Some (arg.Substring RUNTIMEIDPARAM.Length) else None
    )

let buildProjectPrerequisites (mode: BuildMode) =
    let build =
        DotNet.build <| fun p ->
            { p with
                Configuration = mode.AsDotNet
                MSBuildParams = 
                    match Environment.environVarOrNone "OS" with
                    | Some "Windows_NT" ->
                        { p.MSBuildParams with
                            Properties = ["Configuration", string mode]
                            DisableInternalBinLog = true
                        }
                    | _ ->
                        p.MSBuildParams
            }
    build "src/compiler/WebSharper.MSBuild.CSharp/WebSharper.MSBuild.CSharp.csproj"
    build "src/compiler/WebSharper.MSBuild.FSharp/WebSharper.MSBuild.FSharp.csproj"

let buildPackage (mode: BuildMode) (rid: string option) (version: Paket.SemVerInfo) =
    printfn "%A %A" mode rid
    publish [ yield None; if rid.IsSome then yield rid ] mode
    // the next Prepare writes the version of regular builds again
    writeVersionProps version.AsString
    let package (tp: string) =
        let tempTemplatePath = tp.Replace(".paket.template", ".local.paket.template")
        File.WriteAllLines(tempTemplatePath, File.ReadAllLines tp |> Array.map (fun line -> line.Replace("Release", mode.ToString())))
        try
            Paket.pack <| fun p ->
                { p with
                    ToolType = ToolType.CreateLocalTool()
                    OutputPath = "build"
                    Version = version.AsString
                    TemplateFile = tempTemplatePath
                }
        finally
            File.Delete tempTemplatePath
    [
        "tools/WebSharper.FSharp.paket.template"
        "tools/WebSharper.CSharp.paket.template"
    ]
    |> List.iter package
    rid |> Option.iter (fun rid -> packBoosters mode [ rid ] version.AsString "build")

Target.create "WS-CompileDebugPackage" <| fun args ->
    let versionToBuild = { computedVersion with Build = bigint 999 }
    buildProjectPrerequisites Debug
    buildPackage Debug (obtainRuntimeId ()) versionToBuild

Target.create "WS-CompileReleasePackage" <| fun args ->
    let versionToBuild = { computedVersion with Build = bigint 999 }
    buildProjectPrerequisites Release
    buildPackage Release (obtainRuntimeId ()) versionToBuild

//endregion

"WS-BuildRelease"
    ==> "PackBoosters"
    ==> "WS-Package"

"WS-BuildRelease"
//    ?=> "RunSPATestsRelease"
    ==> "RunMainTestsRelease"
    ?=> "WS-Package"

"RunMainTestsRelease"
    ==> "CI-Release"

"WS-Restore" ==> "Prepare"
"WS-Stop" ==> "WS-Clean"
"WS-Stop" ==> "WS-Restore"

Target.runOrDefaultWithArguments "Build"
