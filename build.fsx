#if INTERACTIVE
#r "nuget: FAKE.Core"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Tools.Git"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Paket"
#r "nuget: Paket.Core"
#else
#r "paket:
nuget FAKE.Core
nuget Fake.Core.Target
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git
nuget Fake.DotNet.Cli
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Paket.Core //"
#endif

#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"

#r "System.Xml.Linq"

#if INTERACTIVE
#r "nuget: NUglify"
#else
#r "paket:
nuget NUglify //"
#endif

open System.IO
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open WebSharper.Fake

let version = "5.0"
let pre = Some "preview1"

let baseVersion =
    version + match pre with None -> "" | Some x -> "-" + x
    |> Paket.SemVer.Parse

let specificFw = Environment.environVarOrNone "WS_TARGET_FW"

let targets = MakeTargets {
    WSTargets.Default (fun () -> ComputeVersion (Some baseVersion)) with
        BuildAction =
            let buildSln sln =
                let sln =
                    match specificFw with
                    | None -> sln
                    | Some d -> d </> sln
                match Environment.environVarOrNone "OS" with
                | Some "Windows_NT" ->
                    BuildAction.Projects [sln]
                | _ ->
                    BuildAction.Custom <| fun mode ->
                        DotNet.build (fun p ->
                            { p with
                                Configuration = DotNet.BuildConfiguration.fromString (mode.ToString())
                                MSBuildParams = 
                                    { p.MSBuildParams with 
                                        DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                                    }
                            }) sln
            let dest mode lang =
                __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> lang
            let publishExe (mode: BuildMode) fw input output explicitlyCopyFsCore =
                let outputPath =
                    __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> output </> fw </> "deploy"
                DotNet.publish (fun p ->
                    { p with
                        Framework = Some fw
                        OutputPath = Some outputPath
                        NoRestore = true
                        Configuration = DotNet.BuildConfiguration.fromString (mode.ToString())
                        MSBuildParams = 
                            { p.MSBuildParams with 
                                DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                            }
                    }) input
                if explicitlyCopyFsCore then
                    let fsharpCoreLib = __SOURCE_DIRECTORY__ </> "packages/includes/FSharp.Core/lib/netstandard2.0"
                    [ 
                        fsharpCoreLib </> "FSharp.Core.dll" 
                    ] 
                    |> Shell.copy outputPath                
            BuildAction.Multiple [
                buildSln "WebSharper.Compiler.sln"
                BuildAction.Custom <| fun mode ->
                    publishExe mode "net5.0" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" true
                    publishExe mode "net5.0" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" true
                buildSln "WebSharper.sln"
            ]
}

let NeedsBuilding input output =
    let i = FileInfo(input)
    let o = FileInfo(output)
    not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc

let Minify () =
    let minify (path: string) =
        let out = Path.ChangeExtension(path, ".min.js")
        if NeedsBuilding path out then
            let raw = File.ReadAllText(path)
            let mjs = NUglify.Uglify.Js(raw).Code
            File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
            stdout.WriteLine("Written {0}", out)
    minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"
    minify "src/stdlib/WebSharper.Main/Json.js"
    minify "src/stdlib/WebSharper.Main/AnimFrame.js"

let MakeNetStandardTypesList() =
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

let AddToolVersions() =
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
        ]
    if not (File.Exists(outFile) && t = File.ReadAllText(outFile)) then
        File.WriteAllText(outFile, t)

Target.create "Prepare" <| fun _ ->
    Minify()
    MakeNetStandardTypesList()
    AddToolVersions()
targets.AddPrebuild "Prepare"
"WS-GenAssemblyInfo" ==> "Prepare"
    
"WS-BuildRelease"
    ==> "WS-Package"

let rm_rf x =
    if Directory.Exists(x) then
        // Fix access denied issue deleting a read-only *.idx file in .git
        for git in Directory.EnumerateDirectories(x, ".git", SearchOption.AllDirectories) do
            for f in Directory.EnumerateFiles(git, "*.*", SearchOption.AllDirectories) do
                File.SetAttributes(f, FileAttributes.Normal)
        Directory.Delete(x, true)
    elif File.Exists(x) then File.Delete(x)

Target.create "Clean" <| fun _ ->
    rm_rf "netcore"
    rm_rf "netfx"
"WS-Clean" ==> "Clean"

Target.create "Run" <| fun _ ->
    Shell.Exec(
        @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe",
        "/r WebSharper.sln"
    )
    |> ignore

"Build" ==> "Run"

Target.create "RunTests" <| fun _ ->
    match Environment.environVarOrNone "WS_TEST_URL" with
    | Some publishUrl ->
        use cancellationSource = new System.Threading.CancellationTokenSource()
        let startAsync = async {
            let dotnetRunResult = DotNet.exec (fun p ->
                                    { p with
                                        CustomParams = "" |> Some
                                        WorkingDirectory = "tests/Web/"
                                    }) "run" ""
            if not dotnetRunResult.OK then
                dotnetRunResult.Errors
                |> String.concat System.Environment.NewLine
                |> failwith
            }
        try
            startAsync
            |> fun x -> Async.Start (x, cancellationSource.Token)
            System.TimeSpan.FromMinutes 4.
            |> Async.Sleep
            |> Async.RunSynchronously
            let res =
                Shell.Exec(
                    "packages/test/Chutzpah/tools/chutzpah.console.exe",
                    publishUrl + "/consoletests /engine Chrome"
                )
            if res <> 0 then
                failwith "Chutzpah test run failed"
        finally
            cancellationSource.Cancel()
    | _ ->
        failwithf "Could not find WS_TEST_URL environment variable for running tests"

"WS-BuildRelease"
    ==> "WS-Package"
    ==> "CI-Release"
"WS-BuildRelease"
    ==> "RunTests"
    ?=> "WS-Package"
"RunTests"
    ==> "CI-Release"
    
Target.runOrDefault "Build"
