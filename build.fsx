#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"
#I "packages/build/AjaxMin/lib/net40"
#r "AjaxMin.dll"
#I "packages/build/Mono.Cecil/lib/net40"
#r "Mono.Cecil.dll"

open System.IO
open Fake
open WebSharper.Fake

let version = "4.4"
let pre = None

let baseVersion =
    version + match pre with None -> "" | Some x -> "-" + x
    |> Paket.SemVer.Parse

let specificFw = environVarOrNone "WS_TARGET_FW"

let targets = MakeTargets {
    WSTargets.Default (fun () -> ComputeVersion (Some baseVersion)) with
        BuildAction =
            let buildSln sln =
                let sln =
                    match specificFw with
                    | None -> sln
                    | Some d -> d </> sln
                match environVarOrNone "OS" with
                | Some "Windows_NT" ->
                    BuildAction.Projects [sln]
                | _ ->
                    BuildAction.Custom <| fun mode ->
                        DotNetCli.Build <| fun p ->
                            { p with
                                Project = sln
                                Configuration = mode.ToString()
                            }
            let dest mode lang =
                __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> lang
            let publishExe (mode: BuildMode) input output =
                let outputPath = dest mode output </> "deploy"
                DotNetCli.Publish <| fun p ->
                    { p with
                        Project = input
                        Framework = "netcoreapp2.0"
                        Output = outputPath
                        AdditionalArgs = ["--no-dependencies"; "--no-restore"]
                        Configuration = mode.ToString() }
                let fsharpCoreLib = __SOURCE_DIRECTORY__ </> "packages/compilers/FSharp.Core/lib/netstandard1.6"
                [ 
                    fsharpCoreLib </> "FSharp.Core.dll" 
                    fsharpCoreLib </> "FSharp.Core.sigdata" 
                    fsharpCoreLib </> "FSharp.Core.optdata" 
                ] 
                |> Copy outputPath                
            BuildAction.Multiple [
                buildSln "WebSharper.Compiler.sln"
                BuildAction.Custom <| fun mode ->
                    publishExe mode "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp"
                    publishExe mode "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp"
                buildSln "WebSharper.sln"
            ]
}

let NeedsBuilding input output =
    let i = FileInfo(input)
    let o = FileInfo(output)
    not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc

let Minify () =
    let minify (path: string) =
        let min = Microsoft.Ajax.Utilities.Minifier()
        let out = Path.ChangeExtension(path, ".min.js")
        if NeedsBuilding path out then
            let raw = File.ReadAllText(path)
            let mjs = min.MinifyJavaScript(raw)
            File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
            stdout.WriteLine("Written {0}", out)
    minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"
    minify "src/stdlib/WebSharper.Main/Json.js"
    minify "src/stdlib/WebSharper.Main/AnimFrame.js"

let MakeNetStandardTypesList() =
    let f = FileInfo("src/compiler/WebSharper.Core/netstandardtypes.txt")
    if not f.Exists then
        let asm =
            "packages/includes/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
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
            .GetGroup(Paket.Domain.GroupName "main")
    let t =
        [
            "FcsVersion", "FSharp.Compiler.Service"
            "RoslynVersion", "Microsoft.CodeAnalysis.CSharp"
        ]
        |> List.map (fun (name, value) ->
            let version = lockFile.GetPackage(Paket.Domain.PackageName value).Version.AsString
            sprintf "    let [<Literal>] %s = \"%s\"" name version
        )
        |> String.concat "\n"
    File.AppendAllText("msbuild/AssemblyInfo.fs", t)

Target "Prepare" <| fun () ->
    File.Copy("build/AssemblyInfo.fs", "msbuild/AssemblyInfo.fs", true)
    Minify()
    MakeNetStandardTypesList()
    AddToolVersions()
targets.AddPrebuild "Prepare"
"WS-GenAssemblyInfo" ==> "Prepare"

Target "Build" DoNothing
targets.BuildDebug ==> "Build"

Target "CI-Release" DoNothing
targets.CommitPublish ==> "CI-Release"

let rm_rf x =
    if Directory.Exists(x) then
        // Fix access denied issue deleting a read-only *.idx file in .git
        for git in Directory.EnumerateDirectories(x, ".git", SearchOption.AllDirectories) do
            for f in Directory.EnumerateFiles(git, "*.*", SearchOption.AllDirectories) do
                File.SetAttributes(f, FileAttributes.Normal)
        Directory.Delete(x, true)
    elif File.Exists(x) then File.Delete(x)

Target "Clean" <| fun () ->
    rm_rf "netcore"
    rm_rf "netfx"
"WS-Clean" ==> "Clean"

Target "Run" <| fun () ->
    shellExec {
        defaultParams with
            Program = @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe"
            CommandLine = "/r WebSharper.sln"
    }
    |> ignore

"Build" ==> "Run"

RunTargetOrDefault "Build"
