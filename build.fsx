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

let version = "4.7"
let pre = None

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
                    publishExe mode "netcoreapp3.1" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" true
                    publishExe mode "netcoreapp3.1" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" true
                    publishExe mode "net461" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" false
                    publishExe mode "net461" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" false
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

// Generate App.config redirects from the actual assemblies being used,
// because Paket gets some versions wrong.
Target.create "GenAppConfig" <| fun _ ->
    [
        "build/Release/CSharp/net461/deploy", "ZafirCs.exe.config"
        "build/Release/FSharp/net461/deploy", "wsfsc.exe.config"
    ]
    |> List.iter (fun (dir, xmlFile) ->
        let xmlFullPath = dir </> xmlFile
        let mgr = XmlNamespaceManager(NameTable())
        mgr.AddNamespace("ac", "urn:schemas-microsoft-com:asm.v1")
        let doc = XDocument.Load(xmlFullPath)
        let e::rest = doc.XPathSelectElements("/configuration/runtime/ac:assemblyBinding", mgr) |> List.ofSeq
        e.RemoveAll()
        for e in rest do e.Remove()
        let loadElt (s: string) =
            let parserContext = XmlParserContext(null, mgr, null, XmlSpace.None)
            use reader = new XmlTextReader(s, XmlNodeType.Element, parserContext)
            XElement.Load(reader)
        for asmFullPath in Directory.GetFiles(dir, "*.dll") do
            if not (xmlFile.StartsWith(Path.GetFileName(asmFullPath))) then
                let asm = Mono.Cecil.AssemblyDefinition.ReadAssembly(asmFullPath)
                let token = asm.Name.PublicKeyToken
                let token = String.init token.Length (fun i -> sprintf "%02x" token.[i])
                sprintf """<ac:dependentAssembly>
                        <ac:assemblyIdentity name="%s" publicKeyToken="%s" culture="neutral" />
                        <ac:bindingRedirect oldVersion="0.0.0.0-65535.65535.65535.65535" newVersion="%A" />
                    </ac:dependentAssembly>"""
                    asm.Name.Name token asm.Name.Version
                |> loadElt
                |> e.Add
        doc.Save(xmlFullPath)
    )
    
"WS-BuildRelease"
    ==> "GenAppConfig"
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

Target.create "PublishTests" <| fun _ ->
    match Environment.environVarOrNone "WS_TEST_FOLDER" with
    | Some publishPath ->
        Shell.copyDir publishPath "tests/Web" (fun _ -> true)
    | _ ->
        failwithf "Could not find WS_TEST_FOLDER environment variable for publishing test project"

Target.create "RunTests" <| fun _ ->
    match Environment.environVarOrNone "WS_TEST_URL" with
    | Some publishUrl ->
        let res =
            Shell.Exec(
                "packages/test/Chutzpah/tools/chutzpah.console.exe",
                publishUrl + "/consoletests /engine Chrome"
            )
        if res <> 0 then
            failwith "Chutzpah test run failed"
    | _ ->
        failwithf "Could not find WS_TEST_URL environment variable for running tests"

"WS-BuildRelease"
    ==> "WS-Package"
    ==> "CI-Release"
"WS-BuildRelease"
    ==> "PublishTests"
    ==> "RunTests"
    ?=> "WS-Package"
"RunTests"
    ==> "CI-Release"

Target.runOrDefault "Build"
