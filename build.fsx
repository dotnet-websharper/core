#load "paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.fsx"
#I "packages/build/AjaxMin/lib/net40"
#r "AjaxMin.dll"
#I "packages/build/Mono.Cecil/lib/net40"
#r "Mono.Cecil.dll"
#r "System.Xml.Linq"

open System.IO
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Fake
open WebSharper.Fake

let version = "4.5"
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
            let publishExe (mode: BuildMode) fw input output explicitlyCopyFsCore =
                let outputPath =
                    __SOURCE_DIRECTORY__ </> "build" </> mode.ToString() </> output </> fw </> "deploy"
                DotNetCli.Publish <| fun p ->
                    { p with
                        Project = input
                        Framework = fw
                        Output = outputPath
                        AdditionalArgs = ["--no-dependencies"; "--no-restore"]
                        Configuration = mode.ToString() }
                if explicitlyCopyFsCore then
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
                    publishExe mode "netcoreapp2.0" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" true
                    publishExe mode "netcoreapp2.0" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" true
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

Target "Prepare" <| fun () ->
    Minify()
    MakeNetStandardTypesList()
    AddToolVersions()
targets.AddPrebuild "Prepare"
"WS-GenAssemblyInfo" ==> "Prepare"

// Generate App.config redirects from the actual assemblies being used,
// because Paket gets some versions wrong.
Target "GenAppConfig" <| fun () ->
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
