
#r "nuget: FAKE.Core"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Tools.Git"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.DotNet.AssemblyInfoFile"
#r "nuget: Fake.DotNet.Paket"
#r "nuget: Paket.Core"

open System.IO
open System.Diagnostics
open System.Threading
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Tools
open System
open System.Text.RegularExpressions
open Fake.IO.Globbing.Operators

System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

#r "System.Xml.Linq"

#if INTERACTIVE
#r "nuget: NUglify"
#else
#r "paket:
nuget NUglify //"
#endif

module UpdateLicense =
    let thisDir = __SOURCE_DIRECTORY__
    let ( ++ ) a b = Path.Combine(a, b)
    let rootDir = thisDir ++ "../../../../.."

    /// Replaces the copyright notice in a given file.
    let replaceCopyright file (newCopyright: string []) =
        let lines =
            File.ReadAllText(file).Split '\n'
        if lines.Length > 2 then
            let lines =
                if String.IsNullOrWhiteSpace lines.[lines.Length - 1] then
                    lines.[..lines.Length - 2]
                else lines
            let endsWithR (l: string) = l.Length > 0 && l.[l.Length-1] = '\r'
            if endsWithR lines.[lines.Length - 2] && not (endsWithR lines.[lines.Length - 1]) then
                lines.[lines.Length - 1] <- lines.[lines.Length - 1] + "\r"
            let endsWith (s: string) (l: string) =
                l.EndsWith(s) || l.EndsWith(s + "\r")
            let mutable hasChanges = false
            let text =
                use output = new StringWriter()
                output.NewLine <- "\n"
                let mutable skip = false
                for line in lines do
                    if line |> endsWith "$begin{copyright}" then
                        skip <- true
                        hasChanges <- true
                        for c in newCopyright do
                            output.WriteLine(c)
                    if not skip then
                        output.WriteLine line
                    if line |> endsWith "$end{copyright}" then
                        skip <- false
                output.ToString()
            if hasChanges then
                File.WriteAllText(file, text)

    /// Adds the copyright notice in a given file.
    let addCopyright file (newCopyright: string []) =
        let oldText = File.ReadAllText(file)
        use f = File.OpenWrite(file)
        use w = new StreamWriter(f, NewLine = "\n")
        for l in newCopyright do
            w.WriteLine(l)
        w.Write(oldText)

    let readLicenseFile license =
        File.ReadAllLines(license)
        |> Array.map (fun line -> line.TrimEnd())

    /// Updates copyright notices in all F# files in a given folder.
    let updateLicense license =
        let copyright = readLicenseFile license
        let findFiles pattern =
            Directory.GetFiles(rootDir, pattern, SearchOption.AllDirectories)
        Array.concat [|
            findFiles "*.fs"
            findFiles "*.fsi"
            findFiles "*.fsx"
            findFiles "*.cs"
            findFiles "*.js"
        |]
        |> Array.iter (fun f ->
            stdout.WriteLine("Replacing: {0}", f)
            replaceCopyright f copyright
        )

    let excludedFilenames = [|
        "build.fsx"
        "runtests.fsx"
        "WebSharper.Fake.fsx"
        "UpdateLicense.fsx"
        "UpdateVersion.fsx"
        "FsEye.fsx"
    |]

    let findSourceFilesWithoutCopyright () =
        let findFiles pattern =
            Directory.GetFiles(rootDir, pattern, SearchOption.AllDirectories)
        let hasCopyright f =
            File.ReadAllLines(f)
            |> Array.exists (fun l ->
                l.Contains "$begin{copyright}" || l.Contains "$nocopyright")
        Array.concat [|
            findFiles "*.fs"
            findFiles "*.fsi"
            findFiles "*.fsx"
            findFiles "*.cs"
        |]
        |> Array.filter (fun f ->
            let fname = Path.GetFileName f
            not (Array.contains fname excludedFilenames)
            && not (fname.EndsWith ".g.cs")
            && not (fname.EndsWith ".g.fs")
            && not (hasCopyright f))

    /// Update all copyright notices in all F# files in WebSharper.
    let updateAllLicenses () =
        let ( ++ ) a b = Path.Combine(a, b)
        updateLicense (thisDir ++ "LICENSE.txt")

    /// Add copyright to files that don't, asking confirmation for each.
    let interactiveAddMissingCopyright () =
        let license = readLicenseFile (thisDir ++ "LICENSE.txt")
        for f in findSourceFilesWithoutCopyright () do
            printf "Add copyright to '%s'? (y/n) " f
            stdout.Flush()
            match stdin.ReadLine().Trim().ToLowerInvariant() with
            | "y" | "yes" -> addCopyright f license
            | _ -> ()

module WSFake =

    let usage = """
    usage: build [FAKE options] [--] [options]

    options:
    -v, --verbose      Verbose build output
    -d, --debug        Build in Debug configuration
    """

    let opts =
        let parsed = ref None
        fun (o: TargetParameter) ->
            match !parsed with
            | Some p -> p
            | None ->
                printfn "ARGS: %A" o.Context.Arguments
                try
                    let value = Docopt(usage).Parse(Array.ofSeq o.Context.Arguments |> Seq.append ["build"])
                    parsed := Some value
                    value
                with DocoptException _ ->
                    Trace.traceError usage
                    reraise()

    let verbose = fun _ -> false //opts >> DocoptResult.hasFlag "-v"
    
    let isDebug = fun _ -> false //opts >> DocoptResult.hasFlag "-d"

    let private mainGroupName = Paket.Domain.GroupName "Main"

    let GetSemVerOf pkgName =
        let lockFile = Paket.LockFile.LoadFrom "./paket.lock"
        lockFile
            .GetGroup(mainGroupName)
            .GetPackage(Paket.Domain.PackageName pkgName)
            .Version
        |> Some

    let shell program cmd =
        Printf.kprintf (fun cmd ->
            Shell.Exec(program, cmd, ".")
            |> function
                | 0 -> ()
                | n -> failwithf "%s %s failed with code %i" program cmd n
        ) cmd

    let shellOut program cmd =
        Printf.kprintf (fun cmd ->
            let psi =
                System.Diagnostics.ProcessStartInfo(
                    FileName = program,
                    Arguments = cmd,
                    RedirectStandardOutput = true,
                    UseShellExecute = false
                )
            let proc = System.Diagnostics.Process.Start(psi)
            let out = proc.StandardOutput.ReadToEnd()
            proc.WaitForExit()
            match proc.ExitCode with
            | 0 -> out
            | n -> failwithf "%s %s failed with code %i" program cmd n
        ) cmd

    let git cmd =
        Printf.kprintf (fun s ->
            Trace.logfn "> git %s" s
            Git.CommandHelper.directRunGitCommandAndFail "." s
        ) cmd

    let private splitLines (s: string) =
        s.Split([| "\r\n"; "\n" |], StringSplitOptions.None)

    let gitOut cmd =
        Printf.kprintf (fun s ->
            Trace.logfn "> git %s" s
            Git.CommandHelper.getGitResult "." s 
        ) cmd

    let gitSilentNoFail cmd =
        Printf.kprintf (fun s ->
            Git.CommandHelper.directRunGitCommand "." s
        ) cmd

    /// Generate a file at the given location, but leave it unchanged
    /// if the generated contents are identical to the existing file.
    /// `generate` receives the actual filename it should write to,
    /// which may be a temp file.
    let unchangedIfIdentical filename generate =
        if File.Exists(filename) then
            let tempFilename = Path.GetTempFileName()
            generate tempFilename
            if not (Shell.compareFiles true filename tempFilename) then
                File.Delete(filename)
                File.Move(tempFilename, filename)
        else
            generate filename

    module Git =
        let getCurrentBranch() =
            match Git.Information.getBranchName "." with
            | "NoBranch" ->
                // Jenkins runs git in "detached head" and sets this env instead
                Environment.environVar "GIT_BRANCH"
            | s -> s

    module VC =
        let getTags() =
            let _ok, out, _err = Git.CommandHelper.runGitCommand "." "tag --merged"
            Array.ofList out

        let getCurrentCommitId() =
            Git.Information.getCurrentSHA1 "."

        let commitIdForTag tag =
            Git.Branches.getSHA1 "." ("refs/tags/" + tag)

    let ComputeVersion (baseVersion: option<Paket.SemVerInfo>) =
        let lastVersion, tag =
            try
                let tags = VC.getTags()
                let re = Regex("""^(?:[a-zA-Z]+[-.]?)?([0-9]+(?:\.[0-9]+){0,3})(?:-.*)?$""")
                match tags |> Array.choose (fun tag ->
                    try
                        let m = re.Match(tag)
                        if m.Success then
                            let v = m.Groups.[1].Value |> Paket.SemVer.Parse
                            Some (v, Some tag)
                        else None
                    with _ -> None) with
                | [||] ->
                    Trace.traceImportant "Warning: no latest tag found"
                    Paket.SemVer.Zero, None
                | a ->
                    let v, t = Array.maxBy fst a
                    v, t
            with e ->
                Trace.traceImportant (sprintf "Warning: getting version from latest tag: %s" e.Message)
                Paket.SemVer.Zero, None
        let baseVersion = defaultArg baseVersion lastVersion
        let patch =
            try
                if lastVersion.Major <> baseVersion.Major || lastVersion.Minor <> baseVersion.Minor then
                    0u
                else
                    let head = VC.getCurrentCommitId()
                    let tagged =
                        match tag with
                        | Some tag -> VC.commitIdForTag tag
                        | None -> head
                    match Environment.environVarOrNone "INCREASE_PATCH_VERSION" with
                    | Some "true" -> lastVersion.Patch + 1u
                    | _ -> lastVersion.Patch
            with e ->
                Trace.traceImportant (sprintf "Warning: computing patch version: %s" e.Message)
                0u
        let build =
            match Environment.environVarOrNone "BUILD_NUMBER" with
            | None -> string (int64 lastVersion.Build + 1L)
            | Some b -> b
        Printf.ksprintf (fun v ->
            Trace.tracefn "==== Building project v%s ===="  v
            Paket.SemVer.Parse v
        ) "%i.%i.%i.%s%s" baseVersion.Major baseVersion.Minor patch build
            (match baseVersion.PreRelease with Some r -> "-" + r.Origin | None -> "")

    let LazyVersionFrom packageName =
        fun () -> GetSemVerOf packageName |> ComputeVersion

    type BuildMode =
        | Debug
        | Release

        override this.ToString() =
            match this with
            | Debug -> "Debug"
            | Release -> "Release"

        member this.AsDotNet =
            match this with
            | Debug -> DotNet.BuildConfiguration.Debug
            | Release -> DotNet.BuildConfiguration.Release

    [<NoComparison; NoEquality>]
    type BuildAction =
        | Projects of seq<string>
        | Custom of (BuildMode -> unit)
        | Multiple of seq<BuildAction>

        static member Solution s =
            BuildAction.Projects (!!s)

    [<NoComparison; NoEquality>]
    type Args =
        {
            GetVersion : unit -> Paket.SemVerInfo
            BuildAction : BuildAction
            Attributes : seq<AssemblyInfo.Attribute>
            WorkBranch : option<string>
            PushRemote : string
            HasDefaultBuild : bool
        }

    type WSTargets =
        {
            BuildDebug : string
            Publish : string
        }

        member this.AddPrebuild s =
            "WS-GenAssemblyInfo" ==> s 
            s ==> "WS-BuildDebug"
            s ==> "WS-BuildRelease"
            ()

    let msbuildVerbosity = 
        verbose >> function
        | true -> MSBuildVerbosity.Detailed
        | false -> MSBuildVerbosity.Minimal

    let buildModeFromFlag =
        isDebug >> function
        | true -> BuildMode.Debug
        | false -> BuildMode.Release

    let build o (mode: BuildMode) action =
        let rec buildRec action =
            match action with
            | BuildAction.Projects files ->
                let build = DotNet.build <| fun p ->
                    { p with
                        Configuration = mode.AsDotNet
                        MSBuildParams = 
                            match Environment.environVarOrNone "OS" with
                            | Some "Windows_NT" ->
                                { p.MSBuildParams with
                                    Verbosity = Some (msbuildVerbosity o)
                                    Properties = ["Configuration", string mode]
                                    DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                                }
                            | _ ->
                                p.MSBuildParams
                    }
                Seq.iter build files
            | Custom f -> f mode
            | Multiple actions -> Seq.iter buildRec actions
        buildRec action

    let MakeTargets (args: Args) =

        Target.create "WS-Stop" <| fun _ ->
            try
                Process.GetProcessesByName("wsfscservice")
                |> Array.iter (fun x -> x.Kill())
                |> ignore
            with
            | _ -> ()
        
        let dirtyDirs =
            !! "**/bin/Debug"
            ++ "**/bin/Release"
            ++ "**/obj/Debug"
            ++ "**/obj/Release"
            ++ "**/Scripts/WebSharper"
            ++ "**/Content/WebSharper"
            ++ "build"

        Target.create "WS-Clean" <| fun _ ->
            Seq.iter Directory.delete dirtyDirs

        Target.create "WS-Update" <| fun _ ->
            let depsFile = Paket.DependenciesFile.ReadFromFile "./paket.dependencies"
            let mainGroup = depsFile.GetGroup mainGroupName
            let needsUpdate =
                mainGroup.Packages
                |> Seq.exists (fun { Name = pkg } ->
                    pkg.Name.Contains "WebSharper")
            if needsUpdate then
                let res =
                    DotNet.exec id "paket"
                        (sprintf "update -g %s" mainGroup.Name.Name)
                if not res.OK then failwith "dotnet paket update failed"
            for g, _ in depsFile.Groups |> Map.toSeq do
                if g.Name.ToLower().StartsWith("test") then
                    let res =
                        DotNet.exec id "paket"
                            (sprintf "update -g %s" g.Name)
                    if not res.OK then failwith "dotnet paket update failed"

        Target.create "WS-Restore" <| fun o ->
            DotNet.exec id "paket" "restore"
            if not (Environment.environVarAsBoolOrDefault "NOT_DOTNET" false) then
                let slns = (Environment.environVarOrDefault "DOTNETSOLUTION" "").Trim('"').Split(';')
                let restore proj =
                    DotNet.restore (fun p -> 
                        { p with 
                            DisableParallel = true
                            MSBuildParams = 
                                { p.MSBuildParams with
                                    Verbosity = Some (msbuildVerbosity o)
                                    DisableInternalBinLog = true // workaround for https://github.com/fsharp/FAKE/issues/2515
                                }
                        }
                    ) proj
                if slns |> Array.isEmpty then
                    restore ""
                else
                    for sln in slns do
                        restore sln

        /// DO NOT force this lazy value in or before WS-Update.
        let version =
            lazy
            let version = args.GetVersion()
            let addVersionSuffix = Environment.environVarAsBoolOrDefault "AddVersionSuffix" false
            let version =
                if addVersionSuffix then
                    match args.WorkBranch, version.PreRelease with
                    | None, _ -> version
                    | Some b, Some p when b = p.Origin -> version
                    | Some b, _ -> Paket.SemVer.Parse (version.AsString + "-" + b)
                else version
            printfn "Computed version: %s" version.AsString
            version

        Target.create "WS-GenAssemblyInfo" <| fun _ ->
            unchangedIfIdentical ("build" </> "AssemblyInfo.fs") <| fun file ->
                AssemblyInfoFile.createFSharp file [
                    yield AssemblyInfo.Version (sprintf "%i.%i.0.0" version.Value.Major version.Value.Minor)
                    yield AssemblyInfo.FileVersion (sprintf "%i.%i.%i.%A" version.Value.Major version.Value.Minor version.Value.Patch version.Value.Build)
                    yield! args.Attributes
                ]

        Target.create "WS-BuildDebug" <| fun o ->
            build o BuildMode.Debug args.BuildAction

        Target.create "WS-BuildRelease" <| fun o ->
            build o BuildMode.Release args.BuildAction

        if args.HasDefaultBuild then
            Target.create "Build" ignore

        Target.create "WS-Package" <| fun _ ->      
            let outputPath = Environment.environVarOrNone "WSPackageFolder" |> Option.defaultValue "build"
            Paket.pack <| fun p ->
                { p with
                    ToolType = ToolType.CreateLocalTool()
                    OutputPath = outputPath
                    Version = version.Value.AsString
                }
            let versionsFilePath = Environment.environVarOrNone "WSVersionsFile" |> Option.defaultValue "build/versions.txt"
            let repoName = Directory.GetCurrentDirectory() |> Path.GetFileName
            if not (File.exists versionsFilePath) then
                File.writeNew versionsFilePath [ repoName + " " + version.Value.AsString ]
            else
                File.write true versionsFilePath [ repoName + " " + version.Value.AsString ]

        Target.create "WS-Checkout" <| fun _ ->
            match args.WorkBranch with
            | None -> ()
            | Some branch ->
                try git "checkout -f %s" branch
                with e ->
                    try git "checkout -f -b %s" branch
                    with _ -> raise e

        Target.create "CI-Release" ignore

        Target.create "UpdateLicense" <| fun _ ->
            UpdateLicense.updateAllLicenses ()

        Target.create "AddMissingLicense" <| fun _ ->
            UpdateLicense.interactiveAddMissingCopyright ()

        Target.create "CI-Commit" <| fun _ ->
            let versionsFilePath = Environment.environVarOrNone "WSVersionsFile" |> Option.defaultValue "build/versions.txt"
            let repoName = Directory.GetCurrentDirectory() |> Path.GetFileName
            if File.exists versionsFilePath then
                let versions = File.ReadAllLines versionsFilePath
                let version =
                    versions |> Array.tryPick (fun l ->
                        if l.StartsWith(repoName + " ") then
                            Some (l.[repoName.Length + 1 ..])
                        else None
                    )
                match version with
                | None ->
                    failwith "version not found in versions.txt"
                | Some v ->
                    git "add -A"
                    git "commit -m \"Version %s\" --allow-empty" v
                    git "push"
            else
                failwith "versions.txt not found"

        Target.create "CI-Tag" <| fun _ ->
            let lastCICommitLog =
                gitOut "log --author=\"ci@intellifactory.com\" "
            // prints something like:
                //commit 39a5220f342488162bc5625fd2db3f9c13048626 (HEAD -> websharper50, origin/websharper50)
                //Author: IntelliFactory CI <ci@intellifactory.com>
                //Date:   Tue Aug 3 16:42:05 2021 +0000
                //
                //    Version 5.0.0.60-preview1

            Trace.log "git log returned:"
            lastCICommitLog |> List.iter Trace.log

            let commitSHA = 
                lastCICommitLog |> Seq.pick (fun l ->
                    let l = l.Trim()
                    if l.StartsWith("commit ") then
                        Some (l.Split(' ').[1])
                    else
                        None
                )

            let tagName =
                lastCICommitLog |> Seq.pick (fun l ->
                    let l = l.Trim()
                    if l.StartsWith "Version " then
                        Some (l.Split(' ').[1])
                    else
                        None
                )
            
            try
                git "tag %s %s" tagName commitSHA
                git "push origin %s" tagName
            with _ ->
                Trace.log "Tagging failed, ignored."

        "WS-Clean"
            ==> "WS-Checkout"

        "WS-Clean"
            ==> "WS-Update"
            ?=> "WS-Restore"

        "WS-Restore"
            ==> "WS-BuildRelease"
            ==> "WS-Package"
            ==> "CI-Release"

        "WS-Clean"
            ?=> "WS-GenAssemblyInfo"
            ==> "WS-BuildDebug"
            =?> ("Build", args.HasDefaultBuild)

        "WS-GenAssemblyInfo"
            ==> "WS-BuildRelease"

        "WS-Update"
            ==> "CI-Release"

        "WS-Stop"
            ?=> "WS-Clean"

        "WS-Stop"
            ==> "WS-Update"
            ==> "CI-Commit"

        {
            BuildDebug = "Build"
            Publish = "CI-Release"
        }

    let RunTargets (targets: WSTargets) =
        Target.runOrDefaultWithArguments targets.BuildDebug

    type WSTargets with

        static member Default getVersion =
            let buildBranch = Environment.environVarOrNone "BuildBranch"
            {
                GetVersion = getVersion
                BuildAction = BuildAction.Solution "*.sln"
                Attributes = Seq.empty
                WorkBranch = buildBranch
                PushRemote = Environment.environVarOrDefault "PushRemote" "origin"
                HasDefaultBuild = true
            }

        static member Default () =
            WSTargets.Default (fun () -> ComputeVersion None)

let version = "6.1"
let pre = None

let baseVersion =
    version + match pre with None -> "" | Some x -> "-" + x
    |> Paket.SemVer.Parse

let publish rids (mode: WSFake.BuildMode) =
    let publishExe (mode: WSFake.BuildMode) fw input output explicitlyCopyFsCore =
        for rid in rids do
            let outputPath =
                __SOURCE_DIRECTORY__ </> "build" </> string mode </> output </> fw </> (rid |> Option.defaultValue "") </> "deploy"
            DotNet.publish (fun p ->
                { p with
                    Framework = Some fw
                    OutputPath = Some outputPath
                    NoRestore = true
                    SelfContained = false |> Some
                    Runtime = rid
                    Configuration = mode.AsDotNet
                    MSBuildParams =
                        {
                            p.MSBuildParams with
                                DisableInternalBinLog = true 
                        }
                }) input
            if explicitlyCopyFsCore then
                let fsharpCoreLib = __SOURCE_DIRECTORY__ </> "packages/includes/FSharp.Core/lib/netstandard2.0"
                [ 
                    fsharpCoreLib </> "FSharp.Core.dll" 
                ] 
                |> Shell.copy outputPath                
    publishExe mode "net8.0" "src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj" "FSharp" true
    publishExe mode "net8.0" "src/compiler/WebSharper.FSharp.Service/WebSharper.FSharp.Service.fsproj" "FSharp" true
    publishExe mode "net8.0" "src/compiler/WebSharper.CSharp/WebSharper.CSharp.fsproj" "CSharp" true

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
        ]
    if not (File.Exists(outFile) && t = File.ReadAllText(outFile)) then
        File.WriteAllText(outFile, t)

    // make minified scripts
    let needsBuilding input output =
        let i = FileInfo(input)
        let o = FileInfo(output)
        not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc
    let minify (path: string) =
        let out = Path.ChangeExtension(path, ".min.js")
        if needsBuilding path out then
            let raw = File.ReadAllText(path)
            let mjs = NUglify.Uglify.Js(raw).Code
            File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
            stdout.WriteLine("Written {0}", out)
    minify "src/compiler/WebSharper.Core.JavaScript/Runtime.js"
    minify "src/stdlib/WebSharper.Main/Json.js"
    minify "src/stdlib/WebSharper.Main/AnimFrame.js"

let targets = WSFake.MakeTargets {
    WSFake.WSTargets.Default (fun () -> WSFake.ComputeVersion (Some baseVersion)) with
        HasDefaultBuild = false
        BuildAction =
            WSFake.BuildAction.Multiple [
                WSFake.BuildAction.Projects ["WebSharper.Compiler.sln"]
                WSFake.BuildAction.Custom (publish [ None; Some "win-x64"; Some "linux-x64"; Some "linux-musl-x64"; Some "osx-x64" ])
                WSFake.BuildAction.Projects ["WebSharper.sln"]
            ]
}

targets.AddPrebuild "Prepare"

Target.create "Build" <| fun o ->
    WSFake.BuildAction.Multiple [
        WSFake.BuildAction.Projects ["WebSharper.Compiler.sln"]
    ]
    |> WSFake.build o (WSFake.buildModeFromFlag o) 

"Prepare"
    ==> "Build"

Target.create "Publish" <| fun o ->
    publish [ None ] (WSFake.buildModeFromFlag o)  
    
Target.create "BuildAll" <| fun o ->
    WSFake.BuildAction.Multiple [
        WSFake.BuildAction.Projects ["WebSharper.Compiler.sln"]
        //BuildAction.Custom publish
        WSFake.BuildAction.Projects ["WebSharper.NoTests.sln"]
    ]
    |> WSFake.build o (WSFake.buildModeFromFlag o) 

"Prepare"
    ==> "BuildAll"

Target.create "Tests" <| fun o ->
   WSFake.BuildAction.Multiple [
       WSFake.BuildAction.Projects ["WebSharper.Compiler.sln"]
       //BuildAction.Custom publish
       WSFake.BuildAction.Projects ["WebSharper.sln"]
   ]
   |> WSFake.build o (WSFake.buildModeFromFlag o)

"Prepare"
    ==> "Tests"

Target.create "RunCompilerTestsRelease" <| fun _ ->
    if Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false then
        Trace.log "Compiler testing skipped"
    else

    [
        //"tests/WebSharper.Compiler.FSharp.Tests/WebSharper.Compiler.FSharp.Tests.fsproj"
        // yield "tests/WebSharper.Core.JavaScript.Tests/WebSharper.Core.JavaScript.Tests.fsproj"
        // if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform System.Runtime.InteropServices.OSPlatform.Windows then 
        //     yield "tests/WebSharper.CSharp.Analyzer.Tests/WebSharper.CSharp.Analyzer.Tests.fsproj"
    ]
    |> List.iter (
        DotNet.test (fun t ->
            { t with
                NoRestore = true
                Configuration = DotNet.BuildConfiguration.Release
                MSBuildParams =
                    {
                        t.MSBuildParams with
                            DisableInternalBinLog = true
                    }
            }
        ) 
    ) 

"WS-BuildRelease"
    ?=> "RunCompilerTestsRelease"
    ?=> "WS-Package"

"RunCompilerTestsRelease"
    ==> "CI-Release"

Target.create "RunSPATestsRelease" <| fun _ ->
    if Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false then
        Trace.log "Chutzpah testing for SPA skipped"
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

Target.create "RunMainTestsRelease" <| fun _ ->
    if true || Environment.environVarAsBoolOrDefault "SKIP_CORE_TESTING" false || not <| System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform System.Runtime.InteropServices.OSPlatform.Windows then
        Trace.log "Chutzpah testing skipped"
    else

    Trace.log "Starting Web test project"
    let mutable startedOk = false
    let started = new EventWaitHandle(false, EventResetMode.ManualReset)

    use webTestsProc = new Process()
    webTestsProc.StartInfo.FileName <- @"build\Release\Tests\net8.0\Web.exe"
    webTestsProc.StartInfo.WorkingDirectory <- @"tests\Web"
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

    webTestsProc.Start()
    webTestsProc.BeginOutputReadLine()
    started.WaitOne()
    Thread.Sleep(5000)

    let res =
        Shell.Exec(
            "packages/test/Chutzpah/tools/chutzpah.console.exe", 
            "http://localhost:5000/consoletests /engine Chrome /parallelism 1 /silent /failOnError /showFailureReport"
        )
    webTestsProc.Kill()
    if res <> 0 then
        failwith "Chutzpah test run failed"

"WS-BuildRelease"
    ?=> "RunSPATestsRelease"
    ==> "RunMainTestsRelease"
    ?=> "WS-Package"

"RunMainTestsRelease"
    ==> "CI-Release"

"WS-Restore" ==> "Prepare"
"WS-Stop" ==> "WS-Clean"
"WS-Stop" ==> "WS-Restore"

Target.runOrDefaultWithArguments "Build"
