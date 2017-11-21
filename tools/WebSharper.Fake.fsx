/// Helpers for FAKE scripts for the standard WebSharper extensions.
/// Check the sources for usage.
(**
Uses the following environment variables (some of which are overridden by WSTargets options):
  * BUILD_NUMBER: the build number to use (last component of the version).
    Automatically set by Jenkins.
  * BuildBranch: the name of the branch to switch to before building, and to push to on success.
    Default: current branch
  * PushRemote: the name of the git or hg remote to push to
    Default: origin
  * NuGetPublishUrl: the URL of the NuGet server
    Default: https://nuget.intellifactory.com/nuget
  * NugetApiKey: the API key of the NuGet server.

Versioning policy (as implemented in ComputeVersion):
  * Major, minor and pre are taken from baseVersion.
  * Patch is set based on the latest tag:
    * If the tag's major/minor is different from baseVersion, then patch = 0.
    * If the major/minor is the same and the latest tag is on HEAD, then this is a rebuild
      of the same code: use the tag's patch number (they will be differenciated by ther Build).
    * Otherwise, increment from the tag's patch number.
  * Build is set to $(BUILD_NUMBER) if that is defined, baseVersion.Build + 1 otherwise.
*)
module WebSharper.Fake

#nowarn "211" // Don't warn about nonexistent #I
#nowarn "20"  // Ignore string result of ==>
#I "../packages/build/FAKE/tools"
#I "../../../../../packages/build/FAKE/tools"
#I "../packages/build/Paket.Core/lib/net45"
#I "../packages/build/Chessie/lib/net40"
#I "../../../../../packages/build/Paket.Core/lib/net45"
#I "../../../../../packages/build/Chessie/lib/net40"
#r "Chessie"
#r "Paket.Core"
#r "FakeLib"
#nowarn "49"

open System
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.AssemblyInfoFile

let private depsFile = Paket.DependenciesFile.ReadFromFile "./paket.dependencies"
let private lockFile = Paket.LockFile.LoadFrom "./paket.lock"
let private mainGroupName = Paket.Domain.GroupName "Main"
let private mainGroup = depsFile.GetGroup mainGroupName

let GetSemVerOf pkgName =
    lockFile
        .GetGroup(mainGroupName)
        .GetPackage(Paket.Domain.PackageName pkgName)
        .Version
    |> Some

let shell program cmd =
    Printf.kprintf (fun cmd ->
        shellExec {
            Program = program
            WorkingDirectory = "."
            CommandLine = cmd
            Args = []
        }
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
        logfn "> git %s" s
        Git.CommandHelper.directRunGitCommandAndFail "." s
    ) cmd

let hg cmd = shell "hg" cmd
let hg' cmd = shellOut "hg" cmd

let private splitLines (s: string) =
    s.Split([| "\r\n"; "\n" |], StringSplitOptions.None)

module Hg =
    let getBookmarks() =
        hg' """bookmark -T "{bookmark} {node|short}\n" """
        |> splitLines
        |> Array.map (fun s -> s.Split(' ').[0])

    let getCurrentCommitId() =
        (hg' "id -i").Trim()

    let getCurrentBranch() =
        (hg' "branch").Trim()

    let commitIdForBookmark (b: string) =
        hg' """bookmark -T "{bookmark} {node|short}\n" """
        |> splitLines
        |> Array.pick (fun s ->
            let a = s.Split(' ')
            if a.[0] = b then Some a.[1] else None
        )

    let branchExists (b: string) =
        hg' """branches -T "{branch}\n" """
        |> splitLines
        |> Array.contains b

    let isAncestorOfCurrent (ref: string) =
        let currentRef = getCurrentCommitId()
        let o = hg' """log --rev "ancestors(.) and %s" """ ref
        not (String.IsNullOrWhiteSpace o)

module VC =
    let isGit = Directory.Exists ".git"

    let getTags() =
        if isGit then
            let ok, out, err = Git.CommandHelper.runGitCommand "." "tag --merged"
            out.ToArray()
        else
            Hg.getBookmarks()

    let getCurrentCommitId() =
        if isGit then
            Git.Information.getCurrentSHA1 "."
        else
            Hg.getCurrentCommitId()

    let commitIdForTag tag =
        if isGit then
            Git.Branches.getSHA1 "." ("refs/tags/" + tag)
        else Hg.commitIdForBookmark tag

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
                traceImportant "Warning: no latest tag found"
                Paket.SemVer.Zero, None
            | a ->
                let v, t = Array.maxBy fst a
                v, t
        with e ->
            traceImportant (sprintf "Warning: getting version from latest tag: %s" e.Message)
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
                if head = tagged then
                    lastVersion.Patch
                else
                    lastVersion.Patch + 1u
        with e ->
            traceImportant (sprintf "Warning: computing patch version: %s" e.Message)
            0u
    let build =
        match jenkinsBuildNumber with
        | null | "" -> string (int64 lastVersion.Build + 1L)
        | b -> b
    Printf.ksprintf (fun v ->
        tracefn "==== Building project v%s ===="  v
        Paket.SemVer.Parse v
    ) "%i.%i.%i.%s%s" baseVersion.Major baseVersion.Minor patch build
        (match baseVersion.PreRelease with Some r -> "-" + r.Origin | None -> "")

type BuildMode =
    | Debug
    | Release

    override this.ToString() =
        match this with
        | Debug -> "Debug"
        | Release -> "Release"

type BuildAction =
    | Projects of seq<string>
    | Custom of (BuildMode -> unit)

    static member Solution s =
        BuildAction.Projects (!!s)

type Args =
    {
        Version : Paket.SemVerInfo
        BuildAction : BuildAction
        Attributes : seq<Attribute>
        StrongName : bool
        BaseRef : string
        WorkBranch : option<string>
        MergeMaster : bool
        PushRemote : string
    }

type WSTargets =
    {
        BuildDebug : string
        Publish : string
        CommitPublish : string
        ComputedVersion : Paket.SemVerInfo
    }

    member this.AddPrebuild s =
        "WS-Clean" ?=> s ==> "WS-BuildDebug"
        "WS-Update" ?=> s ==> "WS-BuildRelease"
        ()

let verbose = EnvironmentHelper.getEnvironmentVarAsBoolOrDefault "verbose" false
let msbuildVerbosity = if verbose then MSBuildVerbosity.Normal else MSBuildVerbosity.Minimal
let dotnetArgs = if verbose then [ "-v"; "n" ] else []
MSBuildDefaults <- { MSBuildDefaults with Verbosity = Some msbuildVerbosity }

let MakeTargets (args: Args) =

    let dirtyDirs =
        !! "/**/bin"
        ++ "/**/obj/Debug"
        ++ "/**/obj/Release"
        ++ "build"

    Target "WS-Clean" <| fun () ->
        DeleteDirs dirtyDirs

    Target "WS-Update" <| fun () ->
        let needsUpdate =
            mainGroup.Packages
            |> Seq.exists (fun { Name = Paket.Domain.PackageName(pkg, _) } ->
                pkg.Contains "WebSharper" || pkg.Contains "Zafir")
        if needsUpdate then shell ".paket/paket.exe" "update"

    Target "WS-GenAssemblyInfo" <| fun () ->
        CreateFSharpAssemblyInfo ("build" </> "AssemblyInfo.fs") [
                yield Attribute.Version (sprintf "%i.%i.0.0" args.Version.Major args.Version.Minor)
                yield Attribute.FileVersion (sprintf "%i.%i.%i.%s" args.Version.Major args.Version.Minor args.Version.Patch args.Version.Build)
                yield! args.Attributes
                match environVarOrNone "INTELLIFACTORY" with
                | None -> ()
                | Some p ->
                    yield Attribute.Company "IntelliFactory"
                    if args.StrongName then
                        yield Attribute.KeyFile (p </> "keys" </> "IntelliFactory.snk")
            ]

    let build mode =
        match args.BuildAction with
        | BuildAction.Projects files ->
            let f =
                match mode with
                | BuildMode.Debug -> MSBuildDebug
                | BuildMode.Release -> MSBuildRelease
            files
            |> f "" "Build"
            |> Log "AppBuild-Output: "
        | Custom f -> f mode

    Target "WS-BuildDebug" <| fun () ->
        build BuildMode.Debug

    Target "WS-BuildRelease" <| fun () ->
        build BuildMode.Release

    Target "WS-Package" <| fun () ->
        Paket.Pack <| fun p ->
            { p with
                OutputPath = "build"
                Version = args.Version.AsString
            }

    Target "WS-Checkout" <| fun () ->
        match args.WorkBranch with
        | None -> ()
        | Some branch ->
            if VC.isGit then
                try git "checkout -f %s" branch
                with e ->
                    try git "checkout -f -b %s" branch
                    with _ -> raise e
                if args.MergeMaster then
                    git "merge -Xtheirs --no-ff --no-commit %s" args.BaseRef
            else
                if Hg.branchExists branch
                then hg "update -C %s" branch
                else hg "branch %s" branch
                if args.MergeMaster && not (Hg.isAncestorOfCurrent args.BaseRef) then
                    hg "merge --tool internal:other %s" args.BaseRef
            Directory.CreateDirectory("build")
            File.WriteAllText("build/buildFromRef", args.BaseRef)

    Target "WS-Commit" <| fun () ->
        let tag = "v" + args.Version.AsString
        if VC.isGit then
            git "add ."
            git "commit --allow-empty -m \"[CI] %s\"" tag
            git "tag %s" tag
            git "push %s %s" args.PushRemote (Git.Information.getBranchName ".")
            git "push %s %s" args.PushRemote tag
        else
            hg "add ."
            hg "commit -m \"[CI] %s\"" tag
            hg "bookmark -i %s" tag
            hg "push --new-branch -b %s %s" (Hg.getCurrentBranch()) args.PushRemote

    Target "WS-Publish" <| fun () ->
        match environVarOrNone "NugetPublishUrl", environVarOrNone "NugetApiKey" with
        | Some nugetPublishUrl, Some nugetApiKey ->
            tracefn "[NUGET] Publishing to %s" nugetPublishUrl 
            Paket.Push <| fun p ->
                { p with
                    PublishUrl = nugetPublishUrl
                    ApiKey = nugetApiKey
                    WorkingDir = "build"
                }
        | _ -> traceError "[NUGET] Not publishing: NugetPublishUrl and/or NugetApiKey are not set"

    Target "WS-CommitPublish" DoNothing

    "WS-Clean"
        ==> "WS-GenAssemblyInfo"
        ?=> "WS-BuildDebug"

    "WS-Clean"
        ==> "WS-Update"
        ==> "WS-GenAssemblyInfo"
        ==> "WS-BuildRelease"
        ==> "WS-Package"
        ==> "WS-Publish"
        
    "WS-Package"    
        ==> "WS-Commit"
        ?=> "WS-Publish"
        ==> "WS-CommitPublish"

    "WS-Package"
        ==> "WS-Publish"

    "WS-Clean"
        ==> "WS-Checkout"

    {
        BuildDebug = "WS-BuildDebug"
        Publish = "WS-Publish"
        CommitPublish = "WS-CommitPublish"
        ComputedVersion = args.Version
    }

type WSTargets with

    static member Default (version: Paket.SemVerInfo) =
        let buildBranch = environVarOrNone "BuildBranch"
        let baseRef =
            match environVarOrNone "BuildFromRef" with
            | Some r -> r
            | None -> VC.getCurrentCommitId()
        let version =
            match buildBranch, version.PreRelease with
            | None, _ -> version
            | Some b, Some p when b = p.Origin -> version
            | Some b, _ -> Paket.SemVer.Parse (version.AsString + "-" + b)
        {
            Version = version
            BuildAction = BuildAction.Solution "*.sln"
            Attributes = Seq.empty
            StrongName = false
            BaseRef = baseRef
            WorkBranch = buildBranch
            MergeMaster = buildBranch = Some "staging"
            PushRemote =
                environVarOrDefault "PushRemote"
                    (if VC.isGit then "origin" else "default")
        }

    static member Default () =
        WSTargets.Default (ComputeVersion None)
