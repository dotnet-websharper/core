/// Helpers for FAKE scripts for the standard WebSharper extensions.
/// Check the sources for usage.
(**
Uses the following environment variables (some of which are overridden by WSTargets options):
  * BUILD_NUMBER: the build number to use (last component of the version).
    Automatically set by Jenkins.
  * PushOnlyAllowedChanges: whether to check that we're only committing paket.lock.
    Default: true
  * PushRemote: the name of the git remote to push to
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
#I "../packages/FAKE/tools"
#I "../../../../packages/FAKE/tools"
#r "FakeLib"
#I "../.paket"
#I "../../../../.paket"
#r "paket.exe"
#nowarn "49"

open System
open System.Text.RegularExpressions
open Fake
open Fake.AssemblyInfoFile

let private depsFile = Paket.DependenciesFile.ReadFromFile "./paket.dependencies"

let private mainGroup = depsFile.GetGroup(Paket.Domain.GroupName "Main")

let GetSemVerOf pkgName =
    match Paket.NuGet.GetVersions true None "." (mainGroup.Sources, Paket.Domain.PackageName pkgName)
        |> Async.RunSynchronously
        |> List.map fst with
    | [] -> None
    | l -> Some (List.max l)

let ComputeVersion (baseVersion: option<Paket.SemVerInfo>) =
    let lastVersion, tag =
        try
            let ok, out, err = Git.CommandHelper.runGitCommand "." "tag --merged"
            let re = Regex("""^(?:[a-zA-Z]+[-.]?)?([0-9]+(?:\.[0-9]+){0,3})(?:-.*)?$""")
            match out.ToArray()
                |> Array.choose (fun tag ->
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
                let head = Git.Information.getCurrentSHA1 "."
                let tagged =
                    match tag with
                    | Some tag -> Git.Branches.getSHA1 "." ("refs/tags/" + tag)
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
    | Solution of string
    | Custom of (BuildMode -> unit)

type Args =
    {
        Version : Paket.SemVerInfo
        BuildAction : BuildAction
        Attributes : seq<Attribute>
        StrongName : bool
        BaseBranch : string
        PushByMergingOnto : option<string>
        PushOnlyAllowedChanges : bool
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

let MakeTargets (args: Args) =

    let dirtyDirs =
        !! "/**/bin"
        ++ "/**/obj"
        ++ "build"

    Target "WS-Clean" <| fun () ->
        DeleteDirs dirtyDirs

    Target "WS-Update" <| fun () ->
        for { Name = Paket.Domain.PackageName(pkg, _) } in mainGroup.Packages do
            if pkg.Contains "WebSharper" || pkg.Contains "Zafir" then
                Paket.UpdateProcess.UpdatePackage("./paket.dependencies", mainGroup.Name,
                    Paket.Domain.PackageName pkg,
                    None, Paket.UpdaterOptions.Default)

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
        | BuildAction.Solution file ->
            let f =
                match mode with
                | BuildMode.Debug -> MSBuildDebug
                | BuildMode.Release -> MSBuildRelease
            !! file
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
        match args.PushByMergingOnto with
        | None -> ()
        | Some branch ->
            try Git.Branches.checkoutBranch "." branch
            with _ -> Git.Branches.checkoutNewBranch "." args.BaseBranch branch
            Git.Merge.merge "." "" args.BaseBranch

    Target "WS-Commit" <| fun () ->
        let changes =
            Git.FileStatus.getChangedFilesInWorkingCopy "." "HEAD"
            |> Seq.map snd
            |> Set
        let allowedChanges = Set ["paket.lock"]
        let badChanges = changes - allowedChanges
        if args.PushOnlyAllowedChanges && not (Set.isEmpty badChanges) then
            let msg =
                "[GIT] Not committing because these files have changed:\n    * "
                + String.concat "\n    * " badChanges
            raise (BuildException(msg, []))
        else
            let tag = "v" + args.Version.AsString
            if Set.isEmpty changes then
                trace "[GIT] OK -- no changes"
            else
                match args.PushByMergingOnto with
                | None ->
                    tracefn "[GIT] OK -- committing %s" (String.concat ", " changes)
                    Git.Staging.StageAll "."
                    Git.Commit.Commit "." ("[CI] " + tag)
                | Some branch ->
                    tracefn "[GIT] OK -- merging onto %s: %s" branch (String.concat ", " changes)
                    Git.Staging.StageAll "."
                    Git.Commit.Commit "." ("[CI] " + tag)
                Git.Branches.pushBranch "." args.PushRemote (Git.Information.getBranchName ".")
                    
            Git.CommandHelper.directRunGitCommand "." ("tag " + tag) |> ignore
            Git.Branches.pushTag "." args.PushRemote tag

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
        ?=> "WS-Update"

    "WS-Checkout"
        ==> "WS-Commit"

    {
        BuildDebug = "WS-BuildDebug"
        Publish = "WS-Publish"
        CommitPublish = "WS-CommitPublish"
        ComputedVersion = args.Version
    }

type WSTargets with

    static member Default (version) =
        {
            Version = version
            BuildAction = BuildAction.Solution "*.sln"
            Attributes = Seq.empty
            StrongName = false
            BaseBranch = Git.Information.getBranchName "."
            PushByMergingOnto = Some "ci"
            PushOnlyAllowedChanges = environVarOrDefault "PushOnlyAllowedChanges" "true" |> bool.Parse
            PushRemote = environVarOrDefault "PushRemote" "origin"
        }

    static member Default () =
        WSTargets.Default (ComputeVersion None)
