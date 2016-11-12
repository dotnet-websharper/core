/// Helpers for FAKE scripts for the standard WebSharper extensions.
/// Check the sources for usage.
(**
Uses the following environment variables:
  * BUILD_NUMBER: the build number to use (last component of the version).
    Automatically set by Jenkins.
  * CHECK_CHANGES: whether to check that we're only committing paket.lock. (default: true)
  * NUGET_API_KEY: the API key of the NuGet server.

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
#r "paket"
#nowarn "49"

open System
open Fake
open Fake.AssemblyInfoFile

let private depsFile = Paket.DependenciesFile.ReadFromFile "./paket.dependencies"

let private mainGroup = depsFile.GetGroup(Paket.Domain.GroupName "Main")

let GetSemVerOf pkgName =
    match Paket.NuGetV2.GetVersions true None "." (mainGroup.Sources, Paket.Domain.PackageName pkgName)
        |> List.map fst with
    | [] -> None
    | l -> Some (List.max l)

let ComputeVersion (baseVersion: option<Paket.SemVerInfo>) =
    let lastVersion, tag =
        try
            let ok, out, err = Git.CommandHelper.runGitCommand "." "tag -l v*.*.*"
            match out.ToArray()
                |> Array.choose (fun tag ->
                    try
                        let v =
                            if tag.StartsWith "v" then tag.[1..] else tag
                            |> Paket.SemVer.Parse
                        Some (v, Some tag)
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
        match environVar "BUILD_NUMBER" with
        | null | "" -> string (int64 lastVersion.Build + 1L)
        | b -> b
    Printf.ksprintf (fun v ->
        tracefn "==== Building project v%s ===="  v
        Paket.SemVer.Parse v
    ) "%i.%i.%i.%s%s" baseVersion.Major baseVersion.Minor patch build
        (match baseVersion.PreRelease with Some r -> "-" + r.Origin | None -> "")

type Level =
    | Public
    | Private
    | Premium
    | Custom of repo: string

type Args =
    {
        Version : Paket.SemVerInfo
        ProjectFiles : seq<string>
        Level : Level
        Attributes : seq<Attribute>
        StrongName : bool
    }

type WSTargets =
    {
        BuildDebug : string
        Publish : string
        ComputedVersion : Paket.SemVerInfo
    }

    member this.AddPrebuild s =
        "WS-Clean" ==> s ==> "WS-BuildDebug"
        "WS-Update" ?=> s ==> "WS-BuildRelease"
        ()

let MakeTargets (args: Args) =

    let dirtyDirs =
        !! "/**/bin"
        ++ "/**/obj"

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

    let build f =
        f "" "Build" args.ProjectFiles
        |> Log "AppBuild-Output: "

    Target "WS-BuildDebug" <| fun () ->
        build MSBuildDebug

    Target "WS-BuildRelease" <| fun () ->
        build MSBuildRelease

    Target "WS-Package" <| fun () ->
        Paket.Pack <| fun p ->
            { p with
                OutputPath = environVarOrDefault "NuGetPackageOutputPath" "build"
                Version = args.Version.AsString
            }

    Target "WS-Commit" <| fun () ->
        let changes =
            Git.FileStatus.getChangedFilesInWorkingCopy "." "HEAD"
            |> Seq.map snd
            |> Set
        let allowedChanges = Set ["paket.lock"]
        let badChanges = changes - allowedChanges
        let checkChanges = environVarOrDefault "CHECK_CHANGES" "true" |> bool.Parse
        if checkChanges && not (Set.isEmpty badChanges) then
            let msg =
                "[GIT] Not committing because these files have changed:\n    * "
                + String.concat "\n    * " badChanges
            raise (BuildException(msg, []))
        else
            let tag = "v" + args.Version.AsString
            if Set.isEmpty changes then
                trace "[GIT] OK -- no changes"
            else
                tracefn "[GIT] OK -- committing %s" (String.concat ", " changes)
                Git.Staging.StageAll "."
                Git.Commit.Commit "." ("[CI] " + tag)
            Git.CommandHelper.directRunGitCommand "." ("tag " + tag) |> ignore
            Git.Branches.push "."
            Git.Branches.pushTag "." "origin" tag

    Target "WS-Publish" <| fun () ->
        Paket.Push <| fun p ->
            { p with
                PublishUrl =
                    match args.Level with
                    | Public -> "https://staging.websharper.com"
                    | Private -> "https://nuget.websharper.com"
                    | Premium -> "https://premium.websharper.com"
                    | Custom r -> r
                ApiKey = environVar "NUGET_API_KEY"
            }

    "WS-Clean"
        ==> "WS-GenAssemblyInfo"
        ==> "WS-BuildDebug"

    "WS-Clean"
        ==> "WS-Update"
        ==> "WS-GenAssemblyInfo"
        ==> "WS-BuildRelease"
        ==> "WS-Package"
        ==> "WS-Commit"
        ==> "WS-Publish"

    {
        BuildDebug = "WS-BuildDebug"
        Publish = "WS-Publish"
        ComputedVersion = args.Version
    }

type WSTargets with

    static member Make(Level, ?Version, ?BaseVersion, ?ProjectFiles, ?Attributes, ?StrongName) =
        MakeTargets {
            Level = Level
            Version =
                match Version with
                | None -> ComputeVersion BaseVersion
                | Some v -> v
            ProjectFiles =
                match ProjectFiles with
                | Some p -> p
                | None ->
                    !! "/**/*.csproj"
                    ++ "/**/*.fsproj"
                    :> seq<string>
            Attributes = defaultArg Attributes Seq.empty
            StrongName = defaultArg StrongName false
        }
