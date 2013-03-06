#r @"packages/FAKE.1.74.127.0/tools/FakeLib.dll"
#r "System.Xml"
#r "System.Xml.Linq"

open System
open System.IO
open System.Net
open System.Xml
open System.Xml.Linq
open Fake

let ( +/ ) a b = Path.Combine(a, b)
let T n f = Target n f; n
let baseDir = __SOURCE_DIRECTORY__

module Config =

    let private doc = XDocument.Load(baseDir +/ "Config.xml")

    let private setting name =
        let v =
            let n = XName.Get(name)
            doc.Descendants()
            |> Seq.find (fun x -> x.Name = n)
        v.Value

    let AssemblyVersion = Version(setting "AssemblyVersion")
    let FileVersion = Version(setting "FileVersion")
    let PackageVersion = setting "PackageVersion"

/// Infers the current Mercurial revision from the `.hg` folder.
let InferTag () =
    let bytes = File.ReadAllBytes(baseDir +/ ".hg" +/ "dirstate")
    Array.sub bytes 0 20
    |> Array.map (fun b -> String.Format("{0:x2}", b))
    |> String.concat ""

[<AutoOpen>]
module Tagging =
    type private A = AssemblyInfoFile.Attribute

    let PrepareAssemblyInfo =
        T "PrepareAssemblyInfo" (fun () ->
            let tag = InferTag ()
            let buildDir = baseDir +/ ".build"
            ensureDirectory buildDir
            let info = buildDir +/ "AssemblyInfo.fs"
            let desc =
                String.Format("This file is part of WebSharper. See \
                    the source code at <http://bitbucket.org/IntelliFactory/websharper>. \
                    Mercurial tag: {0}. Build date: {1}", tag, System.DateTimeOffset.UtcNow)
            AssemblyInfoFile.CreateFSharpAssemblyInfo info [
                A.Company "IntelliFactory"
                A.Copyright (String.Format("(c) {0} IntelliFactory", DateTime.Now.Year))
                A.FileVersion (string Config.FileVersion)
                A.Description desc
                A.Product "WebSharper"
                A.Version (string Config.AssemblyVersion)
            ]
        )

let AllProjects =
    !+ (baseDir +/ "**" +/ "*.csproj")
        ++ (baseDir +/ "**" +/ "*.fsproj")
            |> Scan

let DownloadDependencies =
    T "DownloadDependencies" <| fun () ->
        ensureDirectory "Lib"
        let baseUrl =
            "http://bitbucket.org/IntelliFactory/websharper/downloads/"
        let download url path =
            if not (fileExists path) then
                tracefn "Downloading: %s" url
                use client = new WebClient()
                client.DownloadFile(url, path)
                |> ignore
        let downloadFile (filename: string) =
            let url = baseUrl + filename
            let path = baseDir +/ "Lib" +/ filename
            download url path
        downloadFile "IntelliFactory.Xml.dll"

let BuildCompiler =
    T "BuildCompiler" <| fun () ->
        MSBuildRelease "" "Build" [baseDir +/ "WebSharper" +/ "WebSharper.fsproj"] |> ignore

let PrepareTargets =
    T "PrepareTargets" <| fun () ->
        let bDir = baseDir +/ ".build"
        ensureDirectory bDir
        let template = baseDir +/ "WebSharper.targets.template"
        let t =
            File.ReadAllText(template)
                .Replace("%PackageVersion%", Config.PackageVersion)
        let out = bDir +/ "WebSharper.targets"
        File.WriteAllText(out, t)

let BuildNuGet =
    T "BuildNuGet" <| fun () ->
        let bDir = baseDir +/ ".build"
        ensureDirectory bDir
        let nuSpec = bDir +/ "WebSharper.nuspec"
        File.WriteAllText(nuSpec,
            File.ReadAllText(baseDir +/ "WebSharper.nuspec.template")
                .Replace("%PackageVersion%", Config.PackageVersion)
                .Replace("%Year%", string DateTime.Now.Year))
        let rDir = bDir +/ "root"
        ensureDirectory rDir
        do
            let prefix = baseDir +/ "*" +/ "bin" +/ "Release"
            (!+ (prefix +/ "*.dll")
                ++ (prefix +/ "*.xml")
                ++ (prefix +/ "*.exe")
                ++ (prefix +/ "*.exe.config"))
            |> Scan
            |> Seq.filter (fun x ->
                [
                    "generator.exe"
                    "generator.exe.config"
                    "mscorlib.dll"
                    "fsharp.core.dll"
                    "fsharp.core.xml"
                    "system.dll"
                    "system.core.dll"
                    "system.web.dll"
                    "tests.dll"
                    "tests.xml"
                    "test.exe"
                    "websharper.exe"
                    "websharper.exe.xml"
                ]
                |> List.forall (fun n -> not (x.ToLower().EndsWith n)))
            |> Copy rDir
        for (outVer, inVer) in [("v2.0", "net20"); ("v4.0", "net40")] do
            let d = rDir +/ outVer
            ensureDirectory d
            !! (baseDir +/ "packages" +/ "FSharp.Core.*" +/ "lib" +/ inVer +/ "*.*")
            |> Copy d
        do
            !! (baseDir +/ "build" +/ "*.targets")
            |> Copy rDir
        let nuGetExe = baseDir +/ ".nuget" +/ "NuGet.exe"
        nuSpec
        |> NuGetPack (fun p ->
            { p with
                OutputPath = bDir
                ToolPath = nuGetExe
                Version = Config.PackageVersion
                WorkingDir = bDir
            })

let BuildProjects =
    T "BuildProjects" <| fun () ->
        MSBuildRelease "" "Build" AllProjects |> ignore

let Clean =
    T "Clean" <| fun () ->
        MSBuildRelease "" "Clean" AllProjects |> ignore
        DeleteDir (baseDir +/ ".build")
Clean
    ==> DownloadDependencies
    ==> PrepareAssemblyInfo
    ==> BuildCompiler
    ==> BuildProjects
    ==> PrepareTargets
    ==> BuildNuGet

RunTargetOrDefault BuildNuGet
