#r @"../packages/FAKE.1.74.127.0/tools/FakeLib.dll"

open System
open System.IO
open System.Net
open Fake

module Config =
    let AssemblyVersion = Version("2.5.0.0")
    let FileVersion = Version("2.5.2.0")
    let PackageVersion = "2.5.2-alpha"
    let Repo = "http://bitbucket.org/IntelliFactory/websharper"
    let Product = "WebSharper"

let ( +/ ) a b = Path.Combine(a, b)
let T n f = Target n f; n
let baseDir = Path.GetDirectoryName(__SOURCE_DIRECTORY__)

/// Infers the current Mercurial revision from the `.hg` folder.
let InferTag () =
    let bytes = File.ReadAllBytes(baseDir +/ ".hg" +/ "dirstate")
    Array.sub bytes 0 20
    |> Array.map (fun b -> String.Format("{0:x2}", b))
    |> String.concat ""

let WriteIfNecessary (file: string) (text: string) =
    if not (File.Exists file) || File.ReadAllText file <> text then
        tracefn "Wrte: %s" text
        File.WriteAllText(file, text)

let CopyIfNecessary (input: string) (output: string) =
    if not (File.Exists output) || File.ReadAllText output <> File.ReadAllText input then
        tracefn "Copy: %s -> %s" input output
        File.Copy(input, output, true)

[<AutoOpen>]
module Tagging =
    type private A = AssemblyInfoFile.Attribute

    let PrepareAssemblyInfo =
        T "PrepareAssemblyInfo" (fun () ->
            let tag = InferTag ()
            let buildDir = baseDir +/ ".build"
            ensureDirectory buildDir
            let fsInfoTemp = buildDir +/ "AutoAssemblyInfo.tmp.fs"
            let csInfoTemp = buildDir +/ "AutoAssemblyInfo.tmp.cs"
            let fsInfo = buildDir +/ "AutoAssemblyInfo.fs"
            let csInfo = buildDir +/ "AutoAssemblyInfo.cs"
            let desc =
                String.Format("This file is part of WebSharper. See \
                    the source code at <{2}> \
                    Mercurial tag: {0}. Build date: {1}", tag, DateTimeOffset.UtcNow.Date, Config.Repo)
            let attrs =
                [
                    A.Company "IntelliFactory"
                    A.Copyright (String.Format("(c) {0} IntelliFactory", DateTime.Now.Year))
                    A.FileVersion (string Config.FileVersion)
                    A.Description desc
                    A.Product (String.Format("{1} (tag: {0})", tag, Config.Product))
                    A.Version (string Config.AssemblyVersion)
                ]
            AssemblyInfoFile.CreateCSharpAssemblyInfo csInfoTemp attrs
            AssemblyInfoFile.CreateFSharpAssemblyInfo fsInfoTemp attrs
            CopyIfNecessary csInfoTemp csInfo
            CopyIfNecessary fsInfoTemp fsInfo
        )

let AllProjects =
    !+ (baseDir +/ "**" +/ "*.csproj")
        ++ (baseDir +/ "**" +/ "*.fsproj")
            |> Scan

let DownloadDependencies =
    T "DownloadDependencies" <| fun () ->
        let libDir = baseDir +/ "Lib"
        ensureDirectory libDir
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
        do
            !+ (baseDir +/ "packages" +/ "*" +/ "lib" +/ "net40" +/ "*.dll")
            |> Scan
            |> Copy libDir
        downloadFile "IntelliFactory.Xml.dll"

type Framework =
    | V35
    | V40

let Frameworks = [V35; V40]

let Properties (f: Framework) (project: string) =
    [
        "Framework",
            match f with
            | V35 -> "v3.5"
            | V40 -> "v4.0"
    ]

let BuildCompiler =
    T "BuildCompiler" <| fun () ->
        for f in Frameworks do
            [baseDir +/ "WebSharper" +/ "WebSharper.fsproj"] 
            |> MSBuildWithProjectProperties "" "Build" (Properties f)
            |> ignore

let PrepareTargets =
    T "PrepareTargets" <| fun () ->
        let bDir = baseDir +/ ".build"
        let text =
            ensureDirectory bDir
            let template = baseDir +/ "WebSharper.targets.template"
            File.ReadAllText(template)
                .Replace("%PackageVersion%", Config.PackageVersion)
        let out = bDir +/ "WebSharper.targets"
        WriteIfNecessary out text

let BuildNuGet =
    T "BuildNuGet" <| fun () ->
        let bDir = baseDir +/ ".build"
        ensureDirectory bDir
        let nuSpec = bDir +/ "WebSharper.nuspec"
        File.WriteAllText(nuSpec,
            File.ReadAllText(baseDir +/ "WebSharper.nuspec.template")
                .Replace("%PackageVersion%", Config.PackageVersion)
                .Replace("%Year%", string DateTime.Now.Year))
        for f in Frameworks do
            let rDir =
                match f with
                | V35 -> bDir +/ "root" +/ "net35"
                | V40 -> bDir +/ "root" +/ "net40"
            ensureDirectory rDir
            let config =
                match f with
                | V35 -> "Release-v3.5"
                | V40 -> "Release-v4.0"
            let prefix = baseDir +/ "*" +/ "bin" +/ config
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
                    "system.dll"
                    "system.core.dll"
                    "system.numerics.dll"
                    "system.web.dll"
                    "tests.dll"
                    "tests.xml"
                    "test.exe"
                ]
                |> List.forall (fun n -> not (x.ToLower().EndsWith n)))
            |> Seq.distinct
            |> Copy rDir
            do
                !! (baseDir +/ "build" +/ "IntelliFactory.*.targets")
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
        for f in Frameworks do
            AllProjects
            |> MSBuildWithProjectProperties "" "Build" (Properties f)
            |> ignore

let Clean =
    T "Clean" <| fun () ->
        for f in Frameworks do
            AllProjects
            |> MSBuildWithProjectProperties "" "Clean" (Properties f)
            |> ignore
        DeleteDir (baseDir +/ ".build")

DownloadDependencies
    ==> PrepareAssemblyInfo
    ==> BuildCompiler
    ==> BuildProjects
    ==> PrepareTargets
    ==> BuildNuGet

RunTargetOrDefault BuildNuGet
