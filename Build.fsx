#r "packages/FAKE.2.1.158-alpha/tools/FakeLib.dll"
#r "packages/IntelliFactory.Build.0.0.6/lib/net40/IntelliFactory.Build.dll"
#r "packages/DotNetZip.1.9.1.8/lib/net20/Ionic.Zip.dll"

open System
open System.IO
open System.Net
open Fake
open Ionic.Zip
module B = IntelliFactory.Build.CommonBuildSetup
module NG = IntelliFactory.Build.NuGet
module VP = IntelliFactory.Build.VsixPackages
module VST = IntelliFactory.Build.VSTemplates
module VX = IntelliFactory.Build.VsixExtensions
module X = IntelliFactory.Build.XmlGenerator

let ( +/ ) a b = Path.Combine(a, b)
let RootDir = __SOURCE_DIRECTORY__
let PackagesDir = RootDir +/ "packages"
let ToolsDir = PackagesDir +/ "tools"
let T x f = Target x f; x

module Config =
    let Company = "IntelliFactory"
    let Description = "F#-to-JavaScript compiler and web application framework"
    let Icon = VST.Icon.FromFile (RootDir +/ "WebSharper.png")
    let LicenseUrl = "http://websharper.com/licensing"
    let PackageId = "WebSharper"
    let Tags = ["Web"; "JavaScript"; "F#"]
    let AssemblyVersion = Version "2.5.0.0"
    let AssemblyFileVersion = Version "2.5.4.0"
    let Version = "2.5.4-alpha"
    let Website = "http://bitbucket.org/IntelliFactory/websharper"

let Metadata =
    let m = B.Metadata.Create()
    m.Author <- Some Config.Company
    m.AssemblyVersion <- Some Config.AssemblyVersion
    m.FileVersion <- Some Config.AssemblyFileVersion
    m.Description <- Some Config.Description
    m.Product <- Some Config.PackageId
    m.Website <- Some Config.Website
    m

let Frameworks = [B.Net35; B.Net40]

let CompilerSolution =
    B.Solution.Standard __SOURCE_DIRECTORY__ Metadata [
        B.Project.FSharp "IntelliFactory.JavaScript" Frameworks
        B.Project.FSharp "IntelliFactory.JavaScript.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Core" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Compiler" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.InterfaceGenerator" Frameworks
        B.Project.FSharp "WebSharper" Frameworks
    ]

let MainSolution =
    B.Solution.Standard __SOURCE_DIRECTORY__ Metadata [
        B.Project.FSharp "IntelliFactory.WebSharper" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Dom" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.JQuery" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Collections" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Collections.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Control" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Ecma" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Testing" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Web" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Web.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.Reactive" Frameworks
        B.Project.FSharp "IntelliFactory.Formlet" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Formlet" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Formlet.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.Html" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Html" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Html5" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Html5.Tests" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Sitelets" Frameworks
        B.Project.FSharp "IntelliFactory.WebSharper.Sitelets.Tests" Frameworks
        B.Project.FSharp "Website" [B.Net40]
        fun s ->
            let p = B.Project.CSharp "Web" [B.Net40] s
            match B.GetOutDir() with
            | Some outDir -> { p with Properties = Map ["OutDir", outDir] }
            | None -> p
    ]

let BuildMain = T "BuildMain" MainSolution.Build
let CleanMain = T "CleanMain" MainSolution.Clean

let PrepareTools =
    T "PrepareTools" <| fun () ->
        Fake.FileSystemHelper.ensureDirectory ToolsDir
        !+ (PackagesDir +/ "AjaxMin*" +/ "tools" +/ "net40" +/ "*.*")
        |> Scan
        |> Copy ToolsDir

let CleanTools =
    T "CleanTools" <| fun () ->
        Directory.Delete(ToolsDir, true)

let BuildCompiler = T "BuildCompiler" CompilerSolution.Build
let CleanCompiler = T "CleanCompiler" CompilerSolution.Clean

let Build = T "Build" ignore
let Clean = T "Clean" ignore

let DotBuildDir = RootDir +/ ".build"

let BuildWebSharperTargetsXml () =
    let ns = "http://schemas.microsoft.com/developer/msbuild/2003"
    let e n = X.Element.Create(n, ns)
    let ( -- ) (a: X.Element) (b: string) = X.Element.WithText b a
    e "Project" - [
        e "PropertyGroup" - [
            yield e "WebSharperVersion" -- Config.Version
            let variants =
                [
                    "$(MSBuildProjectDirectory)/../packages"
                    "$(MSBuildProjectDirectory)/../../packages"
                    "$(MSBuildProjectDirectory)/../../../packages"
                ]
            for variant in variants do
                yield
                    e "PackagesFolder"
                        + [
                            "Condition",
                                sprintf " '$(PackagesFolder)' == '' AND Exists('%s')" variant
                        ]
                    -- variant
            let homeVariants =
                [
                    "v2.0", "net35"
                    "v3.0", "net35"
                    "v3.5", "net35"
                    "v4.0", "net40"
                    "v4.5", "net40"
                ]
            for (tfv, home) in homeVariants do
                yield
                    e "WebSharperHome"
                        + [
                            "Condition",
                                sprintf " '$(TargetFrameworkVersion)' == '%s' " tfv
                        ]
                        -- sprintf "$(PackagesFolder)/WebSharper.$(WebSharperVersion)/tools/%s" home
        ]
        e "Import" + ["Project", "$(WebSharperHome)/WebSharper.targets"]
    ]

let BuildNuSpecXml () =
    let e n = X.Element.Create n
    let ( -- ) (a: X.Element) (b: string) = X.Element.WithText b a
    e "package" - [
        e "metadata" - [
            e "id" -- Config.PackageId
            e "version" -- Config.Version
            e "authors"-- Config.Company
            e "owners"-- Config.Company
            e "licenseUrl" -- Config.LicenseUrl
            e "projectUrl"-- Config.Website
            e "requireLicenseAcceptance" -- "false"
            e "description" -- Config.Description
            e "copyright" -- sprintf "Copyright (c) %O %s" DateTime.Now.Year Config.Company
            e "tags" -- String.concat " " Config.Tags
        ]
        e "files" - [
            e "file" + ["src", "WebSharper.targets"; "target", "content"]
            e "file" + ["src", @"root\net35\*.*"; "target", @"tools\net35"]
            e "file" + ["src", @"root\net40\*.*"; "target", @"tools\net40"]
        ]
    ]

let BuildWebSharperTargets =
    T "BuildWebSharperTargets" <| fun () ->
        ensureDirectory DotBuildDir
        let targets = DotBuildDir +/ "WebSharper.targets"
        X.WriteFile targets (BuildWebSharperTargetsXml ())

let NuGetPackageFile =
    DotBuildDir +/ sprintf "%s.%s.nupkg" Config.PackageId Config.Version

let BuildNuGet =
    T "BuildNuGet" <| fun () ->
        ensureDirectory DotBuildDir
        let nuSpec = DotBuildDir +/ "WebSharper.nuspec"
        X.WriteFile nuSpec (BuildNuSpecXml ())
        for f in [B.Net35; B.Net40] do
            let rDir = DotBuildDir +/ "root" +/ f.GetNuGetLiteral()
            if Directory.Exists rDir then
                Directory.Delete(rDir, true)
            ensureDirectory rDir
            let config = "Release-" + f.GetMSBuildLiteral()
            let prefix = RootDir +/ "*" +/ "bin" +/ config
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
                    "tests.exe"
                ]
                |> List.forall (fun n -> not (x.ToLower().EndsWith n)))
            |> Seq.distinct
            |> Copy rDir
            do
                !! (RootDir +/ "build" +/ "DeployedTargets" +/ "*.targets")
                |> Copy rDir
        let nuGetExe = RootDir +/ ".nuget" +/ "NuGet.exe"
        nuSpec
        |> NuGetPack (fun p ->
            { p with
                OutputPath = DotBuildDir
                ToolPath = nuGetExe
                Version = Config.Version
                WorkingDir = DotBuildDir
            })

let DownloadDependencies =
    T "DownloadDependencies" <| fun () ->
        let libDir = RootDir +/ "Lib"
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
            let path = RootDir +/ "Lib" +/ filename
            download url path
        downloadFile "IntelliFactory.Xml.dll"

module Templates =

    let VsixFileName = sprintf "%s-%s.vsix" Config.PackageId Config.Version
    let VsixFile = DotBuildDir +/ VsixFileName

    let Category = [sprintf "%s-%s" Config.PackageId Config.Version]
    let Guid = Guid("371cf828-9e17-41cb-b014-496f3e9e7171")

    let Identity =
        VP.Identity.Create (sprintf "%s-%s" Config.PackageId Config.Version) Guid

    let NuGetPackages =
        lazy
        VST.NuGetPackages.Create Identity [
            NG.Package.FromFile NuGetPackageFile
        ]

    let WebSharperLibrary =
        lazy
        let dir = RootDir +/ "templates" +/ "library"
        let meta =
            let m =
                VST.TemplateData.Create
                    VST.FSharp
                    (sprintf "WebSharper %s Library" Config.Version)
                    "An F# library capable of containing JavaScript-compiled code"
                    Config.Icon
            m.DefaultName <- Some "Library"
            m
        let main =
            let i = VST.ProjectItem.FromTextFile (dir +/ "Main.fs")
            i.ReplaceParameters <- true
            i
        let project =
            let p =
                VST.Project.FromFile (dir +/ "Library.fsproj") [
                    VST.NestedProjectItem main
                ]
            p.ReplaceParameters <- true
            p
        let projectTemplate =
            let t = VST.ProjectTemplate.Create meta project
            t.NuGetPackages <- Some NuGetPackages.Value
            t
        projectTemplate

    let WebSharperExtension =
        lazy
        let id =
            let id =
                VX.Identifier.Create
                    Config.Company
                    Identity
                    Config.PackageId
                    Config.Description
            id.Version <- Config.AssemblyFileVersion
            id.Products <-
                [
                    VX.VSProduct.Create "11.0" [VX.Premium; VX.Pro; VX.Ultimate]
                    |> VX.VS
                ]
            id
        let vsix =
            VX.Vsix.Create id [
                VX.VsixContent.ProjectTemplate Category WebSharperLibrary.Value
            ]
        VX.VsixFile.Create VsixFileName vsix

    let BuildExtension =
        T "BuildExtension" <| fun () ->
            WebSharperExtension.Value.WriteToDirectory DotBuildDir

let BuildConfigFile =
    T "BuildConfigFile" <| fun () ->
        let content =
            use w = new StringWriter()
            w.WriteLine("module Website.Config")
            let var (name: string) (value: string) : unit =
                fprintfn w @"let %s = @""%s""" name (value.Replace(@"""", @""""""))
            IntelliFactory.Build.Mercurial.InferTag RootDir
            |> Option.iter (var "Tag")
            var "PackageId" Config.PackageId
            var "Version" Config.Version
            var "AssemblyVersion" (string Config.AssemblyVersion)
            var "AssemblyFileVersion" (string Config.AssemblyFileVersion)
            var "Description" Config.Description
            var "Website" Config.Website
            w.ToString()
            |> IntelliFactory.Build.FileSystem.TextContent
        content.WriteFile(DotBuildDir +/ "Config.fs")

let ZipPackageFile =
    RootDir +/ "Web" +/ "downloads" +/ sprintf "%s-%s.zip" Config.PackageId Config.Version

let BuildZipPackage =
    T "BuildZipPackage" <| fun () ->
        ensureDirectory (Path.GetDirectoryName ZipPackageFile)
        let zip = new ZipFile()
        let addFile path =
            zip.AddEntry(Path.GetFileName path, File.ReadAllBytes path)
            |> ignore
        addFile NuGetPackageFile
        addFile (RootDir +/ "LICENSE.txt")
        addFile Templates.VsixFile
        zip.Save ZipPackageFile

PrepareTools
    ==> BuildConfigFile
    ==> DownloadDependencies
    ==> BuildCompiler
    ==> BuildMain
    ==> BuildWebSharperTargets
    ==> BuildNuGet
    ==> Templates.BuildExtension
    ==> BuildZipPackage
    ==> Build

PrepareTools
    ==> CleanMain
    ==> CleanCompiler
    ==> CleanTools
    ==> Clean

RunTargetOrDefault "Build"
