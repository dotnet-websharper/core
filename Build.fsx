#r "packages/FAKE.1.74.131.0/tools/FakeLib.dll"
#r "packages/IntelliFactory.Build.0.0.5/lib/net40/IntelliFactory.Build.dll"


open Fake
open System
open System.IO
open System.Net
open Fake
module B = IntelliFactory.Build.CommonBuildSetup

let ( +/ ) a b = Path.Combine(a, b)
let RootDir = __SOURCE_DIRECTORY__
let PackagesDir = RootDir +/ "packages"
let ToolsDir = PackagesDir +/ "tools"
let T x f = Target x f; x

let Metadata =
    let m = B.Metadata.Create()
    m.Author <- Some "IntelliFactory"
    m.AssemblyVersion <- Some (Version "2.5.0.0")
    m.FileVersion <- Some (Version "2.5.4.0")
    m.Description <- Some "F#-to-JavaScript compiler and web application framework"
    m.Product <- Some "WebSharper"
    m.Website <- Some "http://bitbucket.org/IntelliFactory/websharper"
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
        ensureDirectory ToolsDir
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

PrepareTools ==> DownloadDependencies ==> BuildCompiler ==> BuildMain ==> Build
PrepareTools ==> CleanMain ==> CleanCompiler ==> CleanTools ==> Clean

match Environment.GetCommandLineArgs() with
| xs when xs.[xs.Length - 1] = Clean -> RunTargetOrDefault Clean
| _ -> RunTargetOrDefault Build

//let BuildNuGet =
//    T "BuildNuGet" <| fun () ->
//        let bDir = baseDir +/ ".build"
//        ensureDirectory bDir
//        let nuSpec = bDir +/ "WebSharper.nuspec"
//        File.WriteAllText(nuSpec,
//            File.ReadAllText(baseDir +/ "WebSharper.nuspec.template")
//                .Replace("%PackageVersion%", Config.PackageVersion)
//                .Replace("%Year%", string DateTime.Now.Year))
//        for f in Frameworks do
//            let rDir =
//                match f with
//                | V35 -> bDir +/ "root" +/ "net35"
//                | V40 -> bDir +/ "root" +/ "net40"
//            ensureDirectory rDir
//            let config =
//                match f with
//                | V35 -> "Release-v3.5"
//                | V40 -> "Release-v4.0"
//            let prefix = baseDir +/ "*" +/ "bin" +/ config
//            (!+ (prefix +/ "*.dll")
//                ++ (prefix +/ "*.xml")
//                ++ (prefix +/ "*.exe")
//                ++ (prefix +/ "*.exe.config"))
//            |> Scan
//            |> Seq.filter (fun x ->
//                [
//                    "generator.exe"
//                    "generator.exe.config"
//                    "mscorlib.dll"
//                    "system.dll"
//                    "system.core.dll"
//                    "system.numerics.dll"
//                    "system.web.dll"
//                    "tests.dll"
//                    "tests.xml"
//                    "test.exe"
//                ]
//                |> List.forall (fun n -> not (x.ToLower().EndsWith n)))
//            |> Seq.distinct
//            |> Copy rDir
//            do
//                !! (baseDir +/ "build" +/ "IntelliFactory.*.targets")
//                |> Copy rDir
//        let nuGetExe = baseDir +/ ".nuget" +/ "NuGet.exe"
//        nuSpec
//        |> NuGetPack (fun p ->
//            { p with
//                OutputPath = bDir
//                ToolPath = nuGetExe
//                Version = Config.PackageVersion
//                WorkingDir = bDir
//            })
