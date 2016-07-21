// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.Build

open System
open System.IO
open IntelliFactory.Build
open IntelliFactory.Core

module Main =

    let private root =
        Path.Combine(__SOURCE_DIRECTORY__, "../../..")
        |> Path.GetFullPath

    let private buildDir =
        Path.Combine(root, "build",
#if DEBUG
            "Debug"
#else
            "Release"
#endif
        )

    let private version =
        Config.PackageVersion + match Config.VersionSuffix with Some s -> "-" + s | _ -> ""    

    let private bt =
        BuildTool()
            .PackageId(Config.PackageId, version)
            .Configure(fun bt ->
                let outDir = BuildConfig.OutputDir.Find bt
                bt
                |> BuildConfig.RootDir.Custom root
                |> FSharpConfig.OtherFlags.Custom ["--optimize+"]
                |> Logs.Config.Custom(Logs.Default.Verbose().ToConsole()))
            .WithFramework(fun fw -> fw.Net40)

    let private searchDir (dir: string) =
        let dir =Path.GetFullPath(dir)
        Directory.EnumerateFiles(dir, "*.*", SearchOption.AllDirectories)
        |> Seq.map (fun p -> (p, p.Substring(dir.Length).Replace("\\", "/")))

    let private coreExports, fsExports, csExports, testingExports, compilerExports =
        let lib name =
            Seq.concat [
                Directory.EnumerateFiles(buildDir, name + ".dll")
                Directory.EnumerateFiles(buildDir, name + ".xml")
            ]

        let tools name =
            Directory.EnumerateFiles(buildDir, name + ".dll")
        
        Seq.concat [
            lib "WebSharper.Core.JavaScript"
            lib "WebSharper.Core"
            lib "WebSharper.InterfaceGenerator"
            lib "WebSharper.Sitelets"
            lib "WebSharper.Web"
            lib "WebSharper.Main"
            lib "WebSharper.Collections"
            lib "WebSharper.Control"
            lib "WebSharper.JavaScript"
            lib "WebSharper.JQuery"
        ] |> List.ofSeq
        ,
        Seq.concat [                        
            tools "WebSharper.Compiler"
            tools "WebSharper.Compiler.FSharp"
            tools "WebSharper.Sitelets.Offline"
            // shared with main package:
            tools "WebSharper.Core"
            tools "WebSharper.Core.JavaScript"
            tools "WebSharper.InterfaceGenerator"
            // foreign:
            tools "FSharp.Core"
            tools "Mono.Cecil"
            tools "Mono.Cecil.Mdb"
            tools "Mono.Cecil.Pdb"
            tools "FSharp.Compiler.Service"
            tools "System.Collections.Immutable"
            tools "System.Reflection.Metadata"
        ] |> List.ofSeq
        ,
        Seq.concat [
            tools "WebSharper.Compiler"
            tools "WebSharper.Compiler.CSharp"
            tools "WebSharper.MSBuild.CSharp"
            tools "WebSharper.CSharp.Analyzer"
            tools "WebSharper.Sitelets.Offline"
            // shared with main package:
            tools "WebSharper.Core"
            tools "WebSharper.Core.JavaScript"
            tools "WebSharper.InterfaceGenerator"
            // foreign:
            tools "FSharp.Core"
            tools "Mono.Cecil"
            tools "Mono.Cecil.Mdb"
            tools "Mono.Cecil.Pdb"
            tools "Microsoft.CodeAnalysis"
            tools "Microsoft.CodeAnalysis.CSharp"
            tools "System.Collections.Immutable"
            tools "System.Reflection.Metadata"
        ] |> List.ofSeq
        ,
        lib "WebSharper.Testing"
        ,
        Seq.concat [
            lib "WebSharper.Compiler"
            lib "WebSharper.Compiler.FSharp"
            lib "WebSharper.Compiler.CSharp"
            lib "WebSharper.Sitelets.Offline"
        ] |> List.ofSeq

    let private nuPkgs () =
        let nuPkg =
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.Description
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
                            Authors = [ Config.Company ]
                    })
        let fileAt src tgt =
            {
                new INuGetFile with
                    member x.Read() = File.OpenRead(Path.Combine(root, src)) :> _
                    member x.TargetPath = tgt
            }
        let fileLib src =
            sprintf "/lib/net40/%s" (Path.GetFileName src)
            |> fileAt src
        let fileLib45 src =
            sprintf "/lib/net45/%s" (Path.GetFileName src)
            |> fileAt src
        let fileTools src =
            sprintf "/tools/%s" (Path.GetFileName src)
            |> fileAt src

        let out f = Path.Combine(buildDir, f)
        let nuPkg = 
            nuPkg.AddNuGetExportingProject {
                new INuGetExportingProject with
                    member p.NuGetFiles =
                        seq {
                            for src in coreExports do
                                yield fileLib src
                            for (src, tgt) in searchDir (Path.Combine(root, "docs")) do
                                yield fileAt src ("/docs" + tgt)
                            yield fileAt (Path.Combine(root, "src", "htmllib", "tags.csv")) ("/tools/net40/tags.csv")
                        }
            }
        let v = Version(nuPkg.GetComputedVersion().Split('-').[0])
        let fsharpNuPkg =
            let bt =
                bt.PackageId(Config.FSharpPackageId, version)
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.FSharpDescription
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
                            Authors = [ Config.Company ]
                    })
                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
//                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.FSharp.Readme.txt")) "content/Zafir.FSharp.Readme.txt"
                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.FSharp.targets")) ("build/" + Config.FSharpPackageId + ".targets")
                                yield fileTools (out "WsFsc.exe")
                                yield fileTools (out "WsFsc.exe.config")
                                let fscore = Path.Combine(root, "packages", "FSharp.Core.4.0.0.1", "lib", "net40")
                                yield fileTools (Path.Combine(fscore, "FSharp.Core.optdata"))
                                yield fileTools (Path.Combine(fscore, "FSharp.Core.sigdata"))
                                for src in fsExports do
                                    yield fileTools src
                            }
                }
        let csharpNuPkg =
            let bt =
                bt.PackageId(Config.CSharpPackageId, version)
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.CSharpDescription
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
                            Authors = [ Config.Company ]
                    })
                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
//                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.CSharp.Readme.txt")) "content/Zafir.CSharp.Readme.txt"
                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.CSharp.targets")) ("build/" + Config.CSharpPackageId + ".targets")
                                yield fileTools (out "ZafirCs.exe")
                                yield fileTools (out "ZafirCs.exe.config")
                                let fscore = Path.Combine(root, "packages", "FSharp.Core.4.0.0.1", "lib", "net40")
                                yield fileTools (Path.Combine(fscore, "FSharp.Core.optdata"))
                                yield fileTools (Path.Combine(fscore, "FSharp.Core.sigdata"))
                                for src in csExports do
                                    yield fileTools src
                                yield fileTools (Path.Combine(root, "src/compiler/WebSharper.CSharp.Analyzer/install.ps1"))
                                yield fileTools (Path.Combine(root, "src/compiler/WebSharper.CSharp.Analyzer/uninstall.ps1"))
                            }
                }
        let testingNuPkg =
            let bt =
                bt.PackageId(Config.TestingPackageId, version)
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.TestingDescription
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
                            Authors = [ Config.Company ]
                    })
                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
                                for src in testingExports do
                                    yield fileLib src
                            }
                }
        let compilerNuPkg =
            let bt =
                bt.PackageId(Config.CompilerPackageId, version)
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.CompilerDescription
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
                            Authors = [ Config.Company ]
                    })
                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
                                for src in compilerExports do
                                    yield fileLib45 src
                            }
                }
        [ nuPkg; fsharpNuPkg; csharpNuPkg; testingNuPkg; compilerNuPkg ]

    let Package () =
        let nuPkgs = nuPkgs ()
        let sln = bt.Solution (Seq.cast nuPkgs)
        sln.Build()

    let SetVersion () =
        let v = PackageVersion.Full.Find bt
        let outPath = Path.Combine(root, "msbuild", "AssemblyInfo.extra.fs")
        let inPath = Path.Combine(root, "msbuild", "AssemblyInfo.extra.fs.in")
        let oldFile =
            try File.ReadAllLines outPath
            with _ -> [||]
        let newFile =
            File.ReadAllLines inPath
            |> Array.map (fun s -> s.Replace("{version}", v.ToString()))
        if newFile <> oldFile then
            File.WriteAllLines(outPath, newFile)

    [<EntryPoint>]
    let Start args =
        try
            match Seq.toList args with
            | ["minify"] | ["prepare"] ->
                Minify.Run()
                SetVersion()
            | ["package"] -> Package ()
            | _ ->
                printfn "Known commands:"
                printfn "  minify"
                printfn "  package"
                printfn "  prepare"
            0
        with e ->
            stderr.WriteLine(e)
            1
