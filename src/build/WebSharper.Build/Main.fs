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
            |> Seq.map (fun p -> ("lib", p))

        let tools name =
            Directory.EnumerateFiles(buildDir, name + ".dll")
            |> Seq.map (fun p -> ("tools", p))
        
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
//            tools "WebSharper.Sitelets.Offline" // TODO: Sitelets API for C#
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
        let file kind src tgt =
            sprintf "/%s/net40/%s" kind (defaultArg tgt (Path.GetFileName src))
            |> fileAt src
        let file45 kind src tgt =
            sprintf "/%s/net45/%s" kind (defaultArg tgt (Path.GetFileName src))
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
//                            yield fileAt (Path.Combine(root, "msbuild", "WebSharper.FSharp.targets")) "build/WebSharper.FSharp.targets"
//                            yield fileAt (Path.Combine(root, "msbuild", "WebSharper.CSharp.targets")) "build/WebSharper.CSharp.targets"
                            for kind, src in coreExports do
                                yield file kind src None
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
//                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.FSharp.targets")) ("build/" + Config.FSharpPackageId + ".targets")
                                yield file "tools" (out "WsFsc.exe") None
                                yield file "tools" (out "WsFsc.exe.config") None
                                let fscore = Path.Combine(root, "packages", "FSharp.Core.4.0.0.1", "lib", "net40")
                                yield file "tools" (Path.Combine(fscore, "FSharp.Core.optdata")) None
                                yield file "tools" (Path.Combine(fscore, "FSharp.Core.sigdata")) None
                                for kind, src in fsExports do
                                    yield file kind src None
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
//                .AddDependency(Config.PackageId, nuPkg.GetComputedVersion())
                .AddNuGetExportingProject {
                    new INuGetExportingProject with
                        member p.NuGetFiles =
                            seq {
                                yield fileAt (Path.Combine(root, "msbuild", "Zafir.CSharp.targets")) ("build/" + Config.CSharpPackageId + ".targets")
                                yield file45 "tools" (out "ZafirCs.exe") None
                                yield file45 "tools" (out "ZafirCs.exe.config") None
                                let fscore = Path.Combine(root, "packages", "FSharp.Core.4.0.0.1", "lib", "net40")
                                yield file45 "tools" (Path.Combine(fscore, "FSharp.Core.optdata")) None
                                yield file45 "tools" (Path.Combine(fscore, "FSharp.Core.sigdata")) None
                                for kind, src in csExports do
                                    yield file45 kind src None
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
                                for kind, src in testingExports do
                                    yield file kind src None
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
                                for kind, src in compilerExports do
                                    yield file45 kind src None
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
