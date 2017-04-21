// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

    let private bt =
        BuildTool()
            .PackageId(Config.PackageId, Config.PackageVersion)
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

    let private exports, compilerExports, testingExports =
        let lib kind name =
            Seq.concat [
                Directory.EnumerateFiles(buildDir, name + ".dll")
                Directory.EnumerateFiles(buildDir, name + ".xml")
            ]
            |> Seq.map (fun p -> (kind, p))
        Seq.concat [
            // compiler:
            lib "lib"   "WebSharper.Core.JavaScript"
            lib "tools" "WebSharper.Compiler"
            lib "lib"   "WebSharper.Core"
            lib "lib"   "WebSharper.InterfaceGenerator"
            lib "tools" "WebSharper.MSBuild"
            // sitelets:
            lib "lib"   "WebSharper.Sitelets"
            lib "lib"   "WebSharper.Web"
            // stdlib:
            lib "lib"   "WebSharper.Main"
            lib "lib"   "WebSharper.Collections"
            lib "lib"   "WebSharper.Control"
            lib "lib"   "WebSharper.JavaScript"
            lib "lib"   "WebSharper.JQuery"
            // foreign:
            lib "tools" "FsNuGet"
            lib "tools" "NuGet.Core"
            lib "tools" "FSharp.Core"
            lib "tools" "Mono.Cecil"
            lib "tools" "Mono.Cecil.Mdb"
            lib "tools" "Mono.Cecil.Pdb"
            lib "tools" "IntelliFactory.Core"
        ]
        |> Seq.toList
        ,
        // compilerExports:
        Seq.concat [
            lib "lib" "WebSharper.Compiler"
            lib "lib" "IntelliFactory.Core"
            lib "lib" "Mono.Cecil"
            lib "lib" "Mono.Cecil.Mdb"
            lib "lib" "Mono.Cecil.Pdb"
        ]
        |> Seq.toList
        ,
        // testingExports:
        lib "lib" "WebSharper.Testing"

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
        let out f = Path.Combine(buildDir, f)
        let nuPkg = 
            nuPkg.AddNuGetExportingProject {
                new INuGetExportingProject with
                    member p.NuGetFiles =
                        seq {
                            yield fileAt (Path.Combine(root, "msbuild", "WebSharper.targets"))
                                "build/WebSharper.targets"
                            yield file "tools" (out "WebSharper.exe") None
                            yield file "tools" (out "WebSharper.exe") (Some "WebSharper31.exe")
                            yield file "tools" (out "WebSharper.exe") (Some "WebSharper40.exe")
                            yield file "tools" (out "WebSharper.exe.config") None
                            yield file "tools" (out "WebSharper31.exe.config") None
                            yield file "tools" (out "WebSharper40.exe.config") None
                            yield file "tools" (out "WebSharper41.exe.config") None
                            for kind, src in exports do
                                yield file kind src None
                            let fscore = Path.Combine(root, "packages", "FSharp.Core.3", "lib", "net40")
                            yield file "tools" (Path.Combine(fscore, "FSharp.Core.optdata")) None
                            yield file "tools" (Path.Combine(fscore, "FSharp.Core.sigdata")) None
                            for (src, tgt) in searchDir (Path.Combine(root, "docs")) do
                                yield fileAt src ("/docs" + tgt)
                            yield fileAt (Path.Combine(root, "src", "htmllib", "tags.csv")) ("/tools/net40/tags.csv")
                        }
            }
        let v = Version(nuPkg.GetComputedVersion().Split('-').[0])
        let compilerNuPkg =
            let bt =
                bt.PackageId(Config.CompilerPackageId, Config.PackageVersion)
                |> PackageVersion.Full.Custom v
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
                                    yield file kind src None
                            }
                }
        let testingNuPkg =
            let bt =
                bt.PackageId(Config.TestingPackageId, Config.PackageVersion)
                |> PackageVersion.Full.Custom v
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
        [ nuPkg; compilerNuPkg; testingNuPkg ]

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
