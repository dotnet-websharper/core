// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
        let bt =
            BuildTool()
                .PackageId(Config.PackageId, Config.PackageVerion)
                .Configure(fun bt ->
                    let outDir = BuildConfig.OutputDir.Find bt
                    bt
                    |> BuildConfig.RootDir.Custom root
                    |> FSharpConfig.OtherFlags.Custom ["--optimize+"]
                    |> Logs.Config.Custom(Logs.Default.Verbose().ToConsole()))
        BuildConfig.CurrentFramework.Custom bt.Framework.Net40 bt

    let private searchDir (dir: string) =
        let dir =Path.GetFullPath(dir)
        Directory.EnumerateFiles(dir, "*.*", SearchOption.AllDirectories)
        |> Seq.map (fun p -> (p, p.Substring(dir.Length).Replace("\\", "/")))

    let private exports =
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
            // htmllib:
            lib "lib"   "WebSharper.Html.Server"
            lib "lib"   "WebSharper.Html.Client"
            // sitelets:
            lib "lib"   "WebSharper.Sitelets"
            lib "lib"   "WebSharper.Web"
            // stdlib:
            lib "lib"   "WebSharper.Main"
            lib "lib"   "WebSharper.Collections"
            lib "lib"   "WebSharper.Control"
            lib "lib"   "WebSharper.JavaScript"
            lib "lib"   "WebSharper.JQuery"
            lib "lib"   "WebSharper.Testing"
            // foreign:
            lib "tools" "FsNuGet"
            lib "tools" "NuGet.Core"
            lib "tools" "FSharp.Core"
            lib "tools" "Mono.Cecil"
            lib "tools" "Mono.Cecil.Mdb"
            lib "tools" "Mono.Cecil.Pdb"
            lib "tools" "IntelliFactory.Core"
            lib "lib"   "IntelliFactory.Xml"
        ]
        |> Seq.toList

    let private nuPkg () =
        let nuPkg =
            bt.NuGet.CreatePackage()
                .Configure(fun x ->
                    {
                        x with
                            Description = Config.Description
                            ProjectUrl = Some Config.Website
                            LicenseUrl = Some Config.LicenseUrl
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
        nuPkg.AddNuGetExportingProject {
            new INuGetExportingProject with
                member p.NuGetFiles =
                    seq {
                        yield fileAt (Path.Combine(root, "msbuild", "WebSharper.targets"))
                            "build/WebSharper.targets"
                        yield file "tools" (out "WebSharper.exe") None
                        yield file "tools" (out "WebSharper.exe") (Some "WebSharper31.exe")
                        yield file "tools" (out "WebSharper31.exe.config") None
                        yield file "tools" (out "WebSharper.exe.config") None
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

    let Package () =
        let nuPkg = nuPkg ()
        let sln = bt.Solution [nuPkg]
        sln.Build()

    [<EntryPoint>]
    let Start args =
        try
            match Seq.toList args with
            | ["minify"] | ["prepare"] ->
                Minify.Run()
                Tags.Run()
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
