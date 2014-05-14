// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.WebSharper.Build

open System
open System.IO
open IntelliFactory.Build
open IntelliFactory.Core
module VSI = VSIntegration

module Main =

    let private root =
        Path.Combine(__SOURCE_DIRECTORY__, "../../..")
        |> Path.GetFullPath

    let private buildDir =
        Path.Combine(root, "build", "Release")

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

    let private configureVSI (nuPkg: NuGetPackageBuilder) : VSI.Config =
        let nupkgPath = nuPkg.GetComputedFileName()
        let vsixPath = Path.ChangeExtension(nupkgPath, ".vsix")
        {
            NuPkgPath = nupkgPath
            RootPath = root
            VsixPath = vsixPath
        }

    let private searchDir (dir: string) =
        let dir =Path.GetFullPath(dir)
        Directory.EnumerateFiles(dir, "*.*", SearchOption.AllDirectories)
        |> Seq.map (fun p -> (p, p.Substring(dir.Length).Replace("\\", "/")))

    let private exports =
        let lib name =
            Seq.concat [
                Directory.EnumerateFiles(buildDir, name + ".dll")
                Directory.EnumerateFiles(buildDir, name + ".xml")
            ]
        Seq.concat [
            // compiler:
            lib "IntelliFactory.JavaScript"
            lib "IntelliFactory.WebSharper.Compiler"
            lib "IntelliFactory.WebSharper.Core"
            lib "IntelliFactory.WebSharper.InterfaceGenerator"
            lib "IntelliFactory.WebSharper.MSBuild"
            // formlets:
            lib "IntelliFactory.Formlet"
            lib "IntelliFactory.Reactive"
            lib "IntelliFactory.WebSharper.Formlet"
            // htmllib:
            lib "IntelliFactory.Html"
            lib "IntelliFactory.WebSharper.Html"
            lib "IntelliFactory.WebSharper.Html5"
            // sitelets:
            lib "IntelliFactory.WebSharper.Sitelets"
            lib "IntelliFactory.WebSharper.Web"
            // stdlib:
            lib "IntelliFactory.WebSharper"
            lib "IntelliFactory.WebSharper.Collections"
            lib "IntelliFactory.WebSharper.Control"
            lib "IntelliFactory.WebSharper.Dom"
            lib "IntelliFactory.WebSharper.Ecma"
            lib "IntelliFactory.WebSharper.JQuery"
            lib "IntelliFactory.WebSharper.Testing"
            // build:
            lib "IntelliFactory.WebSharper.Templates"
            // foreign:
            lib "FsNuGet"
            lib "NuGet.Core"
            lib "FSharp.Core"
            lib "Mono.Cecil"
            lib "Mono.Cecil.Mdb"
            lib "Mono.Cecil.Pdb"
            lib "IntelliFactory.Core"
            lib "IntelliFactory.Xml"
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
        let file src tgt =
            "/tools/net40/" + defaultArg tgt (Path.GetFileName src)
            |> fileAt src
        nuPkg.AddNuGetExportingProject {
            new INuGetExportingProject with
                member p.NuGetFiles =
                    seq {
                        let cfg = configureVSI nuPkg
                        yield! VSI.BuildContents cfg
                        yield file "build/Release/WebSharper.exe" None
                        yield file "build/Release/WebSharper.exe" (Some "WebSharper31.exe")
                        yield file "build/Release/WebSharper31.exe.config" None
                        for src in exports do
                            yield file src None
                        let fscore = Path.Combine(root, "packages", "FSharp.Core.3", "lib", "net40")
                        yield file (Path.Combine(fscore, "FSharp.Core.optdata")) None
                        yield file (Path.Combine(fscore, "FSharp.Core.sigdata")) None
                        for (src, tgt) in searchDir (Path.Combine(root, "docs")) do
                            yield fileAt src ("/docs" + tgt)
                        yield fileAt "build/templates.zip" "/templates/templates.zip"
                    }
        }

    let BuildTemplatesZip () =
        use zip = SharpCompress.Archive.Zip.ZipArchive.Create()
        let dir = DirectoryInfo("templates")
        for f in dir.EnumerateFiles("*.*", SearchOption.AllDirectories) do
            zip.AddEntry(f.FullName.Replace(dir.FullName, ""), f)
        let ci = SharpCompress.Common.CompressionInfo()
        ci.Type <- SharpCompress.Common.CompressionType.Deflate
        Directory.CreateDirectory("build") |> ignore
        use out = File.OpenWrite("build/templates.zip")
        zip.SaveTo(out, ci)

    let Package () =
        let nuPkg = nuPkg ()
        let sln = bt.Solution [nuPkg]
        sln.Build()
        configureVSI nuPkg
        |> VSI.BuildVsixFile

    [<EntryPoint>]
    let Start args =
        try
            match Seq.toList args with
            | ["minify"] | ["prepare"] ->
                Minify.Run()
                BuildTemplatesZip ()
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
