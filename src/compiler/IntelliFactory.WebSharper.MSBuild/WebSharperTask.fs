// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.MSBuild

open System
open System.IO
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.Core
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Compiler
module FE = FrontEnd

[<AutoOpen>]
module WebSharperTaskModule =

    type Settings =
        {
            Command : string
            Configuration : string
            ItemInput : ITaskItem []
            ItemOutput : ITaskItem []
            KeyOriginatorFile : string
            Log : TaskLoggingHelper
            MSBuildProjectDirectory : string
            Name : string
            SetItemOutput : ITaskItem [] -> unit
            WebProjectOutputDir : string
            WebSharperBundleOutputDir : string
            WebSharperHtmlDirectory : string
            WebSharperExplicitRefs : string
            WebSharperProject : string
        }

    type ProjectType =
        | Bundle of webroot: option<string>
        | Extension
        | Html
        | Library
        | Website of webroot: string

    let GetWebRoot settings =
        match settings.WebProjectOutputDir with
        | null | "" -> None
        | dir ->
            let isWeb =
                File.Exists(Path.Combine(dir, "Web.config"))
                || File.Exists(Path.Combine(dir, "web.config"))
            if isWeb then Some dir else None

    let GetProjectType settings =
        match settings.WebSharperProject with
        | null | "" ->
            match GetWebRoot settings with
            | None -> Library
            | Some dir -> Website dir
        | proj ->
            match proj.ToLower() with
            | "bundle" -> Bundle (GetWebRoot settings)
            | "extension" | "interfacegenerator" -> Extension
            | "html" -> Html
            | "library" -> Library
            | "site" | "web" | "website" ->
                match GetWebRoot settings with
                | None -> Library
                | Some dir -> Website dir
            | _ -> invalidArg "type" ("Invalid project type: " + proj)

    let Fail settings fmt =
        fmt
        |> Printf.ksprintf (fun msg ->
            settings.Log.LogError(msg)
            false)

    let SendResult settings result =
        match result with
        | Compiler.Commands.Ok -> true
        | Compiler.Commands.Errors errors ->
            for e in errors do
                settings.Log.LogError(e)
            true

    let BundleOutputDir settings webRoot =
        match settings.WebSharperBundleOutputDir with
        | null | "" ->
            match webRoot with
            | Some webRoot ->
                let d = Path.Combine(webRoot, "Content")
                let di = DirectoryInfo(d)
                if not di.Exists then
                    di.Create()
                d
            | None -> failwith "WebSharperBundleOutputDir property is required"
        | dir -> dir

    let Bundle settings =
        match GetProjectType settings with
        | Bundle webRoot ->
            let outputDir = BundleOutputDir settings webRoot
            let fileName =
                match settings.Name with
                | null | "" -> "Bundle"
                | name -> name
            match List.ofArray settings.ItemInput with
            | raw :: refs ->
                let cfg =
                    {
                        Compiler.BundleCommand.Config.Create() with
                            AssemblyPaths = raw.ItemSpec :: [for r in refs -> r.ItemSpec]
                            FileName = fileName
                            OutputDirectory = outputDir
                    }
                let env = Compiler.Commands.Environment.Create()
                Compiler.BundleCommand.Instance.Execute(env, cfg)
                |> SendResult settings
            | _ -> Fail settings "Invalid options for Bundle command"
        | _ -> true

    let BundleClean settings webRoot =
        let outputDir = BundleOutputDir settings webRoot
        let fileName =
            match settings.Name with
            | null | "" -> "Bundle"
            | name -> name
        let files =
            Directory.EnumerateFiles(outputDir, "*.*")
            |> Seq.filter (fun p -> Path.GetFileName(p).StartsWith(fileName))
        for f in files do
            File.Delete(f)

    let Compile settings =
        match List.ofArray settings.ItemInput with
        | raw :: refs ->
            let rawInfo = FileInfo(raw.ItemSpec)
            let temp = raw.ItemSpec + ".tmp"
            let tempInfo = FileInfo(temp)
            if not tempInfo.Exists || tempInfo.LastWriteTimeUtc < rawInfo.LastWriteTimeUtc then
                let out =
                    CompilerUtility.Compile {
                        AssemblyFile = raw.ItemSpec
                        KeyOriginatorFile = settings.KeyOriginatorFile
                        References = [ for r in refs -> r.ItemSpec ]
                        RunInterfaceGenerator =
                            match GetProjectType settings with
                            | Extension -> true
                            | _ -> false
                    }
                for msg in out.Messages do
                    msg.SendTo(settings.Log)
                if out.Ok then
                    File.WriteAllText(tempInfo.FullName, "")
                    true
                else
                    Fail settings "Failed to compile assembly with WebSharper"
            else true
        | _ ->
            Fail settings "Need 1+ items for Compile command"

    [<Sealed>]
    type Marker = class end

    let BaseDir =
        typeof<Marker>.Assembly.Location
        |> Path.GetDirectoryName

    let GetReferences ty =
        [
            yield "IntelliFactory.Core"
            yield "IntelliFactory.Formlet"
            yield "IntelliFactory.Html"
            yield "IntelliFactory.JavaScript"
            yield "IntelliFactory.Reactive"
            yield "IntelliFactory.WebSharper.Collections"
            // "IntelliFactory.WebSharper.Compiler"
            yield "IntelliFactory.WebSharper.Control"
            yield "IntelliFactory.WebSharper.Core"
            yield "IntelliFactory.WebSharper.Dom"
            yield "IntelliFactory.WebSharper.Ecma"
            yield "IntelliFactory.WebSharper.Formlet"
            yield "IntelliFactory.WebSharper.Html"
            yield "IntelliFactory.WebSharper.Html5"
            match ty with
            | Extension -> yield "IntelliFactory.WebSharper.InterfaceGenerator"
            | _ -> ()
            yield "IntelliFactory.WebSharper.JQuery"
            yield "IntelliFactory.WebSharper.Sitelets"
            yield "IntelliFactory.WebSharper.Testing"
            yield "IntelliFactory.WebSharper.Web"
            yield "IntelliFactory.WebSharper"
            yield "IntelliFactory.Xml"
        ]

    let ComputeReferences settings =
        let expl =
            match settings.WebSharperExplicitRefs with
            | null | "" -> false
            | t when t.ToLower() = "true" -> true
            | _ -> false
        let alreadyReferenced =
            Set [
                for asm in settings.ItemInput ->
                    AssemblyName(asm.ItemSpec).Name
            ]
        if not expl then
            let projTy = GetProjectType settings
            let assemblies = GetReferences projTy
            let priv =
                match projTy with
                | Bundle _ -> false
                | Extension -> false
                | Html -> false
                | Library -> false
                | Website _ -> true
            settings.SetItemOutput [|
                for asm in assemblies do
                    if alreadyReferenced.Contains(asm) |> not then
                        let hintPath = Path.Combine(BaseDir, asm + ".dll")
                        if File.Exists(hintPath) then
                            let it = TaskItem(asm)
                            it.SetMetadata("HintPath", hintPath)
                            it.SetMetadata("Private", string priv)
                            yield it :> _
                    if alreadyReferenced.Contains("FSharp.Core") |> not then
                        let it = TaskItem("FSharp.Core, Version=4.3.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")
                        it.SetMetadata("Private", string priv)
                        it.SetMetadata("HintPath", Path.Combine(BaseDir, "FSharp.Core.dll"))
                        yield it :> _
            |]
        true

    let Unpack settings =
        match GetProjectType settings with
        | Website webRoot ->
            let assemblies =
                let dir = DirectoryInfo(Path.Combine(webRoot, "bin"))
                Seq.concat [
                    dir.EnumerateFiles("*.dll")
                    dir.EnumerateFiles("*.exe")
                ]
                |> Seq.map (fun fn -> fn.FullName)
                |> Seq.toList
            for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
                let dir = DirectoryInfo(Path.Combine(webRoot, d))
                if not dir.Exists then
                    dir.Create()
            let cfg =
                {
                    Compiler.UnpackCommand.Config.Create() with
                        Assemblies = assemblies
                        RootDirectory = webRoot
                }
            let env = Compiler.Commands.Environment.Create()
            Compiler.UnpackCommand.Instance.Execute(env, cfg)
            |> SendResult settings
        | _ -> true

    let HtmlOutputDirectory settings =
        match settings.WebSharperHtmlDirectory with
        | "" -> Path.Combine(settings.MSBuildProjectDirectory, "bin", "html")
        | dir -> dir

    let Html settings =
        match GetProjectType settings with
        | Html ->
            match List.ofArray settings.ItemInput with
            | main :: refs ->
                let main = main.ItemSpec
                let refs = [for r in refs -> r.ItemSpec]
                let cfg =
                    {
                        Compiler.HtmlCommand.Config.Create(main) with
                            Mode =
                                match settings.Configuration with
                                | x when x.ToLower().Contains("debug") -> Compiler.HtmlCommand.Debug
                                | x when x.ToLower().Contains("release") -> Compiler.HtmlCommand.Release
                                | _ -> Compiler.HtmlCommand.Debug
                            OutputDirectory = HtmlOutputDirectory settings
                            ProjectDirectory = settings.MSBuildProjectDirectory
                            ReferenceAssemblyPaths = refs
                    }
                let env = Compiler.Commands.Environment.Create()
                Compiler.HtmlCommand.Instance.Execute(env, cfg)
                |> SendResult settings
            | _ -> Fail settings "Invalid arguments for Html command"
        | _ -> true

    let HtmlClean settings =
        let d = DirectoryInfo(HtmlOutputDirectory settings)
        if d.Exists then
            d.Delete(``recursive`` = true)

    let Clean settings =
        // clean temp file used during compilation
        do
            match settings.ItemInput with
            | [| intermAssembly |] ->
                let tmp = FileInfo(intermAssembly.ItemSpec + ".tmp")
                if tmp.Exists then
                    tmp.Delete()
            | _ -> ()
        match GetProjectType settings with
        | ProjectType.Bundle webRoot ->
            BundleClean settings webRoot
            true
        | ProjectType.Extension ->
            true
        | ProjectType.Html ->
            HtmlClean settings
            true
        | ProjectType.Library ->
            true
        | ProjectType.Website webRoot ->
            // clean what Unpack command generated:
            for d in ["Scripts/WebSharper"; "Content/WebSharper"] do
                let dir = DirectoryInfo(Path.Combine(webRoot, d))
                if dir.Exists then
                    dir.Delete(``recursive`` = true)
            true

    let Execute settings =
        try
            match settings.Command with
            | "Bundle" -> Bundle settings
            | "Clean" -> Clean settings
            | "Compile" -> Compile settings
            | "ComputeReferences" -> ComputeReferences settings
            | "Html" -> Html settings
            | "Unpack" -> Unpack settings
            | cmd -> Fail settings "Unknown command: %s" (string cmd)
        with e ->
            settings.Log.LogErrorFromException(e)
            false

[<Sealed>]
type WebSharperTask() =
    inherit AppDomainIsolatedTask()

    member val Configuration = "" with get, set
    member val ItemInput : ITaskItem [] = Array.empty with get, set
    member val KeyOriginatorFile = "" with get, set
    member val MSBuildProjectDirectory = "" with get, set
    member val Name = "" with get, set
    member val WebProjectOutputDir = "" with get, set
    member val WebSharperBundleOutputDir = "" with get, set
    member val WebSharperExplicitRefs = "" with get, set
    member val WebSharperHtmlDirectory = "" with get, set
    member val WebSharperProject = "" with get, set

    [<Required>]
    member val Command = "" with get, set

    [<Output>]
    member val ItemOutput : ITaskItem [] = Array.empty with get, set

    override this.Execute() =
        Execute {
            Command = this.Command
            Configuration = this.Configuration
            ItemInput = this.ItemInput
            ItemOutput = this.ItemOutput
            KeyOriginatorFile = this.KeyOriginatorFile
            Log = this.Log
            MSBuildProjectDirectory = this.MSBuildProjectDirectory
            Name = this.Name
            SetItemOutput = fun items -> this.ItemOutput <- items
            WebProjectOutputDir = this.WebProjectOutputDir
            WebSharperBundleOutputDir = this.WebSharperBundleOutputDir
            WebSharperExplicitRefs = this.WebSharperExplicitRefs
            WebSharperHtmlDirectory = this.WebSharperHtmlDirectory
            WebSharperProject = this.WebSharperProject
        }
