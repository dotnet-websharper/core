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

namespace WebSharper.MSBuild

open System
open System.Diagnostics
open System.IO
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.Core
open WebSharper
open WebSharper.Compiler
module FE = FrontEnd

[<AutoOpen>]
module WebSharperTaskModule =

    let NotNull (def: 'T) (x: 'T) =
        if Object.ReferenceEquals(x, null) then def else x

    type Settings =
        {
            Command : string
            Configuration : string
            EmbeddedResources : ITaskItem []
            ItemInput : ITaskItem []
            ItemOutput : ITaskItem []
            KeyOriginatorFile : string
            Log : TaskLoggingHelper
            MSBuildProjectDirectory : string
            Name : string
            SetItemOutput : ITaskItem [] -> unit
            SetReferenceCopyLocalPaths : ITaskItem [] -> unit
            WebProjectOutputDir : string
            WebSharperBundleOutputDir : string
            WebSharperHtmlDirectory : string
            WebSharperProject : string
            WebSharperSourceMap : bool
        }

    type ProjectType =
        | Bundle of webroot: option<string>
        | Extension
        | Html
        | Library
        | Website of webroot: string

    let (|Defined|_|) s =
        match s with
        | null | "" -> None
        | _ -> Some s

    let GetWebRoot settings =
        match settings.WebProjectOutputDir with
        | Defined out -> Some out
        | _ ->
            let dir = settings.MSBuildProjectDirectory
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

    let Timed f =
        let sw = Stopwatch()
        sw.Start()
        let r = f ()
        (r, sw.Elapsed)

    let Compile settings =
        match List.ofArray settings.ItemInput with
        | raw :: refs ->
            let rawInfo = FileInfo(raw.ItemSpec)
            let temp = raw.ItemSpec + ".tmp"
            let tempInfo = FileInfo(temp)
            if not tempInfo.Exists || tempInfo.LastWriteTimeUtc < rawInfo.LastWriteTimeUtc then
                let main () =
                    let out =
                        CompilerUtility.Compile {
                            AssemblyFile = raw.ItemSpec
                            KeyOriginatorFile = settings.KeyOriginatorFile
                            EmbeddedResources =
                                [
                                    for r in settings.EmbeddedResources ->
                                        Path.Combine(settings.MSBuildProjectDirectory, r.ItemSpec)
                                ]
                            References = [ for r in refs -> r.ItemSpec ]
                            ProjectDir = settings.MSBuildProjectDirectory
                            RunInterfaceGenerator =
                                match GetProjectType settings with
                                | Extension -> true
                                | _ -> false
                            IncludeSourceMap = settings.WebSharperSourceMap
                        }
                    for msg in out.Messages do
                        msg.SendTo(settings.Log)
                    if out.Ok then
                        File.WriteAllText(tempInfo.FullName, "")
                    out.Ok
                settings.Log.LogMessage(MessageImportance.High, "Compiling with WebSharper..")
                let (res, t) = Timed main
                if res then
                    settings.Log.LogMessage(MessageImportance.High,
                        "WebSharper: compiled ok in {0} seconds",
                        round (t.TotalSeconds * 100.0) / 100.0)
                res
            else true
        | _ ->
            Fail settings "Need 1+ items for Compile command"

    [<Sealed>]
    type Marker = class end

    let BaseDir =
        typeof<Marker>.Assembly.Location
        |> Path.GetDirectoryName

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
                        UnpackSourceMap = settings.WebSharperSourceMap
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
                            UnpackSourceMap = settings.WebSharperSourceMap
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
            | "Html" -> Html settings
            | "Unpack" -> Unpack settings
            | cmd -> Fail settings "Unknown command: %s" (string cmd)
        with e ->
            settings.Log.LogErrorFromException(e)
            false

[<Sealed>]
type WebSharperTask() =
    inherit AppDomainIsolatedTask()

    member val EmbeddedResources : ITaskItem [] = Array.empty with get, set
    member val Configuration = "" with get, set
    member val ItemInput : ITaskItem [] = Array.empty with get, set
    member val KeyOriginatorFile = "" with get, set
    member val MSBuildProjectDirectory = "" with get, set
    member val Name = "" with get, set
    member val WebProjectOutputDir = "" with get, set
    member val WebSharperBundleOutputDir = "" with get, set
    member val WebSharperHtmlDirectory = "" with get, set
    member val WebSharperProject = "" with get, set
    member val WebSharperSourceMap = "" with get, set

    [<Required>]
    member val Command = "" with get, set

    [<Output>]
    member val ItemOutput : ITaskItem [] = Array.empty with get, set

    [<Output>]
    member val ReferenceCopyLocalPaths : ITaskItem [] = Array.empty with get, set

    member private this.AddProjectReferencesToAssemblyResolution() =
        let referencedAsmNames =
            this.ItemInput
            |> Seq.map (fun i -> i.ItemSpec)
            |> Seq.append (Directory.GetFiles(BaseDir, "*.dll"))
            |> Seq.map (fun i -> Path.GetFileNameWithoutExtension(i), i)
            |> Seq.filter (fst >> (<>) this.Name)
            |> Map.ofSeq
        System.AppDomain.CurrentDomain.add_AssemblyResolve(fun sender e ->
            let assemblyName = AssemblyName(e.Name).Name
            match Map.tryFind assemblyName referencedAsmNames with
            | None -> null
            | Some p -> System.Reflection.Assembly.LoadFrom(p)
        )

    override this.Execute() =
        this.AddProjectReferencesToAssemblyResolution()
        let bool s =
            match s with
            | null | "" -> false
            | t when t.ToLower() = "true" -> true
            | _ -> false
        Execute {
            Command = this.Command
            Configuration = NotNull "Release" this.Configuration
            EmbeddedResources = NotNull [||] this.EmbeddedResources
            ItemInput = NotNull [||] this.ItemInput
            ItemOutput = NotNull [||] this.ItemOutput
            KeyOriginatorFile = NotNull "" this.KeyOriginatorFile
            Log = this.Log
            MSBuildProjectDirectory = NotNull "." this.MSBuildProjectDirectory
            Name = NotNull "Project" this.Name
            SetItemOutput = fun items -> this.ItemOutput <- items
            SetReferenceCopyLocalPaths = fun items -> this.ReferenceCopyLocalPaths <- items
            WebProjectOutputDir = NotNull "" this.WebProjectOutputDir
            WebSharperBundleOutputDir = NotNull "" this.WebSharperBundleOutputDir
            WebSharperHtmlDirectory = NotNull "" this.WebSharperHtmlDirectory
            WebSharperProject = NotNull "" this.WebSharperProject
            WebSharperSourceMap = bool this.WebSharperSourceMap
        }
