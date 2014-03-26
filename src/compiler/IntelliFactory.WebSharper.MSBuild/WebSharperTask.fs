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
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.Core
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Compiler
module FE = FrontEnd

/// Commands handled by the WebSharper task (below).
type Command =
    | Compile
    | ComputeReferences
    | Unpack

    /// Parses from string.
    static member Parse(command: string) =
        match command with
        | "Compile" -> Compile
        | "ComputeReferences" -> ComputeReferences
        | "Unpack" -> Unpack
        | _ -> invalidArg "command" ("Invalid command name: " + command)

/// WebSharper project types handled by the task (below).
type ProjectType =
    | Extension
    | Library
    | Website

    /// Parses from string.
    static member Parse(ty: string) =
        match ty.ToLower() with
        | "extension" | "interfacegenerator" -> Extension
        | "library" -> Library
        | "web" | "website" -> Website
        | _ -> invalidArg "type" ("Invalid project type: " + ty)

[<AutoOpen>]
module private WebSharperTaskModule =

    [<Sealed>]
    type private Marker = class end

    let private BaseDir =
        typeof<Marker>.Assembly.Location
        |> Path.GetDirectoryName

    let GetReferences (ty: ProjectType) =
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

    let DoComputeReferences projTy : ITaskItem [] =
        let assemblies = GetReferences projTy
        let priv =
            match projTy with
            | Extension -> false
            | Library -> false
            | Website -> true
        [|
            for asm in assemblies do
                let hintPath = Path.Combine(BaseDir, asm + ".dll")
                if File.Exists(hintPath) then
                    let it = TaskItem(asm)
                    it.SetMetadata("HintPath", hintPath)
                    it.SetMetadata("Private", string priv)
                    yield it :> _
        |]

    let DoCompile ty (log: TaskLoggingHelper) (input: ITaskItem[]) (keyOriginatorFile: string) =
        match List.ofArray input with
        | raw :: refs ->
            let rawInfo = FileInfo(raw.ItemSpec)
            let temp = raw.ItemSpec + ".tmp"
            let tempInfo = FileInfo(temp)
            if not tempInfo.Exists || tempInfo.LastWriteTimeUtc < rawInfo.LastWriteTimeUtc then
                let out =
                    CompilerUtility.Compile {
                        AssemblyFile = raw.ItemSpec
                        KeyOriginatorFile = keyOriginatorFile
                        References = [ for r in refs -> r.ItemSpec ]
                        RunInterfaceGenerator =
                            match ty with
                            | Extension -> true
                            | _ -> false
                    }
                for msg in out.Messages do
                    msg.SendTo(log)
                if out.Ok then
                    File.WriteAllText(tempInfo.FullName, "")
                else
                    failwith "Failed to compile assembly with WebSharper"
        | _ ->
            failwith "Need 1+ items for Compile command"

    let DoUnpack webRoot =
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
        let cmd =
            Commands.UnpackCommand
                (
                    Assemblies = assemblies,
                    RootDirectory = webRoot
                )
        cmd.Run()

/// Implements MSBuild logic used in WebSharper.targets
[<Sealed>]
type WebSharperTask() =
    inherit Task()

    override this.Execute() =
        try
            match Command.Parse this.Command with
            | Compile ->
                DoCompile this.ActualProjectType this.Log this.ItemInput this.KeyOriginatorFile
            | ComputeReferences ->
                this.ItemOutput <- DoComputeReferences this.ActualProjectType
            | Unpack ->
                DoUnpack this.WebRootDirectory
            true
        with e ->
            this.Log.LogErrorFromException(e)
            false

    member this.ActualProjectType =
        ProjectType.Parse this.ProjectType

    [<Required>]
    member val Command = "" with get, set

    member val ItemInput : ITaskItem [] = Array.empty with get, set

    [<Output>]
    member val ItemOutput : ITaskItem [] = Array.empty with get, set

    member val KeyOriginatorFile = "" with get, set
    member val ProjectType = "Library" with get, set
    member val WebRootDirectory = "." with get, set

