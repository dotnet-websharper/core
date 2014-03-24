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
open IntelliFactory.WebSharper.Compiler
module FE = FrontEnd

/// Commands handled by the WebSharper task (below).
type Command =
    | Compile
    | ComputeReferences

    /// Parses from string.
    static member Parse(command: string) =
        match command with
        | "Compile" -> Compile
        | "ComputeReferences" -> ComputeReferences
        | _ -> invalidArg "command" ("Invalid command name: " + command)

/// WebSharper project types handled by the task (below).
type ProjectType =
    | Extension
    | Library

    /// Parses from string.
    static member Parse(ty: string) =
        match ty with
        | "Extension" -> Extension
        | "Library" -> Library
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
            "IntelliFactory.Core"
            "IntelliFactory.Formlet"
            "IntelliFactory.Html"
            // "IntelliFactory.JavaScript"
            "IntelliFactory.Reactive"
            "IntelliFactory.WebSharper.Collections"
            // "IntelliFactory.WebSharper.Compiler"
            "IntelliFactory.WebSharper.Control"
            "IntelliFactory.WebSharper.Core"
            "IntelliFactory.WebSharper.Dom"
            "IntelliFactory.WebSharper.Ecma"
            "IntelliFactory.WebSharper.Formlet"
            "IntelliFactory.WebSharper.Html"
            "IntelliFactory.WebSharper.Html5"
            "IntelliFactory.WebSharper.InterfaceGenerator"
            "IntelliFactory.WebSharper.JQuery"
            "IntelliFactory.WebSharper.Sitelets"
            "IntelliFactory.WebSharper.Testing"
            "IntelliFactory.WebSharper.Web"
            "IntelliFactory.WebSharper"
            "IntelliFactory.Xml"
        ]

    let DoComputeReferences projTy : ITaskItem [] =
        let assemblies = GetReferences projTy
        let priv =
            match projTy with
            | Extension -> false
            | Library -> false
        [|
            for asm in assemblies do
                let hintPath = Path.Combine(BaseDir, asm + ".dll")
                if File.Exists(hintPath) then
                    let it = TaskItem(asm)
                    it.SetMetadata("HintPath", hintPath)
                    it.SetMetadata("Private", string priv)
                    yield it :> _
        |]

    let DoCompile ty (log: TaskLoggingHelper) (input: ITaskItem[]) =
        match List.ofArray input with
        | raw :: refs ->
            let rawInfo = FileInfo(raw.ItemSpec)
            let temp = raw.ItemSpec + ".tmp"
            let tempInfo = FileInfo(temp)
            if not tempInfo.Exists || tempInfo.LastWriteTimeUtc < rawInfo.LastWriteTimeUtc then
                let out =
                    CompilerUtility.Compile {
                        AssemblyFile = raw.ItemSpec
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

/// Implements MSBuild logic used in WebSharper.targets
[<Sealed>]
type WebSharperTask() =
    inherit Task()

    override this.Execute() =
        try
            match Command.Parse this.Command with
            | Compile ->
                DoCompile (ProjectType.Parse this.ProjectType) this.Log this.ItemInput
                true
            | ComputeReferences ->
                this.ItemOutput <- DoComputeReferences (ProjectType.Parse this.ProjectType)
                true
        with e ->
            this.Log.LogErrorFromException(e)
            false

    /// Used to specify which "method" to call
    [<Required>]
    member val Command = "" with get, set

    /// Item input for item commands.
    member val ItemInput : ITaskItem [] = Array.empty with get, set

    /// Item output for item commands.
    [<Output>]
    member val ItemOutput : ITaskItem [] = Array.empty with get, set

    /// Specifies which project type is being built.
    member val ProjectType = "Library" with get, set
