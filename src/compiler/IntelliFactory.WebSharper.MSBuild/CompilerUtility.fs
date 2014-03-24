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

type CompilerInput =
    {
        AssemblyFile : string
        References : list<string>
    }

type CompilerMessage =
    | Send of Message
    | Warn of string

    member msg.SendTo(log: TaskLoggingHelper) =
        match msg with
        | Warn warn -> log.LogWarning(warn)
        | Send msg ->
            match msg.Priority with
            | Priority.Critical
            | Priority.Error ->
                match msg.Location.SourceLocation with
                | Some loc ->
                    log.LogError("WebSharper", "WebSharper", "WebSharper",
                        loc.File, loc.Line, loc.Column, loc.Line, loc.Column, msg.Text)
                | None ->
                    log.LogError(string msg)
            | Priority.Warning ->
                match msg.Location.SourceLocation with
                | Some loc ->
                    log.LogWarning("WebSharper", "WebSharper", "WebSharper",
                        loc.File, loc.Line, loc.Column, loc.Line, loc.Column,
                        msg.Text)
                | None ->
                    log.LogWarning(string msg)

type CompilerOutput =
    {
        Messages : CompilerMessage []
        Ok : bool
    }

[<Sealed>]
type CompilerJob() =

    interface AppDomainUtility.ITransform<CompilerInput,CompilerOutput> with
        member this.Do(input) =
            let refPaths =
                Set [
                    for i in input.AssemblyFile :: input.References ->
                        Path.GetFullPath(i)
                ]
            let aR = AssemblyResolution.AssemblyResolver.Create()
            let aR = aR.SearchPaths(refPaths)
            let out = ResizeArray()
            aR.Wrap <| fun () ->
                let loader = FE.Loader.Create aR (fun msg -> out.Add(Warn msg))
                let refs = [ for r in input.References -> loader.LoadFile(r) ]
                let opts = { FE.Options.Default with References = refs }
                let compiler = FE.Prepare opts (fun msg -> out.Add(Send msg))
                let fileName = input.AssemblyFile
                let assem = loader.LoadFile fileName
                let ok = compiler.CompileAndModify assem
                if ok then
                    assem.Write None fileName
                {
                    Messages = out.ToArray()
                    Ok = ok
                }

module CompilerUtility =

    let Compile input =
        AppDomainUtility.TransformWithAppDomain
            AppDomainUtility.MarkType<CompilerJob>
            input

