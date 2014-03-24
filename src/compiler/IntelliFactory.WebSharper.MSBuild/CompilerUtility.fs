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

type CompilerInput =
    {
        AssemblyFile : string
        References : list<string>
        RunInterfaceGenerator : bool
    }

type CompilerMessage =
    | Report of exn
    | Send of Message
    | Warn of string

    member msg.SendTo(log: TaskLoggingHelper) =
        match msg with
        | Report e -> log.LogErrorFromException(e)
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

type CompilerAction<'T> =
    {
        Run : ResizeArray<CompilerMessage> -> option<'T>
    }

[<Sealed>]
type CompilerActionBuilder() =

    member x.Bind(action, cont) =
        {
            Run = fun ctx ->
                action.Run ctx
                |> Option.bind (fun res ->
                    try Some (cont res) with err ->
                        ctx.Add(Report err)
                        None)
                |> Option.bind (fun next ->
                    next.Run(ctx))
        }

    member x.Combine(a, b) =
        x.Bind(a, fun () -> b)

    member x.Delay(f) =
        {
            Run = fun ctx ->
                try Some (f ()) with err ->
                    ctx.Add(Report err)
                    None
                |> Option.bind (fun next ->
                    next.Run(ctx))
        }

    member x.Fail fmt =
        let ok res =
            {
                Run = fun ctx ->
                    ctx.Add(Warn res)
                    None
            }
        Printf.ksprintf ok fmt

    member x.Out =
        { Run = fun ctx -> Some ctx }

    member x.Return(value: 'T) =
        { Run = fun ctx -> Some value }

    member x.ReturnFrom(act: CompilerAction<_>) =
        act

    member x.Zero() =
        { Run = fun ctx -> Some () }

[<AutoOpen>]
module CompilerJobModule =

    let Act = CompilerActionBuilder()

    let Run (act: CompilerAction<unit>) =
        let msg = ResizeArray()
        match act.Run msg with
        | None -> { Ok = false; Messages = msg.ToArray() }
        | Some () -> { Ok = true; Messages = msg.ToArray() }

    let LoadInterfaceGeneratorAssembly (aR: AssemblyResolver) (file: string) =
        Act {
            let asm = Assembly.Load(File.ReadAllBytes(file))
            let name = AssemblyName.GetAssemblyName(file)
            match Attribute.GetCustomAttribute(asm, typeof<InterfaceGenerator.Pervasives.ExtensionAttribute>) with
            | :? InterfaceGenerator.Pervasives.ExtensionAttribute as attr ->
                return (name, attr.GetAssembly())
            | _ ->
                return! Act.Fail "No ExtensionAttribute set on the input assembly"
        }

    let RunInterfaceGenerator aR (input: CompilerInput) =
        Act {
            let! (name, asm) = LoadInterfaceGeneratorAssembly aR input.AssemblyFile
            let cfg =
                {
                    InterfaceGenerator.CompilerOptions.Default name.Name with
                        AssemblyResolver = Some aR
                        AssemblyVersion = name.Version
                        // DocPath : option<string>
                        ReferencePaths = input.References
                }
            let cmp = InterfaceGenerator.Compiler.Create()
            let out = cmp.Compile(cfg, asm)
            do out.Save(input.AssemblyFile)
            return ()
        }

    let CompileWithWebSharper aR input =
        Act {
            let! out = Act.Out
            let loader = FE.Loader.Create aR (fun msg -> out.Add(Warn msg))
            let refs = [ for r in input.References -> loader.LoadFile(r) ]
            let opts = { FE.Options.Default with References = refs }
            let compiler = FE.Prepare opts (fun msg -> out.Add(Send msg))
            let fileName = input.AssemblyFile
            let assem = loader.LoadFile fileName
            let ok = compiler.CompileAndModify assem
            if ok then
                assem.Write None fileName
        }

[<Sealed>]
type CompilerJob() =

    interface AppDomainUtility.ITransform<CompilerInput,CompilerOutput> with
        member this.Do(input) =
            let aR =
                let dirs =
                    Set [
                        for i in input.AssemblyFile :: input.References ->
                            Path.GetDirectoryName(Path.GetFullPath(i))
                    ]
                AssemblyResolution.AssemblyResolver.Create()
                    .SearchDirectories(dirs)
            aR.Wrap <| fun () ->
                Act {
                    if input.RunInterfaceGenerator then
                        do! RunInterfaceGenerator aR input
                    return! CompileWithWebSharper aR input
                }
                |> Run

module CompilerUtility =

    let Compile input =
        AppDomainUtility.TransformWithAppDomain
            AppDomainUtility.MarkType<CompilerJob>
            input

