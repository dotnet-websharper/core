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

namespace WebSharper.MSBuild.CSharp

open System
open System.IO
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.Core
open WebSharper
open WebSharper.Compiler
module FE = FrontEnd

type CompilerInput =
    {
        AssemblyFile : string
        DocumentationFile : option<string>
        EmbeddedResources : list<string>
        KeyOriginatorFile : string
        ProjectFile : string
        ProjectDir : string
        References : list<string>
        RunInterfaceGenerator : bool
        IncludeSourceMap : bool
    }

    member this.ReadStrongNameKeyPair() =
        match this.KeyOriginatorFile with
        | "" | null -> None
        | p when File.Exists(p) ->
            Some (StrongNameKeyPair(File.ReadAllBytes(p)))
        | _ -> None

type CompilerMessage =
    | CMErr1 of string
    | CMErr2 of string * int * int * string
    | CMExn of exn
    | CMWarn1 of string
    | CMWarn2 of string * int * int * string

    member msg.SendTo(log: TaskLoggingHelper) =
        match msg with
        | CMErr1 msg ->
            log.LogError(msg)
        | CMErr2 (file, line, col, msg) ->
            log.LogError("WebSharper", "WebSharper", "WebSharper",
                file, line, col, line, col, msg)
        | CMWarn1 msg ->
            log.LogWarning(msg)
        | CMWarn2 (file, line, col, msg) ->
            log.LogWarning("WebSharper", "WebSharper", "WebSharper",
                file, line, col, line, col, msg)
        | CMExn err ->
            log.LogErrorFromException(err)

    static member Report(e) =
        CMExn e

    static member Send(msg) =
        match msg.Priority with
        | Priority.Critical
        | Priority.Error ->
            match msg.Location.SourceLocation with
            | Some loc -> CMErr2(loc.File, loc.Line, loc.Column, msg.Text)
            | None -> CMErr1(string msg)
        | Priority.Warning ->
            match msg.Location.SourceLocation with
            | Some loc -> CMWarn2(loc.File, loc.Line, loc.Column, msg.Text)
            | None -> CMWarn1(string msg)

    static member Warn(msg) =
        CMWarn1 msg

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
                        ctx.Add(CompilerMessage.Report err)
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
                    ctx.Add(CompilerMessage.Report err)
                    None
                |> Option.bind (fun next ->
                    next.Run(ctx))
        }

    member x.Fail fmt =
        let ok res =
            {
                Run = fun ctx ->
                    ctx.Add(CompilerMessage.CMErr1 res)
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
                return (name, attr.GetAssembly(), asm)
            | _ ->
                return! Act.Fail "No ExtensionAttribute set on the input assembly"
        }

    let RunInterfaceGenerator aR snk (input: CompilerInput) =
        Act {
            let! (name, asmDef, asm) = LoadInterfaceGeneratorAssembly aR input.AssemblyFile
            let cfg =
                {
                    InterfaceGenerator.CompilerOptions.Default(name.Name) with
                        AssemblyResolver = Some aR
                        AssemblyVersion = name.Version
                        DocPath = input.DocumentationFile
                        EmbeddedResources = input.EmbeddedResources
                        ProjectDir = input.ProjectDir
                        ReferencePaths = input.References
                        StrongNameKeyPair = snk
                }

            let cmp = InterfaceGenerator.Compiler.Create()
            let out = cmp.Compile(cfg, asmDef, asm)
            out.Save(input.AssemblyFile)
        }

    let CompileWithWebSharper aR snk input =
        Act {
            let! out = Act.Out
            let loader = FE.Loader.Create aR (fun msg -> out.Add(CompilerMessage.Warn msg))
            let refs = [ for r in input.References -> loader.LoadFile(r) ]
//            let opts =
//                {
//                    FE.Options.Default with
//                        KeyPair = snk
//                        References = refs
//                }
//            let compiler = FE.Prepare opts (fun msg -> out.Add(CompilerMessage.Send msg))
            
            let errors = 
                try
                    let refMeta =
                        let metas = refs |> List.choose (fun r -> WebSharper.Compiler.FrontEnd.readFromAssembly r)
                        if List.isEmpty metas then None else Some (WebSharper.Core.Metadata.union metas)
                    let compiler = WebSharper.Compiler.CSharp.WebSharperCSharpCompiler(ignore) 
                    let comp = compiler.Compile(refMeta, [||], input.ProjectFile)

                    if not (List.isEmpty comp.Errors) then
                        for pos, e in comp.Errors do
                            match pos with
                            | Some pos ->
                                out.Add(CMErr2 (pos.FileName, fst pos.Start, snd pos.Start, string e))
                            | _ ->
                                out.Add(CMErr1 (string e))
                        Some comp.Errors
                    else
                    let thisMeta = comp.ToCurrentMetadata()
                    let merged = 
                        WebSharper.Core.Metadata.union 
                            [
                                (match refMeta with Some m -> m | _ -> WebSharper.Core.Metadata.empty)
                                thisMeta
                            ]

                    let assem = loader.LoadFile input.AssemblyFile //Mono.Cecil.AssemblyDefinition.ReadAssembly input.AssemblyFile
                    let js = WebSharper.Compiler.FrontEnd.modifyWSAssembly merged thisMeta assem

                    assem.Write snk input.AssemblyFile
                    None
                with e ->
                    out.Add (CMErr1 (e.Message + e.StackTrace))
                    Some []

            match errors with
            | Some err ->
                return! Act.Fail "Failed to compile assembly with WebSharper." 
            | None -> ()

//
//
//            do
//                try WebSharper.Compiler.FrontEnd.modifyAssembly comp assem
//                with e ->
//                    printfn "Could not write metadata because of %A" e
//            if true then
//                printfn "Writing into assembly: %s" fileName
//                assem.Write fileName
//                //assem.Write snk fileName
//            else
//                return! Act.Fail "Failed to compile assembly with WebSharper."
        }

module CompilerUtility =

    let Compile input =
        let aR =
            let files =
                Set [
                    for i in input.AssemblyFile :: input.References ->
                        Path.GetFullPath(i)
                ]
            AssemblyResolution.AssemblyResolver.Create()
                .SearchPaths(files)
        aR.Wrap <| fun () ->
            Act {
                let snk = input.ReadStrongNameKeyPair()
                if input.RunInterfaceGenerator then
                    do! RunInterfaceGenerator aR snk input
                else
                return! CompileWithWebSharper aR snk input
            }
            |> Run
