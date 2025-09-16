// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
#I __SOURCE_DIRECTORY__
#r "../../build/Release/FSharp/net8.0/Mono.Cecil.dll"
#r "../../build/Release/FSharp/net8.0/Mono.Cecil.Mdb.dll"
#r "../../build/Release/FSharp/net8.0/Mono.Cecil.Pdb.dll"
#r "System.Configuration.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Web.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.dll"
#r "../../build/Release/netstandard2.0/WebSharper.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.StdLib.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Web.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Sitelets.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Compiler.dll"

fsi.ShowDeclarationValues <- false

open System
open System.IO
open System.Collections.Generic

typeof<WebSharper.JavaScript.Object> |> ignore // force loading WebSharper.JavaScript
typeof<WebSharper.JavaScript.JS.Kind> |> ignore // force loading WebSharper.StdLib

let metadataCache = Dictionary<System.Reflection.Assembly, option<WebSharper.Core.Metadata.Info>>()

open WebSharper
open WebSharper.Compiler

let translate expr = 
    let metas = ResizeArray()

    let mutable qcomp = QuotationCompiler(WebSharper.Core.Metadata.Info.Empty)

    let printErrors (comp: Compilation) =
        for err in comp.Errors do
            match err with
            | Some pos, msg -> 
                printfn "Error: %O at %O" msg pos
            | None, msg ->
                printfn "Error: %O" msg

    for asm in System.AppDomain.CurrentDomain.GetAssemblies() do
        match metadataCache.TryGetValue asm with
        | true, m -> 
            m |> Option.iter metas.Add
        | _ ->
            let loc = asm.Location
            let m = 
                if not <| System.String.IsNullOrEmpty loc then
                    loc
                    |> WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Core.Metadata.MetadataOptions.FullMetadata 
                else
                    if asm.GetName().Name = "FSI-ASSEMBLY" then
                        //// logging
                        printfn "Assembly: %s" asm.FullName
                        for t in asm.GetTypes() do
                            printfn "Type: %s" t.FullName
                            let printReflDef (m: Reflection.MethodBase) =
                                match Microsoft.FSharp.Quotations.Expr.TryGetReflectedDefinition m with
                                | Some expr ->
                                    printfn "%s.%s (isStatic:%b) : %A" m.DeclaringType.FullName m.Name m.IsStatic expr
                                | None -> () 
                            for m in t.GetMethods() do      
                                printReflDef m
                            for m in t.GetConstructors() do      
                                printReflDef m
                            for f in t.GetFields(WebSharper.Core.AST.Reflection.AllMethodsFlags) do
                                printfn "Field %s: %s attrs %s" f.Name (f.FieldType.ToString())
                                    (f.CustomAttributes |> Seq.map (fun a -> a.AttributeType.ToString()) |> String.concat ", ")

                        let metadata = WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas
                        qcomp <- QuotationCompiler(metadata)
                        let comp = qcomp.Compilation
                        qcomp.CompileReflectedDefinitions(asm)
                        Translator.DotNetToJavaScript.CompileFull comp
                        if List.isEmpty comp.Errors then
                            Some (comp.ToCurrentMetadata())
                        else
                            printErrors comp
                            None
                    else
                        None
            metadataCache.Add(asm, m)
            m |> Option.iter metas.Add
    
    let epNode = WebSharper.Core.Metadata.EntryPointNode
    let e = qcomp.CompileExpression(expr, epNode)
    let ep = WebSharper.Core.AST.ExprStatement(e)

    let comp = qcomp.Compilation
    if List.isEmpty comp.Errors then
        let metadata = WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas
        let graph = 
            WebSharper.Core.DependencyGraph.Graph.FromData (
                // for last meta, we need graph which includes the current entry point compilation too
                metas |> Seq.rev |> Seq.tail |> Seq.map (fun m -> m.Dependencies)
                |> Seq.append (Seq.singleton (comp.Graph.GetData()))
            )
        let nodes = graph.GetDependencies [ epNode ]
        let trimmed = trimMetadata metadata nodes
        let pkg =
            JavaScriptPackager.bundleAssembly WebSharper.Core.JavaScript.Output.JavaScript trimmed trimmed "fsi" (Some ep) JavaScriptPackager.EntryPointStyle.ForceImmediate
        let trPkg, _ = WebSharper.Compiler.JavaScriptWriter.transformProgram WebSharper.Core.JavaScript.JavaScript WebSharper.Core.JavaScript.Readable pkg

        let js, _, _ = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter trPkg false
        printfn "%s" js
    else
        printErrors comp


[<ReflectedDefinition>]
type Rec = 
    {
        A : int
    }

    member thisr.Value = thisr.A

[<ReflectedDefinition>]
let recValue = { A = 0 }

translate <@ recValue.Value + 1 @>


[<ReflectedDefinition>]
type Union = 
    | A of int

    member thisu.Value = match thisu with A x -> x

[<ReflectedDefinition>]
let unionValue = A 0

translate <@ unionValue.Value + 1 @>


[<ReflectedDefinition>]
type TestType(a) as self =
    static do printfn "hello from cctor"
    //do printfn "hello from ctor %O" self
    
    member thisx.X x = x + a + recValue.A + unionValue.Value

[<ReflectedDefinition>]
type TestTypeB(a) =
    inherit TestType(a)
    
    member this.Y x = this.X (x + 1)

[<ReflectedDefinition>]
let f x = TestTypeB(1).Y x

translate <@ f 1 @>
