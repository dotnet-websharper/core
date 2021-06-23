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
#r "../../build/Release/net472/Mono.Cecil.dll"
#r "../../build/Release/net472/Mono.Cecil.Mdb.dll"
#r "../../build/Release/net472/Mono.Cecil.Pdb.dll"
#r "System.Configuration.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Web.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../build/Release/net472/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/net472/WebSharper.Core.dll"
#r "../../build/Release/net472/WebSharper.JavaScript.dll"
#r "../../build/Release/net472/WebSharper.JQuery.dll"
#r "../../build/Release/net472/WebSharper.Main.dll"
#r "../../build/Release/net472/WebSharper.Collections.dll"
#r "../../build/Release/net472/WebSharper.Control.dll"
#r "../../build/Release/net472/WebSharper.Web.dll"
#r "../../build/Release/net472/WebSharper.Sitelets.dll"
#r "../../build/Release/net472/WebSharper.Compiler.dll"

fsi.ShowDeclarationValues <- false

open System
open System.IO
open System.Collections.Generic

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release\net472", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.JQuery"
        "WebSharper.Main"
        "WebSharper.Collections"
        "WebSharper.Control"
        "WebSharper.Web"
        "WebSharper.Sitelets"
        //"WebSharper.Tests"
        //"WebSharper.InterfaceGenerator.Tests"
    ]

let metadata =
    let metas =
        wsRefs |> Seq.choose(
            WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
        )
    { 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
            Dependencies = WebSharper.Core.DependencyGraph.Graph.FromData(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
    }

open System.IO

open WebSharper.Compiler
open WebSharper.Core.JavaScript
open Microsoft.FSharp.Quotations

type Rec = 
    {
        A : int
    }

[<ReflectedDefinition>]
let recValue = { A = 0 }

[<ReflectedDefinition>]
type Union = 
    | A of int

    member this.Value = match this with A x -> x

[<ReflectedDefinition>]
let unionValue = A 0

[<ReflectedDefinition>]
type TestType(a) =
    static do printfn "hello"
    
    member thisy.Y = 3
    member thisx.X x = x + a + recValue.A + unionValue.Value

[<ReflectedDefinition>]
let f x = TestType(1).X x

open WebSharper.Core.AST

let translate expr = 
    let compiler = QuotationCompiler(metadata)
    let fsiAsm = System.Reflection.Assembly.Load("FSI-ASSEMBLY")
    
    // logging
    for t in fsiAsm.GetTypes() do
        let printReflDef (m: Reflection.MethodBase) =
            match Expr.TryGetReflectedDefinition m with
            | Some expr ->
                printfn "%s.%s (isStatic:%b) : %A" m.DeclaringType.FullName m.Name m.IsStatic expr
            | None -> () 
        for m in t.GetMethods() do      
            printReflDef m
        for m in t.GetConstructors() do      
            printReflDef m
        for f in t.GetFields(WebSharper.Core.AST.Reflection.AllMethodsFlags) do
            printfn "Field %s.%s" t.FullName f.Name

    compiler.CompileReflectedDefinitions(fsiAsm)
    let node = WebSharper.Core.Metadata.EntryPointNode
    let e = compiler.CompileExpression(expr, node)
    let comp = compiler.Compilation
    Translator.DotNetToJavaScript.CompileFull comp
    let ep = ExprStatement(ItemSet(Global [], Value (String "EntryPoint"), Lambda([], e)))

    if List.isEmpty comp.Errors then
        let prefs = WebSharper.Core.JavaScript.Preferences.Readable
        let p =
            Packager.packageAssembly metadata (comp.ToCurrentMetadata()) (Some ep) Packager.ForceImmediate
        let js, _ =
            p |> Packager.exprToString prefs (fun () -> WebSharper.Core.JavaScript.Writer.CodeWriter())
        let refs =
            comp.Graph.GetResources [ node ]
        js, refs
    else
        let errors = comp.Errors
        errors |> Seq.map (fun (_, e) -> e.ToString()) |> Seq.iter (printfn "Error: %s")
        "", []

translate <@ f 1 @>
