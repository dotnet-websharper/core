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
#r "../../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.Mdb.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.Pdb.dll"
#r "System.Configuration.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Web.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../build/Release/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/WebSharper.Core.dll"
#r "../../build/Release/WebSharper.JavaScript.dll"
#r "../../build/Release/WebSharper.Main.dll"
#r "../../build/Release/WebSharper.Collections.dll"
#r "../../build/Release/WebSharper.Control.dll"
#r "../../build/Release/WebSharper.Web.dll"
#r "../../build/Release/FSharp/WebSharper.Compiler.dll"
#r "../../build/Release/FSharp/WebSharper.Compiler.FSharp.dll"

fsi.ShowDeclarationValues = false

open System
open System.IO
open System.Collections.Generic

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.Main"
        "WebSharper.Collections"
        "WebSharper.Control"
        "WebSharper.Web"
        "WebSharper.Sitelets"
    ]

let metadata =
    let metas =
        wsRefs |> Seq.choose(
            WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
        )
    { 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
            Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
    }

open WebSharper.Compiler

let e =
    metadata.Classes.Values 
    |> Seq.collect (fun c -> c.Methods) 
    |> Seq.pick (fun (KeyValue(k, (_,_,e))) -> if k.Value.MethodName = "GetSlice" then Some e else None)
    |> removeSourcePos.TransformExpression

WebSharper.Core.AST.Debug.PrintExpression e
