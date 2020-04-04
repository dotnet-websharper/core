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
#r "../../build/Release/net461/Mono.Cecil.dll"
#r "../../build/Release/net461/Mono.Cecil.Mdb.dll"
#r "../../build/Release/net461/Mono.Cecil.Pdb.dll"
#r "System.Configuration.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Web.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../build/Release/net461/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/net461/WebSharper.Core.dll"
#r "../../build/Release/net461/WebSharper.JavaScript.dll"
#r "../../build/Release/net461/WebSharper.JQuery.dll"
#r "../../build/Release/net461/WebSharper.Main.dll"
#r "../../build/Release/net461/WebSharper.Collections.dll"
#r "../../build/Release/net461/WebSharper.Control.dll"
#r "../../build/Release/net461/WebSharper.Web.dll"
#r "../../build/Release/net461/WebSharper.Sitelets.dll"
#r "../../build/Release/net461/WebSharper.Compiler.dll"

fsi.ShowDeclarationValues <- false

open System
open System.IO
open System.Collections.Generic

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release\net461", x + ".dll")
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

let translate expr = 
    let compiler = QuotationCompiler(metadata)
    let js, refs = compiler.CompileToJSAndRefs(expr, WebSharper.Core.JavaScript.Preferences.Readable)
    js, refs |> List.length

translate <@ [ 1; 2 ] |> List.map (fun x -> x + 1) @>

//translate """
//module M

//open WebSharper

//[<JavaScript>]
//module Module =
//    let AnonRecord (x: {| A : int |}) = {| B = x.A |}

//    type AnonRecordInUnion =
//        | AnonRecordTest of {| A: int; B: string|}

//    let AnonRecordInUnion() =
//        AnonRecordTest {| A = 3; B = "hi"|}   

//    let AnonRecordNested() =
//        {| A = 1; B = {| A = 2; B = "hi"|}|}  
        
//    let StructAnonRecord() =
//        let a = struct {| SA = 5 |}
//        a.SA
//"""

//translate """
//module M

//open WebSharper

//module Bug923 =
//    type V2<[<Measure>] 'u> =
//        struct
//            val x : float<'u>
//            val y : float<'u>
//            new (x, y) = {x=x; y=y}
//        end

//        static member (+) (a : V2<_>, b : V2<_>) = 
//            V2 (a.x + b.x, a.y + b.y)

//    [<JavaScript>]
//    let addFloatsWithMeasures (a: float<'a>) (b: float<'a>) = a + b

//    """

