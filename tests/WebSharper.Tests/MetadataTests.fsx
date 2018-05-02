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
#r "../../build/Release/WebSharper.JQuery.dll"
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
        "WebSharper.JQuery"
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
