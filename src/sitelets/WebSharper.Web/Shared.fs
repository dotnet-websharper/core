// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Web.Shared

module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph

let private trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let private loadMetadata () =
    let before = System.DateTime.UtcNow
    let metas =
        System.Web.Compilation.BuildManager.GetReferencedAssemblies()
        |> Seq.cast<System.Reflection.Assembly>
        |> Seq.choose M.IO.LoadReflected
        |> Seq.map (fun m -> m.DiscardExpressions())
        |> Seq.toList
    let after = System.DateTime.UtcNow
    trace.TraceInformation("Initialized WebSharper in {0} seconds.",
        (after-before).TotalSeconds)
    if List.isEmpty metas then 
        M.Info.Empty, Graph.Empty 
    else 
        let graph = Graph.FromData (metas |> Seq.map (fun m -> m.Dependencies))
        { M.Info.UnionWithoutDependencies metas with M.Dependencies = graph.GetData() }, graph

let Metadata, Dependencies = loadMetadata () 

let Json = J.Provider.CreateTyped Metadata

let PlainJson = J.Provider.Create ()

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
