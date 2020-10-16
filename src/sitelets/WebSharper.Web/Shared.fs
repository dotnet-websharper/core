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

module WebSharper.Web.Shared

open System.Collections.Generic
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph

let private trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let private loadMetadata () =
    let before = System.DateTime.UtcNow
    let metadataSetting =
        Context.GetSetting "WebSharperSharedMetadata"
        |> Option.map (fun x -> x.ToLower())
    match metadataSetting with
    | Some "none" ->
        M.Info.Empty, Graph.Empty
    | _ ->
        let filterExpressions : M.Info -> M.Info =
            match metadataSetting with
            | Some "inlines" -> fun m -> m.DiscardNotInlineExpressions()
            | Some "notinlines" -> fun m -> m.DiscardInlineExpressions()
            | Some "full" | None -> id
            | _ -> fun m -> m.DiscardExpressions()
        let metas =
            WebSharper.Core.Resources.AllReferencedAssemblies.Value
            |> Seq.choose M.IO.LoadReflected
            |> Seq.map filterExpressions
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

let PlainJson = WebSharper.Json.ServerSideProvider

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
