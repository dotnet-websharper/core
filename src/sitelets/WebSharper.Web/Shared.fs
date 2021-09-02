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

//let private trace =
//    System.Diagnostics.TraceSource("WebSharper",
//        System.Diagnostics.SourceLevels.All)

//let private loadMetadata () =
//    let before = System.DateTime.UtcNow
//    let metadataSetting =
//        Context.GetSetting "WebSharperSharedMetadata"
//        |> Option.map (fun x -> x.ToLower())
//    match metadataSetting with
//    | Some "none" ->
//        M.Info.Empty, Graph.Empty
//    | _ ->
//        let runtimeMeta =
//            System.Reflection.Assembly.GetEntryAssembly()
//            |> M.IO.LoadRuntimeMetadata
//        match runtimeMeta with
//        | None ->
//            trace.TraceInformation("Runtime WebSharper metadata not found.")
//            M.Info.Empty, Graph.Empty 
//        | Some meta ->
//            let after = System.DateTime.UtcNow
//            let res =
//                meta, Graph.FromData meta.Dependencies
//            trace.TraceInformation("Initialized WebSharper in {0} seconds.",
//                (after-before).TotalSeconds)
//            res

//let Metadata, Dependencies = loadMetadata () 

//let Json = J.Provider.CreateTyped Metadata

let PlainJson = WebSharper.Json.ServerSideProvider

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
