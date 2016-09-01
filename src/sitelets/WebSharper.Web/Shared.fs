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

module WebSharper.Web.Shared

open System.Collections.Generic
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module R = WebSharper.Core.Reflection

let private trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let private allReferencedAssemblies() =
    let d = Dictionary<string, System.Reflection.Assembly>()
    let rec loop (asm: System.Reflection.Assembly) =
        asm.GetReferencedAssemblies()
        |> Array.iter (fun asmName ->
            if not (d.ContainsKey asmName.Name) then
                try
                    let asm = System.AppDomain.CurrentDomain.Load(asmName)
                    if not asm.IsDynamic then
                        d.Add(asmName.Name, asm)
                        loop asm
                with _ ->
                    trace.TraceEvent(System.Diagnostics.TraceEventType.Warning, 1,
                        "Failed to load referenced assembly for metadata: ", asmName.FullName))
    let asms =
        System.AppDomain.CurrentDomain.GetAssemblies()
        |> Array.filter (fun asm -> not asm.IsDynamic)
    asms |> Array.iter (fun asm -> d.Add(asm.GetName().Name, asm))
    asms |> Array.iter loop
    d
    |> Seq.map (fun (KeyValue(_, v)) -> v)
    |> List.ofSeq

let private loadMetadata () =
    let before = System.DateTime.UtcNow
    let value =
        allReferencedAssemblies()
        |> List.choose M.AssemblyInfo.LoadReflected
        |> M.Info.Create
    let after = System.DateTime.UtcNow
    trace.TraceInformation("Initialized WebSharper in {0} seconds.",
        (after-before).TotalSeconds)
    value

let Metadata = loadMetadata ()

let Json = J.Provider.CreateTyped Metadata

let PlainJson = J.Provider.Create ()

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
