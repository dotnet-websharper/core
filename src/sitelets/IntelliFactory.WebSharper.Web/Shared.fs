// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module IntelliFactory.WebSharper.Web.Shared

module J = IntelliFactory.WebSharper.Core.Json
module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Reflection

let private trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let private loadMetadata () =
    let before = System.DateTime.UtcNow
    let value =
        System.Web.Compilation.BuildManager.GetReferencedAssemblies()
        |> Seq.cast<System.Reflection.Assembly>
        |> Seq.toList
        |> List.choose M.AssemblyInfo.LoadReflected
        |> M.Info.Create
    let after = System.DateTime.UtcNow
    trace.TraceInformation("Initialized WebSharper in {0} seconds.",
        (after-before).TotalSeconds)
    value

let Metadata = loadMetadata ()

let Json = J.Provider.CreateTyped Metadata

[<Literal>]
let internal SCRIPT_MANAGER_ID = "WebSharperScriptManager"
