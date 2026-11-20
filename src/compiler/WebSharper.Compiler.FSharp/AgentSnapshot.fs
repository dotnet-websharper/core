// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// What a compilation leaves for the booster to answer questions about the project
/// (see the booster's agent tools). Only collected when a sink is set, standalone compilations keep nothing.
module WebSharper.Compiler.FSharp.AgentSnapshot

open FSharp.Compiler.CodeAnalysis
open WebSharper.Compiler
open WebSharper.Compiler.CommandTools

type CompileSnapshot =
    {
        Config : WsConfig
        ExitCode : int
        Checker : FSharpChecker
        /// The project options and results of the F# type check, None if it did not run.
        Check : (FSharpProjectOptions * FSharpCheckProjectResults) option
        /// The WebSharper translation, None if it did not run (F# errors, reference metadata errors).
        Compilation : Compilation option
        /// Merged WebSharper metadata of the references.
        ReferenceMetadata : WebSharper.Core.Metadata.Info option
    }

/// Receives the snapshot at the end of each compilation, set by the booster.
let mutable Sink : (CompileSnapshot -> unit) option = None

let Publish (snapshot: unit -> CompileSnapshot) =
    match Sink with
    | Some sink -> sink (snapshot ())
    | None -> ()
