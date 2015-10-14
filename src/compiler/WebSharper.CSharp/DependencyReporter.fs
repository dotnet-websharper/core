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

/// Prints resource dependency graphs in GML format.
module internal WebSharper.DependencyReporter

open System.IO
open System.Collections.Generic
module R = WebSharper.Core.Resources
module M = WebSharper.Core.Metadata

/// Runs the dependency graph report generation.
let Run (inFile: string) =
    match M.AssemblyInfo.Load inFile with
    | Some data ->
        for (node, res) in data.Requirements do
            stdout.WriteLine node
            for key in res do
                stdout.WriteLine("  {0}", key)
        0
    | None ->
        stderr.WriteLine "Not a WebSharper-compiled assembly."
        1
