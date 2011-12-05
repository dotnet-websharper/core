// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Prints resource dependency graphs in GML format.
module internal WebSharper.DependencyReporter

open System.IO
open System.Collections.Generic
module R = IntelliFactory.WebSharper.Core.Resources
module M = IntelliFactory.WebSharper.Core.Metadata

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
