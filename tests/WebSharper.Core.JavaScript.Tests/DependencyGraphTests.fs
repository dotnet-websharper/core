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

module WebSharper.Core.JavaScript.Test.DependencyGraphTests

open System.IO
open NUnit.Framework
open WebSharper.Core
module M = WebSharper.Core.Metadata

/// The dependency graph in the metadata of a built WebSharper.StdLib.dll.
let private builtStdLibGraph () =
    let rec findRoot (d: DirectoryInfo) =
        if isNull d then None
        elif File.Exists(Path.Combine(d.FullName, "WebSharper.sln")) then Some d.FullName
        else findRoot d.Parent
    match findRoot (DirectoryInfo(TestContext.CurrentContext.TestDirectory)) with
    | Some root when Directory.Exists(Path.Combine(root, "build", "Release")) ->
        Directory.GetFiles(Path.Combine(root, "build", "Release"), "WebSharper.StdLib.dll", SearchOption.AllDirectories)
        |> Array.filter (fun p -> not (p.Contains "deploy") && not (p.Contains (string Path.DirectorySeparatorChar + "publish")))
        |> Array.tryPick (WebSharper.Compiler.FrontEnd.ReadFromFile M.FullMetadata)
        |> Option.map (fun meta -> DependencyGraph.Graph.FromData meta.Dependencies)
    | _ -> None

[<Test>]
let ``#1621 Dictionary members called through its interfaces are kept`` () =
    match builtStdLibGraph () with
    | None -> Assert.Ignore "No built WebSharper.StdLib metadata found"
    | Some graph ->
        let dictionary = "System.Collections.Generic.Dictionary`2"
        let typeNode =
            graph.Nodes |> Seq.find (function
                | M.TypeNode t -> t.Value.FullName = dictionary
                | _ -> false)
        for intf in [ "System.Collections.Generic.IDictionary`2"; "System.Collections.Generic.IReadOnlyDictionary`2" ] do
            for name in [ "TryGetValue"; "ContainsKey"; "get_Item"; "get_Keys"; "get_Values" ] do
                let abstractNode =
                    graph.Nodes |> Seq.tryFind (function
                        | M.AbstractMethodNode (t, m) -> t.Value.FullName = intf && m.Value.MethodName = name
                        | _ -> false)
                match abstractNode with
                // the node is added to the StdLib graph by the Dictionary implementation
                | None -> Assert.Fail(sprintf "Dictionary does not register an implementation of %s.%s" intf name)
                | Some abstractNode ->
                    // a bundle that only calls the interface member on a Dictionary
                    let kept =
                        graph.GetDependencies [ typeNode; abstractNode ]
                        |> List.exists (function
                            | M.MethodNode (t, m) -> t.Value.FullName = dictionary && m.Value.MethodName = name
                            | _ -> false)
                    Assert.IsTrue(kept, sprintf "Dictionary.%s is eliminated when only %s.%s is called" name intf name)
