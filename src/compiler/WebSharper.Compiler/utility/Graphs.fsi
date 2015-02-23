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

/// Provides simple graph algorithms for compiler use.
module internal WebSharper.Compiler.Graphs

/// Represents a directed graph with labeled nodes.
[<Sealed>]
type Graph<'T when 'T : equality> =

    /// Returns the number of nodes.
    member NodeCount : int

    /// Traverses all nodes.
    member Nodes : list<'T>

    /// Looks up all outbound links from a given node.
    member Links : 'T -> list<'T>

    /// Tests if a given node is contained in the graph.
    member Contains : 'T -> bool

    /// Adds a link from source to destination node.
    member Connect : 'T -> 'T -> unit

    /// Adds a node to the graph.
    member Add : 'T -> unit

    /// Traverses all nodes reachable from a given starting point.
    member Walk : 'T -> list<'T>

/// Takes a union of several graphs.
val Union : seq<Graph<'T>> -> Graph<'T>

/// Constructs a graph from an explicit listing of nodes and links.
val New : seq<'T> -> seq<'T * 'T> -> Graph<'T>

/// The empty graph.
val Empty<'T when 'T : equality> : Graph<'T>

/// Returns all nodes in traversal order.
val TopologicalSort : Graph<'T> -> list<'T>
