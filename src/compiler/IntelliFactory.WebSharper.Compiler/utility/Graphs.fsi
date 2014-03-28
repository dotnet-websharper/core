// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Provides simple graph algorithms for compiler use.
module internal IntelliFactory.WebSharper.Compiler.Graphs

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
