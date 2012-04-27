// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Graphs

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type Queue<'T> = System.Collections.Generic.Queue<'T>

module DSet =
    type T<'T> = Dictionary<'T,unit>
    let inline Count (d: T<'T>) = d.Count
    let inline Empty<'T when 'T : equality> : T<'T> = Dictionary()
    let inline Iterate (d: T<'T>) = d.Keys :> seq<_>
    let inline Insert v (d: T<'T>) = d.[v] <- ()

[<CustomEquality>]
[<NoComparison>]
type Node<'T> =
    {
        Index        : int
        Value        : 'T
        Dependencies : DSet.T<Node<'T>>
    }

    override this.GetHashCode() =
        this.Index

    override this.Equals other =
        match other with
        | :? Node<'T> as other -> this.Index = other.Index
        | _ -> false

let NewNode index value =
    {
        Index = index
        Value = value
        Dependencies = DSet.Empty
    }

type NodeLookup<'T> =
    Dictionary<'T, Node<'T>>

let inline GetOrAdd (lookup: NodeLookup<_>) v =
    match lookup.TryGetValue v with
    | true, node -> node
    | false, _ ->
        let node = NewNode lookup.Count v
        lookup.Add(v, node)
        node

type Graph<'T> =
    { Lookup : NodeLookup<'T> }

    member this.Add node    = ignore (GetOrAdd this.Lookup node)
    member this.Contains x  = this.Lookup.ContainsKey x
    member this.NodeCount   = this.Lookup.Count
    member this.Nodes       = Seq.toList this.Lookup.Keys

    member this.Connect src dst =
        let gr      = this.Lookup
        let srcNode = GetOrAdd gr src
        let dstNode = GetOrAdd gr dst
        DSet.Insert dstNode srcNode.Dependencies

    member this.Links node =
        match this.Lookup.TryGetValue node with
        | true, node -> Seq.map (fun x -> x.Value) (DSet.Iterate node.Dependencies)
        | _          -> Seq.empty
        |> Seq.toList

    member this.Walk node =
        let graph = this.Lookup
        match graph.TryGetValue node with
        | true, node ->
            let visited = Array.zeroCreate<bool> graph.Count
            let q = Queue()
            let rec visit (node: Node<_>) =
                if not (visited.[node.Index]) then
                    visited.[node.Index] <- true
                    for target in DSet.Iterate node.Dependencies do
                        visit target
                    q.Enqueue node.Value
            visit node
            q :> seq<_>
        | _ ->
            Seq.empty
        |> Seq.toList

let TopologicalSort { Lookup = graph } =
    let visited = Array.zeroCreate<bool> graph.Count
    let q = Queue()
    let rec visit (node: Node<_>) =
        if not (visited.[node.Index]) then
            visited.[node.Index] <- true
            for target in DSet.Iterate node.Dependencies do
                visit target
            q.Enqueue node.Value
    for node in graph.Values do
        visit node
    Seq.toList q

let Empty<'T when 'T : equality> : Graph<'T> =
    { Lookup = Dictionary() }

let New (nodes: seq<'T>) (links: seq<'T * 'T>) : Graph<'T> =
    let lookup = Dictionary()
    for node in nodes do
        let n = NewNode lookup.Count node
        lookup.Add(node, n)

    for s, t in links do
        let src = lookup.[s]
        let dst = lookup.[t]
        DSet.Insert dst src.Dependencies
    { Lookup = lookup }

let Union (grs: seq<Graph<'T>>) : Graph<'T> =
    let lookup = Dictionary()
    let getOrAdd = GetOrAdd lookup // bind global getOrAdd to local lookup
    for { Lookup = gr } in grs do
        for node in gr.Values do
            let src = getOrAdd node.Value
            for dep in DSet.Iterate node.Dependencies do
                let dst = getOrAdd dep.Value
                DSet.Insert dst src.Dependencies
    { Lookup = lookup }

