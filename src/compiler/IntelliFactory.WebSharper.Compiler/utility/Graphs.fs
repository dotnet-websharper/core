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

module IntelliFactory.WebSharper.Compiler.Graphs

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
        Index : int
        Value : 'T
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

    member this.Add node =
        ignore (GetOrAdd this.Lookup node)

    member this.Contains x =
        this.Lookup.ContainsKey x

    member this.NodeCount =
        this.Lookup.Count

    member this.Nodes =
        Seq.toList this.Lookup.Keys

    member this.Connect src dst =
        let gr = this.Lookup
        let srcNode = GetOrAdd gr src
        let dstNode = GetOrAdd gr dst
        DSet.Insert dstNode srcNode.Dependencies

    member this.Links node =
        match this.Lookup.TryGetValue node with
        | true, node -> Seq.map (fun x -> x.Value) (DSet.Iterate node.Dependencies)
        | _ -> Seq.empty
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
