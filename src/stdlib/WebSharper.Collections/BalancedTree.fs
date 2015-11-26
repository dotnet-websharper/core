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

namespace WebSharper.Collections

open WebSharper.JavaScript

/// Provides balanced binary search tree operations.
module internal BalancedTree =
    open WebSharper

    /// Represents a binary balanced search tree, preserving the balance
    /// invariant: the heights of the branches differ by at most 1.
    [<JavaScript>]
    type Tree<'T when 'T : comparison> =
        private {
            Node    : 'T
            Left    : Tree<'T>
            Right   : Tree<'T>
            Height  : int
            Count   : int
        }

    [<Inline "null">]
    let Empty<'T when 'T : comparison> = X<Tree<'T>>

    [<Inline "$tree == null">]
    let IsEmpty (tree: Tree<'T>) = X<bool>

    [<Inline "$tree == null ? 0 : $tree.Height">]
    let Height (tree: Tree<'T>) = X<int>
            
    [<Inline "$tree == null ? 0 : $tree.Count">]
    let Count (tree: Tree<'T>) = X<int>

    [<Inline "$t.Left">]
    let Left (t: Tree<'T>) = X<Tree<'T>>

    [<Inline "$t.Right">]
    let Right (t: Tree<'T>) = X<Tree<'T>>

    [<Inline "$t.Node">]
    let Node (t: Tree<'T>) = X<'T>

    [<JavaScript>]
    let Branch node left right =
        {
            Node    = node
            Left    = left
            Right   = right
            Height  = 1 + max (Height left) (Height right)
            Count   = 1 + Count left + Count right
        }

    [<JavaScript>]
    let Enumerate flip (t: Tree<'T>) : seq<'T> =
        let rec gen (t, spine: list<'T * Tree<'T>>) =
            if IsEmpty t then
                match spine with
                | []                    -> None
                | (t, other) :: spine   -> Some (t, (other, spine))
            else
                if flip
                then gen (Right t, (Node t, Left t) :: spine)
                else gen (Left t, (Node t, Right t) :: spine)
        Seq.unfold gen (t, [])

    /// Traverses the tree in ascending order.
    [<Inline>]
    [<JavaScript>]
    let Ascend t = Enumerate false t

    /// Traverses the tree in descending order.
    [<Inline>]
    [<JavaScript>]
    let Descend t = Enumerate true t

    /// Builds a tree from sorted input and the indices of the
    /// first and the last elements to include.
    [<JavaScript>]
    let rec private Build (data: 'T []) min max : Tree<'T> =
        let sz = max - min + 1
        if sz <= 0 then
            Empty
        else
            let center = (min + max) / 2
            let left   = Build data min (center - 1)
            let right  = Build data (center + 1) max
            Branch data.[center] left right

    /// Quickly constructs a tree from a sorted, distinct array.
    [<Inline>]
    [<JavaScript>]
    let OfSorted (data: 'T []) : Tree<'T> =
        Build data 0 (Array.length data - 1)

    [<JavaScript>]
    let OfSeq (data: seq<'T>) : Tree<'T> =
        OfSorted (Array.sort (Seq.toArray (Seq.distinct data)))

    [<Inline "$x.unshift($y)">]
    let private unshift (x: 'T) y = X<unit>

    /// Unzips a tree into a matching node and a spine.
    [<JavaScript>]
    let Lookup (k: 'T) (t: Tree<'T>) =
        let mutable spine = [||]
        let mutable t = t
        let mutable loop = true
        while loop do
            if IsEmpty t then loop <- false else
                match compare k t.Node with
                | 0 -> loop <- false
                | 1 ->
                    unshift spine (true, t.Node, t.Left)
                    t <- t.Right
                | _ ->
                    unshift spine (false, t.Node, t.Right)
                    t <- t.Left
        (t, spine)

    /// Rebuilds an unzipped tree by going up the spine and performing
    /// rotations where necessary for balance.
    [<JavaScript>]
    let private Rebuild<'T when 'T : comparison>
            (spine: (bool * 'T * Tree<'T>) []) (t: Tree<'T>) : Tree<'T> =
        let h (x: Tree<'T>) = Height x
        let mutable t = t
        for i = 0 to spine.Length - 1 do
            t <- (
                match spine.[i] with
                | false, x, r ->
                    if h t > h r + 1 then
                        if h t.Right = h t.Left + 1 then
                            // Double rotation:
                            let m = t.Right
                            Branch  m.Node
                                    (Branch t.Node t.Left m.Left)
                                    (Branch x m.Right r)
                        else
                            // Single rotation:
                            Branch  t.Node
                                    t.Left
                                    (Branch x t.Right r)
                    else
                        // No rotation:
                        Branch x t r
                | true, x, l ->
                    if h t > h l + 1 then
                        if h t.Left = h t.Right + 1 then
                            // Double rotation:
                            let m = t.Left
                            Branch m.Node
                                    (Branch x l m.Left)
                                    (Branch t.Node m.Right t.Right)
                        else
                            // Single rotation:
                            Branch t.Node
                                    (Branch x l t.Left)
                                    t.Right
                    else
                        // No rotation:
                        Branch x l t
            )
        t

    /// Inserts or updates a node in the tree. If a matching node is found,
    /// it is replaced with the value of "combine old new".
    [<JavaScript>]
    let Put<'T when 'T : comparison> combine k (t: Tree<'T>) : Tree<'T> =
        let (t, spine) = Lookup k t
        if IsEmpty t then
            Rebuild spine (Branch k Empty Empty)
        else
            Rebuild spine (Branch (combine t.Node k) t.Left t.Right)

    /// Removes a node from the tree.
    [<JavaScript>]
    let Remove k (src: Tree<'T>) =
        let (t, spine) = Lookup k src
        if IsEmpty t then
            src
        else
            if IsEmpty t.Right then
                Rebuild spine t.Left
            elif IsEmpty t.Left then
                Rebuild spine t.Right
            else
                Seq.append (Ascend t.Left) (Ascend t.Right)
                |> Seq.toArray
                |> OfSorted
                |> Rebuild spine

    /// Adds a node into the tree, replacing an existing one if found.
    [<JavaScript>]
    let Add<'T when 'T : comparison> (x: 'T) (t: Tree<'T>) : Tree<'T> =
        Put (fun _ x -> x) x t

    /// Checks if a tree contains a given key.
    [<JavaScript>]
    let rec Contains (v: 'T) (t: Tree<'T>) : bool =
        not (IsEmpty (fst (Lookup v t)))

    /// Looks up a node by key.
    [<JavaScript>]
    let TryFind (v: 'T) (t: Tree<'T>) =
        let x = fst (Lookup v t)
        if IsEmpty x then None else Some x.Node






