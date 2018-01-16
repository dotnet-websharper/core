// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

open WebSharper
open WebSharper.JavaScript

/// Provides balanced binary search tree operations.
[<JavaScript>]
module internal BalancedTree =

    /// Represents a binary balanced search tree, preserving the balance
    /// invariant: the heights of the branches differ by at most 1.
    type Tree<'T when 'T : comparison> =
        private {
            Node    : 'T
            Left    : Optional<Tree<'T>>
            Right   : Optional<Tree<'T>>
            Height  : int
            Count   : int
        }

    [<Inline>]
    let Empty<'T when 'T : comparison> =
        Optional<Tree<'T>>.Undefined

    [<Inline>]
    let IsEmpty (tree: Optional<Tree<'T>>) =
        Optional.isUndefined tree

    [<Inline>]
    let Height (tree: Optional<Tree<'T>>) =
        match tree with
        | Undefined -> 0
        | Defined t -> t.Height
            
    [<Inline>]
    let Count (tree: Optional<Tree<'T>>) =
        match tree with
        | Undefined -> 0
        | Defined t -> t.Count

    let Branch node left right =
        {
            Node    = node
            Left    = left
            Right   = right
            Height  = 1 + max (Height left) (Height right)
            Count   = 1 + Count left + Count right
        }

    let Enumerate flip (t: Optional<Tree<'T>>) : seq<'T> =
        let rec gen (t, spine: list<'T * Optional<Tree<'T>>>) =
            match t with
            | Undefined ->
                match spine with
                | []                    -> None
                | (t, other) :: spine   -> Some (t, (other, spine))
            | Defined t ->
                if flip
                then gen (t.Right, (t.Node, t.Left) :: spine)
                else gen (t.Left, (t.Node, t.Right) :: spine)
        Seq.unfold gen (t, [])

    /// Traverses the tree in ascending order.
    [<Inline>]
    let Ascend t = Enumerate false t

    /// Traverses the tree in descending order.
    [<Inline>]
    let Descend t = Enumerate true t

    /// Builds a tree from sorted input and the indices of the
    /// first and the last elements to include.
    let rec private Build (data: 'T []) min max : Optional<Tree<'T>> =
        let sz = max - min + 1
        if sz <= 0 then
            Empty
        else
            let center = (min + max) / 2
            let left   = Build data min (center - 1)
            let right  = Build data (center + 1) max
            Defined (Branch data.[center] left right)

    /// Quickly constructs a tree from a sorted, distinct array.
    [<Inline>]
    let OfSorted (data: 'T []) : Optional<Tree<'T>> =
        Build data 0 (Array.length data - 1)

    let OfSeq (data: seq<'T>) : Optional<Tree<'T>> =
        let a = Seq.toArray (Seq.distinct data)
        Array.sortInPlace a
        OfSorted a

    [<Inline "$x.unshift($y)">]
    let private unshift (x: 'T) y = X<unit>

    /// Unzips a tree into a matching node and a spine.
    let Lookup (k: 'T) (t: Optional<Tree<'T>>) =
        let mutable spine = [||]
        let mutable t = t
        let mutable loop = true
        while loop do
            match t with
            | Undefined -> loop <- false
            | Defined t' ->
                match compare k t'.Node with
                | 0 -> loop <- false
                | 1 ->
                    unshift spine (true, t'.Node, t'.Left)
                    t <- t'.Right
                | _ ->
                    unshift spine (false, t'.Node, t'.Right)
                    t <- t'.Left
        (t, spine)

    /// Rebuilds an unzipped tree by going up the spine and performing
    /// rotations where necessary for balance.
    let private Rebuild<'T when 'T : comparison>
            (spine: (bool * 'T * Optional<Tree<'T>>) []) (t: Tree<'T>) : Tree<'T> =
        let h (x: Optional<Tree<'T>>) = Height x
        let mutable t = t
        for i = 0 to spine.Length - 1 do
            t <- (
                match spine.[i] with
                | false, x, r ->
                    if t.Height > h r + 1 then
                        if h t.Right = h t.Left + 1 then
                            // Double rotation:
                            let m = t.Right.Value
                            Branch  m.Node
                                    (Defined (Branch t.Node t.Left m.Left))
                                    (Defined (Branch x m.Right r))
                        else
                            // Single rotation:
                            Branch  t.Node
                                    t.Left
                                    (Defined (Branch x t.Right r))
                    else
                        // No rotation:
                        Branch x (Defined t) r
                | true, x, l ->
                    if t.Height > h l + 1 then
                        if h t.Left = h t.Right + 1 then
                            // Double rotation:
                            let m = t.Left.Value
                            Branch m.Node
                                    (Defined (Branch x l m.Left))
                                    (Defined (Branch t.Node m.Right t.Right))
                        else
                            // Single rotation:
                            Branch t.Node
                                    (Defined (Branch x l t.Left))
                                    t.Right
                    else
                        // No rotation:
                        Branch x l (Defined t)
            )
        t

    let private Rebuild' spine t =
        match t with
        | Undefined -> t
        | Defined t -> Defined (Rebuild spine t)

    /// Inserts or updates a node in the tree. If a matching node is found,
    /// it is replaced with the value of "combine old new".
    let Put<'T when 'T : comparison> combine k (t: Optional<Tree<'T>>) : Tree<'T> =
        let (t, spine) = Lookup k t
        match t with
        | Undefined ->
            Rebuild spine (Branch k Empty Empty)
        | Defined t ->
            Rebuild spine (Branch (combine t.Node k) t.Left t.Right)

    /// Removes a node from the tree.
    let Remove k (src: Optional<Tree<'T>>) =
        let (t, spine) = Lookup k src
        match t with
        | Undefined -> src
        | Defined t ->
            if Optional.isUndefined t.Right then
                Rebuild' spine t.Left
            elif Optional.isUndefined t.Left then
                Rebuild' spine t.Right
            else
                Seq.append (Ascend t.Left) (Ascend t.Right)
                |> Seq.toArray
                |> OfSorted
                |> Rebuild' spine

    /// Adds a node into the tree, replacing an existing one if found.
    let Add<'T when 'T : comparison> (x: 'T) (t: Optional<Tree<'T>>) : Tree<'T> =
        Put (fun _ x -> x) x t

    /// Checks if a tree contains a given key.
    let rec Contains (v: 'T) (t: Optional<Tree<'T>>) : bool =
        not (IsEmpty (fst (Lookup v t)))

    /// Looks up a node by key.
    let TryFind (v: 'T) (t: Optional<Tree<'T>>) =
        match fst (Lookup v t) with
        | Undefined -> None
        | Defined x -> Some x.Node






