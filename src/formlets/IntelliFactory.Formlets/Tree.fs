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

namespace IntelliFactory.Formlets.Base

/// Defines a binary tree structure and its associated operations.
/// In particular, defines a derivative type to describe tree edits.
module Tree =
    open System.Collections
    open System.Collections.Generic

    type Tree<'T> =
        | Empty
        | Leaf of 'T
        | Fork of Tree<'T> * Tree<'T>

        [<ReflectedDefinition>]
        member this.Sequence =
            match this with
            | Empty -> Seq.empty
            | Leaf x -> Seq.singleton x
            | Fork (x, y) -> Seq.append x.Sequence y.Sequence

        interface IEnumerable with
            [<ReflectedDefinition>]
            member this.GetEnumerator() =
                this.Sequence.GetEnumerator() :> _

        interface IEnumerable<'T> with
            [<ReflectedDefinition>]
            member this.GetEnumerator() =
                this.Sequence.GetEnumerator()

        [<ReflectedDefinition>]
        member this.Map (f: 'T -> 'U) =
            match this with
            | Empty -> Empty
            | Leaf t -> Leaf (f t)
            | Fork (left, right) ->
                Fork (left.Map f, right.Map f)

    type Edit<'T> =
        | Replace of Tree<'T>
        | Left of Edit<'T>
        | Right of Edit<'T>

        [<ReflectedDefinition>]
        member this.Sequence =
            match this with
            | Replace tree -> tree.Sequence
            | Left edit -> edit.Sequence
            | Right edit -> edit.Sequence

        interface IEnumerable with
            [<ReflectedDefinition>]
            member this.GetEnumerator() =
                this.Sequence.GetEnumerator() :> _

        interface IEnumerable<'T> with
            [<ReflectedDefinition>]
            member this.GetEnumerator() =
                this.Sequence.GetEnumerator()

    [<ReflectedDefinition>]
    let ShowEdit (edit: Edit<'T>) : string =
        let rec showE (edit: Edit<'T>)=
            match edit with
            | Replace _ -> "Replace"
            | Left l    ->
                "Left > " + (showE l)
            | Right r   ->
                "Right > " + (showE r)
        showE edit

    [<ReflectedDefinition>]
    let Count (t: Tree<'T>) : int =
        let rec count n (t: list<Tree<'T>>) = function
            | Fork (a, b) ->
                count n (b :: t) a
            | tree ->
                let k =
                    match tree with
                    | Empty -> 0
                    | _     -> 1
                match t with
                | []      -> n + k
                | t :: ts -> count (n + k) ts t
        count 0 [] t

    [<ReflectedDefinition>]
    let Range (edit: Edit<'T>) (input: Tree<'T>) : int * int =
        let rec range (edit: Edit<'T>) (input: Tree<'T>) offset =
            match edit with
            | Replace output ->
                (offset, Count input)
            | Left edit ->
                match input with
                | Fork (l, r) -> range edit l offset
                | _ -> range edit Empty offset
            | Right edit ->
                match input with
                | Fork (l, r) -> range edit r (offset + Count l)
                | tree -> range edit Empty (offset + Count tree)
        range edit input 0


    // Creates a tree from a sequence of trees.
    [<ReflectedDefinition>]
    let FromSequence (vs: seq<'T>) =
        (Tree.Empty, vs)
        ||> Seq.fold (fun state v ->
            Fork (state, Leaf v)
        )

    // Returns the tree replaced by applying the edit operation.
    [<ReflectedDefinition>]
    let rec ReplacedTree (edit: Edit<'T>) (input: Tree<'T>) =
        match edit with
        | Replace output ->
            input
        | Left edit ->
            match input with
            | Fork (l, r) ->
                ReplacedTree edit l
            | tree ->
                ReplacedTree (Left edit) (Fork (Empty, tree))
        | Right edit ->
            match input with
            | Fork (l, r) ->
                ReplacedTree edit r
            | tree ->
                ReplacedTree (Right edit) (Fork (tree, Empty))


    [<ReflectedDefinition>]
    let Apply (edit: Edit<'T>) (input: Tree<'T>) : Tree<'T> =
        let rec apply (edit: Edit<'T>) input =
            match edit with
            | Replace output ->
                 output
            | Left edit ->
                match input with
                | Fork (l, r) ->
                    Fork (apply edit l, r)
                | tree ->
                    apply (Left edit) (Fork (Empty, tree))
            | Right edit ->
                match input with
                | Fork (l, r) ->
                    Fork (l, apply edit r)
                | tree ->
                    apply (Right edit) (Fork (tree, Empty))
        apply edit input

    [<ReflectedDefinition>]
    let Set (value: 'T) = Replace (Leaf value)

    [<ReflectedDefinition>]
    let rec Transform (f: Tree<'T> -> Tree<'U>) (edit: Edit<'T>) : Edit<'U> =
        match edit with
        | Replace t -> Replace (f t)
        | Left e    -> Left <| Transform f e
        | Right e   -> Right <| Transform f e

    [<ReflectedDefinition>]
    let Delete<'T> () : Edit<'T> = Replace Empty

    [<ReflectedDefinition>]
    let FlipEdit edit =
        match edit with
        | Replace t -> Replace t
        | Left e    -> Right e
        | Right e   -> Left e

    [<ReflectedDefinition>]
    let rec DeepFlipEdit edit =
        match edit with
        | Replace t -> Replace t
        | Left e    -> Right (DeepFlipEdit e)
        | Right e   -> Left (DeepFlipEdit e)
