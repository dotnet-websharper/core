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

namespace IntelliFactory.WebSharper.Collections

open System.Collections
open System.Collections.Generic
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Collections
module T = BalancedTree
module J = JavaScript

[<AutoOpen>]
module private MapUtil =

    [<JavaScript>]
    let fromSeq(s: seq<_>) =
        let a : Pair<_,_> [] =
            [| for (k, v) in Seq.distinctBy fst s ->
                { Key = k; Value = v } |]
        Array.sortInPlace a
        T.OfSorted a

/// Implements a proxy for the F# Map type.
[<Proxy(typeof<Map<_,_>>)>]
type internal FSharpMap<'K,'V when 'K : comparison>

    [<JavaScript>]
    (tree: T.Tree<Pair<'K,'V>>) =

        [<JavaScript>]
        new (s: seq<_>) = new FSharpMap<_,_>(fromSeq s)

        [<JavaScript>]
        member this.Tree = tree

        [<JavaScript>]
        member this.Add(k: 'K, v: 'V) : Map<'K,'V> =
            As (FSharpMap<'K,'V>(tree |> T.Add {Key=k; Value=v}))

        [<JavaScript>]
        member this.ContainsKey k = 
            tree |> T.Contains {Key=k; Value = J.Undefined}

        [<JavaScript>]
        member this.Count = T.Count tree

        [<JavaScript>]
        member this.IsEmpty = T.IsEmpty tree

        [<JavaScript>]
        member this.Item 
            with get (k: 'K) : 'V =
                match this.TryFind k with
                | Some v    ->v
                | None      ->
                    failwith "The given key was not present in the dictionary."

        [<JavaScript>]
        member this.Remove(k: 'K) : Map<'K,'V> =
            As (FSharpMap(tree |> T.Remove {Key=k; Value=J.Undefined}))

        [<JavaScript>]
        member this.TryFind(k: 'K) =
            tree
            |> T.TryFind {Key=k; Value=J.Undefined}
            |> Option.map (fun kv -> kv.Value)

        [<JavaScript>]
        member this.GetEnumerator() =
            let s =
                T.Ascend tree
                |> Seq.map (fun kv ->
                    new KeyValuePair<_,_>(kv.Key, kv.Value))
            s.GetEnumerator()

        [<JavaScript>]
        override this.GetHashCode() =
            hash (Seq.toArray this)

        [<JavaScript>]
        override this.Equals(other) =
            let other = As<FSharpMap<'K,'V>> other
            this.Count = other.Count
            && Seq.forall2 ( = ) this other

        interface System.IComparable with
            [<JavaScript>]
            member this.CompareTo other =
                Seq.compareWith (fun x y ->
                    compare (As<Pair<'K,'V>> x) (As<Pair<'K,'V>> y))
                    this
                    (As<Map<'K,'V>> other)

        interface IEnumerable with
            member this.GetEnumerator() = X<_>

        interface IEnumerable<KeyValuePair<'K,'V>> with
            member this.GetEnumerator() = X<_>



