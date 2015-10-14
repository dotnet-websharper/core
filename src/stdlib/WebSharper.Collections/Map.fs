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

open System.Collections
open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
open WebSharper.Collections
module T = BalancedTree

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
            tree |> T.Contains {Key=k; Value = JS.Undefined}

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
            As (FSharpMap(tree |> T.Remove {Key=k; Value=JS.Undefined}))

        [<JavaScript>]
        member this.TryFind(k: 'K) =
            tree
            |> T.TryFind {Key=k; Value=JS.Undefined}
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
            member this.GetEnumerator() = this.GetEnumerator() :> _

        interface IEnumerable<KeyValuePair<'K,'V>> with
            member this.GetEnumerator() = this.GetEnumerator()



