// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

/// Implements a proxy for the F# Map module.
[<Proxy "Microsoft.FSharp.Collections.MapModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
[<Name "Map">]
module internal MapModule =
    module T = BalancedTree

    [<Inline>]
    let private ToTree (m: Map<'K,'V>) =
        (As<FSharpMap<'K,'V>> m).Tree

    [<Inline>]
    let private OfTree (t: T.Tree<_>) : Map<'K,'V> =
        As (new FSharpMap<'K,'V>(t))

    [<Inline>]
    let Add k v (m: Map<'K,'V>) : Map<'K,'V> = m.Add(k, v)

    [<Inline>]
    let ContainsKey k (m: Map<'K,'V>) : bool = m.ContainsKey k

    [<Inline>]
    let Empty<'K,'V when 'K : comparison> : Map<'K,'V> = new Map<_,_>([||])

    let Exists (f: 'K -> 'V -> bool) (m: Map<'K,'V>) : bool =
        m |> Seq.exists (fun kv -> f kv.Key kv.Value)

    let Filter (f: 'K -> 'V -> bool) (m: Map<'K,'V>) : Map<'K,'V> =
        T.Ascend (ToTree m)
        |> Seq.filter (fun kv -> f kv.Key kv.Value)
        |> Seq.toArray
        |> T.OfSorted
        |> OfTree

    [<Inline>]
    let Find (k: 'K) (m: Map<'K,'V>) : 'V = m.[k]

    let FindKey (f: 'K -> 'T -> bool) (m: Map<'K,'T>) : 'K =
        m 
        |> Seq.pick (fun kv -> 
            if f kv.Key kv.Value then Some kv.Key else None)

    let rec Fold<'K,'V,'S when 'K : comparison>
        (f: 'S -> 'K -> 'V -> 'S) (s: 'S) (m: Map<'K,'V>) : 'S =
            T.Ascend (ToTree m)
            |> Seq.fold (fun s kv -> f s kv.Key kv.Value) s

    let rec FoldBack (f: 'K -> 'V -> 'S -> 'S) (m: Map<'K,'V>) (s: 'S) : 'S =
        T.Descend (ToTree m)
        |> Seq.fold (fun s kv -> f kv.Key kv.Value s) s

    let rec ForAll (f: 'K -> 'V -> bool) (m: Map<'K, 'V>) : bool =
        m |> Seq.forall (fun kv -> f kv.Key kv.Value)

    [<Inline>]
    let IsEmpty (m: Map<'K, 'V>) : bool = m.IsEmpty

    let rec Iterate (f: 'K -> 'V -> unit) (m: Map<'K, 'V>) : unit =
        m |> Seq.iter (fun kv -> f kv.Key kv.Value)

    let OfArray (a: ('K * 'V) []) : Map<'K,'V> =
        a
        |> Seq.map (fun (k, v) -> {Key = k; Value = v} : Pair<_,_>)
        |> T.OfSeq
        |> OfTree

    [<Inline>]
    let OfList (kvs: list<'K * 'V>) : Map<'K,'V> = Map.ofSeq kvs

    [<Inline>]
    let OfSeq (s: seq<'K * 'V>) : Map<'K, 'V> =
        Map.ofArray (Seq.toArray s)

    let Partition (f: 'K -> 'V -> bool) (m: Map<'K,'V>) : Map<'K,'V> * Map<'K,'V> =
        let (x, y) =
            Seq.toArray (T.Ascend (ToTree m))
            |> Array.partition (fun kv -> f kv.Key kv.Value)
        (OfTree (T.OfSorted x), OfTree (T.OfSorted y))

    let Pick (f: 'K -> 'V -> option<'T>) (m: Map<'K, 'V>) : 'T =
        m |> Seq.pick (fun kv -> f kv.Key kv.Value)

    [<Inline>]
    let Remove (k: 'K) (m: Map<'K, 'V>) : Map<'K, 'V> = m.Remove k

    [<Inline>]
    let ToArray (m: Map<'K, 'V>) : array<'K * 'V> = Seq.toArray (Map.toSeq m)

    [<Inline>]
    let ToList (m: Map<'K, 'V>) : list<'K * 'V> = Seq.toList (Map.toSeq m)

    let ToSeq (m: Map<'K, 'V>) : seq<'K * 'V> =
        T.Ascend (ToTree m)
        |> Seq.map (fun kv -> (kv.Key, kv.Value))

    let TryFind (k: 'K) (m: Map<'K, 'V>) : option<'V> = m.TryFind k

    let TryFindKey (f: 'K -> 'V -> bool) (m: Map<'K,'V>) : option<'K> =
        m |> Seq.tryPick (fun kv ->
            if f kv.Key kv.Value then Some kv.Key else None)

    let rec TryPick (f: 'K -> 'V -> option<'T>) (m: Map<'K, 'V>) : option<'T> =
        m |> Seq.tryPick (fun kv -> f kv.Key kv.Value)
         
    let rec Map (f: 'K -> 'V -> 'T) (m: Map<'K,'V>) : Map<'K,'T> =
        T.Ascend (ToTree m)
        |> Seq.map (fun kv -> 
            {Key = kv.Key; Value = f kv.Key kv.Value} : Pair<_,_>)
        |> T.OfSeq
        |> OfTree

    let Keys (m: Map<'K, 'V>) : System.Collections.Generic.ICollection<'K> = m.Keys

    let Values (m: Map<'K, 'V>) : System.Collections.Generic.ICollection<'V> = m.Values
