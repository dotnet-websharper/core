// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

open IntelliFactory.WebSharper

/// Implements a proxy for the F# Map module.
[<Proxy "Microsoft.FSharp.Collections.MapModule, \
    FSharp.Core, Version=2.0.0.0, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
module internal MapModule =
    module T = BalancedTree

    [<Inline>]
    [<JavaScript>]
    let private ToTree (m: Map<'K,'V>) =
        (As<FSharpMap<'K,'V>> m).Tree

    [<Inline>]
    [<JavaScript>]
    let private OfTree (t: T.Tree<_>) =
        As<Map<'K,'V>> (new FSharpMap<'K,'V>(t))

    [<Inline>]
    [<JavaScript>]
    let Add k v (m: Map<'K,'V>) = m.Add(k, v)

    [<Inline>]
    [<JavaScript>]
    let ContainsKey k (m: Map<'K,'V>) = m.ContainsKey k

    [<Inline>]
    [<JavaScript>]
    let Empty<'K,'V> = new Map<_,_>([||])

    [<JavaScript>]
    let Exists (f: 'K -> 'V -> bool) (m: Map<'K,'V>) =
        m |> Seq.exists (fun kv -> f kv.Key kv.Value)

    [<JavaScript>]
    let Filter (f: 'K -> 'V -> bool) (m: Map<'K,'V>) =
        T.Ascend (ToTree m)
        |> Seq.filter (fun kv -> f kv.Key kv.Value)
        |> Seq.toArray
        |> T.OfSorted
        |> OfTree

    [<Inline>]
    [<JavaScript>]
    let Find (k: 'K) (m: Map<'K,'V>) : 'V = m.[k]

    [<JavaScript>]
    let FindKey (f: 'K -> 'T -> bool) (m: Map<'K,'T>) =
        m 
        |> Seq.pick (fun kv -> 
            if f kv.Key kv.Value then Some kv.Key else None)

    [<JavaScript>]
    let rec Fold<'K,'V,'S when 'K : comparison>
        (f: 'S -> 'K -> 'V -> 'S) (s: 'S) (m: Map<'K,'V>) : 'S =
            T.Ascend (ToTree m)
            |> Seq.fold (fun s kv -> f s kv.Key kv.Value) s

    [<JavaScript>]
    let rec FoldBack (f: 'K -> 'V -> 'S -> 'S) (m: Map<'K,'V>) (s: 'S) : 'S =
        T.Descend (ToTree m)
        |> Seq.fold (fun s kv -> f kv.Key kv.Value s) s

    [<JavaScript>]
    let rec ForAll (f: 'K -> 'V -> bool) (m: Map<'K, 'V>) : bool =
        m |> Seq.forall (fun kv -> f kv.Key kv.Value)

    [<Inline>]
    [<JavaScript>]
    let IsEmpty (m: Map<'K, 'V>) = m.IsEmpty

    [<JavaScript>]
    let rec Iterate (f: 'K -> 'V -> unit) (m: Map<'K, 'V>) =
        m |> Seq.iter (fun kv -> f kv.Key kv.Value)

    [<JavaScript>]
    let OfArray (a: ('K * 'V) []) =
        a
        |> Seq.map (fun (k, v) -> {Key = k; Value = v} : Pair<_,_>)
        |> T.OfSeq
        |> OfTree

    [<Inline>]
    [<JavaScript>]
    let OfList (kvs: list<'K * 'V>) = Map.ofSeq kvs

    [<Inline>]
    [<JavaScript>]
    let OfSeq (s: seq<'K * 'V>) : Map<'K, 'V> =
        Map.ofArray (Seq.toArray s)

    [<JavaScript>]
    let Partition (f: 'K -> 'V -> bool) (m: Map<'K,'V>) =
        let (x, y) =
            Seq.toArray (T.Ascend (ToTree m))
            |> Array.partition (fun kv -> f kv.Key kv.Value)
        (OfTree (T.OfSorted x), OfTree (T.OfSorted y))

    [<JavaScript>]
    let Pick (f: 'K -> 'V -> option<'T>) (m: Map<'K, 'V>) : 'T =
        m |> Seq.pick (fun kv -> f kv.Key kv.Value)

    [<Inline>]
    [<JavaScript>]
    let Remove (k: 'K) (m: Map<'K, 'V>) : Map<'K, 'V> = m.Remove k

    [<Inline>]
    [<JavaScript>]
    let ToArray (m: Map<'K, 'V>) = Seq.toArray (Map.toSeq m)

    [<Inline>]
    [<JavaScript>]
    let ToList (m: Map<'K, 'V>) = Seq.toList (Map.toSeq m)

    [<JavaScript>]
    let ToSeq (m: Map<'K, 'V>) =
        T.Ascend (ToTree m)
        |> Seq.map (fun kv -> (kv.Key, kv.Value))

    [<JavaScript>]
    let TryFind (k: 'K) (m: Map<'K, 'V>) = m.TryFind k

    [<JavaScript>]
    let TryFindKey f (m: Map<_,_>) =
        m |> Seq.tryFind (fun kv -> f kv.Key kv.Value)

    [<JavaScript>]
    let rec TryPick (f: 'K -> 'V -> option<'T>) (m: Map<'K, 'V>) =
        m |> Seq.tryPick (fun kv -> f kv.Key kv.Value)

    [<JavaScript>]
    let rec Map (f: 'K -> 'V -> 'T) (m: Map<'K,'V>) =
        T.Ascend (ToTree m)
        |> Seq.map (fun kv -> 
            {Key = kv.Key; Value = f kv.Key kv.Value} : Pair<_,_>)
        |> T.OfSeq
        |> OfTree

