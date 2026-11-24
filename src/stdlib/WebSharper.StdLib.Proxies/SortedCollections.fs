// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
open System.Collections
open System.Collections.Generic

/// The sorted collections keep their items in a JS array in comparer order; lookups are binary searches.
[<JavaScript>]
module internal SortedUtil =

    /// The index of `key` in `arr` (by `cmp` on `keyOf` of the items), or the bitwise complement of its insertion point
    let search (arr: Array<'T>) (keyOf: 'T -> 'K) (cmp: 'K -> 'K -> int) (key: 'K) =
        let mutable lo = 0
        let mutable hi = arr.Length - 1
        let mutable res = -1
        while res < 0 && lo <= hi do
            let mid = (lo + hi) >>> 1
            let c = cmp (keyOf arr.[mid]) key
            if c = 0 then res <- mid
            elif c < 0 then lo <- mid + 1
            else hi <- mid - 1
        if res >= 0 then res else ~~~lo

    let comparerFn (c: IComparer<'K>) = fun (x: 'K) (y: 'K) -> c.Compare(x, y)

open SortedUtil

type private SD<'K,'V> = SortedDictionary<'K,'V>

// The key and value collections are arrays (snapshots); arrays enumerate as sequences.
[<Proxy(typeof<SD<_,_>.KeyCollection>)>]
[<Stub>]
type private SortedKeyCollectionProxy<'K,'V> =
    member this.Count with [<Inline "$this.length">] get () = X<int>
    [<Inline>]
    member this.GetEnumerator() = (As<seq<'K>> this).GetEnumerator()

[<Proxy(typeof<SD<_,_>.ValueCollection>)>]
[<Stub>]
type private SortedValueCollectionProxy<'K,'V> =
    member this.Count with [<Inline "$this.length">] get () = X<int>
    [<Inline>]
    member this.GetEnumerator() = (As<seq<'V>> this).GetEnumerator()

[<Proxy(typeof<SD<_,_>.Enumerator>)>]
[<Stub>]
type private SortedDictionaryEnumeratorProxy<'K,'V> [<JavaScript(false)>] () =
    member this.Current with get() = X<KeyValuePair<'K,'V>>
    member this.MoveNext() = X<bool>
    member this.Dispose() = ()

[<Proxy(typeof<SD<_,_>>)>]
[<Name "SortedDictionary">]
type internal SortedDictionaryProxy<'K,'V when 'K : comparison>
    private (init: seq<KeyValuePair<'K,'V>>, cmp: 'K -> 'K -> int) =

    let items = Array<KeyValuePair<'K,'V>>()
    let find k = search items (fun (kv: KeyValuePair<'K,'V>) -> kv.Key) cmp k

    let set (k: 'K) (v: 'V) (add: bool) =
        let i = find k
        if i >= 0 then
            if add then raise (System.ArgumentException("An item with the same key has already been added."))
            items.[i] <- KeyValuePair(k, v)
        else items.Splice(~~~i, 0, KeyValuePair(k, v)) |> ignore

    do for kv in init do set kv.Key kv.Value true

    new () = SortedDictionaryProxy<'K,'V>(Seq.empty, compare)
    new (comparer: IComparer<'K>) = SortedDictionaryProxy<'K,'V>(Seq.empty, comparerFn comparer)
    new (dictionary: IDictionary<'K,'V>) = SortedDictionaryProxy<'K,'V>(dictionary, compare)
    new (dictionary: IDictionary<'K,'V>, comparer: IComparer<'K>) =
        SortedDictionaryProxy<'K,'V>(dictionary, comparerFn comparer)

    member this.Count = items.Length

    member this.Item
        with get (k: 'K) : 'V =
            let i = find k
            if i < 0 then raise (KeyNotFoundException()) else items.[i].Value
        and set (k: 'K) (v: 'V) = set k v false

    member this.Add(k: 'K, v: 'V) = set k v true

    member this.ContainsKey(k: 'K) = find k >= 0

    member this.ContainsValue(v: 'V) = items.Self |> Array.exists (fun kv -> Unchecked.equals kv.Value v)

    member this.Remove(k: 'K) =
        let i = find k
        if i < 0 then false else
            items.Splice(i, 1) |> ignore
            true

    member this.TryGetValue(k: 'K, [<System.Runtime.InteropServices.Out>] res: byref<'V>) =
        let i = find k
        if i < 0 then false else
            res <- items.[i].Value
            true

    member this.Clear() = items.Splice(0, items.Length) |> ignore

    member this.Keys = As<SD<'K,'V>.KeyCollection> (items.Self |> Array.map (fun kv -> kv.Key))

    member this.Values = As<SD<'K,'V>.ValueCollection> (items.Self |> Array.map (fun kv -> kv.Value))

    member this.GetEnumerator() =
        As<SD<'K,'V>.Enumerator> ((As<seq<KeyValuePair<'K,'V>>> (items.Self |> Array.copy)).GetEnumerator())

    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

    interface IEnumerable<KeyValuePair<'K,'V>> with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

[<Proxy(typeof<SortedSet<_>.Enumerator>)>]
[<Stub>]
type private SortedSetEnumeratorProxy<'T> [<JavaScript(false)>] () =
    member this.Current with get() = X<'T>
    member this.MoveNext() = X<bool>
    member this.Dispose() = ()

[<Proxy(typeof<SortedSet<_>>)>]
[<Name "SortedSet">]
type internal SortedSetProxy<'T when 'T : comparison>
    private (init: seq<'T>, cmp: 'T -> 'T -> int) =

    let items = Array<'T>()
    let find x = search items id cmp x

    let add (x: 'T) =
        let i = find x
        if i >= 0 then false else
            items.Splice(~~~i, 0, x) |> ignore
            true

    do for x in init do add x |> ignore

    new () = SortedSetProxy<'T>(Seq.empty, compare)
    new (comparer: IComparer<'T>) = SortedSetProxy<'T>(Seq.empty, comparerFn comparer)
    new (collection: seq<'T>) = SortedSetProxy<'T>(collection, compare)
    new (collection: seq<'T>, comparer: IComparer<'T>) = SortedSetProxy<'T>(collection, comparerFn comparer)

    member this.Count = items.Length

    member this.Add(x: 'T) = add x

    member this.Contains(x: 'T) = find x >= 0

    member this.Remove(x: 'T) =
        let i = find x
        if i < 0 then false else
            items.Splice(i, 1) |> ignore
            true

    member this.Clear() = items.Splice(0, items.Length) |> ignore

    member this.Min = if items.Length = 0 then As<'T> null else items.[0]

    member this.Max = if items.Length = 0 then As<'T> null else items.[items.Length - 1]

    member this.UnionWith(xs: seq<'T>) = for x in xs do add x |> ignore

    member this.ExceptWith(xs: seq<'T>) = for x in xs do this.Remove x |> ignore

    member this.Reverse() = As<seq<'T>> (items.Self |> Array.rev)

    member this.GetEnumerator() =
        As<SortedSet<'T>.Enumerator> ((As<seq<'T>> (items.Self |> Array.copy)).GetEnumerator())

    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

    interface IEnumerable<'T> with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>
