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

namespace WebSharper.Collections

open WebSharper
open WebSharper.JavaScript
open System.Collections.Generic

type private KVP<'K,'V> = KeyValuePair<'K,'V>
type private D<'K,'V> = Dictionary<'K,'V>

module internal DictionaryUtil =

    [<JavaScript>]
    let notPresent () =
        failwith "The given key was not present in the dictionary."

    [<Inline "$c.Equals($x, $y)">]
    let equals (c: IEqualityComparer<'T>) x y =
        c.Equals(x, y)

    [<Inline "$c.GetHashCode($x)">]
    let getHashCode (c: IEqualityComparer<'T>) x =
        c.GetHashCode x

open DictionaryUtil

/// Implements a proxy for the .NET dictionary.
[<Name "Dictionary">]
[<Proxy(typeof<D<_,_>>)>]
type internal Dictionary<'K,'V when 'K : equality>

    [<JavaScript>]
    private (init   : seq<KVP<'K,'V>>,
             equals : 'K -> 'K -> bool,
             hash   : 'K -> int) =

        let mutable count = 0
        let mutable data  = obj ()

        [<Inline>]
        [<JavaScript>]
        let h x = As<string> (hash x)

        do for x in init do
            (?<-) data (h x.Key) x.Value

        [<JavaScript>]
        new () = new Dictionary<'K,'V>([||], (=), hash)

        [<JavaScript>]
        new (capacity: int) = new Dictionary<'K,'V>()

        [<JavaScript>]
        new (comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>([||], equals comparer, getHashCode comparer)

        [<JavaScript>]
        new (capacity: int, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(comparer)

        [<JavaScript>]
        new (dictionary: IDictionary<'K,'V>) =
            new Dictionary<'K,'V>(dictionary, (=), hash)

        [<JavaScript>]
        new (dictionary: IDictionary<'K,'V>, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(
                dictionary,
                equals comparer,
                getHashCode comparer
            )

        [<JavaScript>]
        member this.Add(k: 'K, v: 'V) =
            let h = h k
            if JS.HasOwnProperty data h then
                failwith "An item with the same key has already been added."
            else
                (?<-) data h (new KVP<'K,'V>(k, v))
                count <- count + 1

        [<JavaScript>]
        member this.Clear() =
            data <- obj ()
            count <- 0

        [<JavaScript>]
        member this.ContainsKey(k: 'K) =
            JS.HasOwnProperty data (h k)

        [<JavaScript>]
        member this.Count with [<Inline>] get () = count

        [<JavaScript>]
        member this.Item
            with get (k: 'K) : 'V =
                let k = h k
                if JS.HasOwnProperty data k then
                    let x : KVP<'K, 'V> = (?) data k
                    x.Value
                else
                    notPresent ()
            and set (k: 'K) (v: 'V) =
                let h = h k
                if not (JS.HasOwnProperty data h) then
                    count <- count + 1
                (?<-) data h (new KVP<'K,'V>(k, v))

        [<JavaScript>]
        member this.GetEnumerator() =
            let s = JS.GetFieldValues data
            (As<seq<obj>> s).GetEnumerator()

        [<JavaScript>]
        member this.Remove(k: 'K) =
            let h = h k
            if JS.HasOwnProperty data h then
                JS.Delete data h
                count <- count - 1
                true
            else
                false
