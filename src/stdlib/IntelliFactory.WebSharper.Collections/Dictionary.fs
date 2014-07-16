// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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
open System.Collections.Generic
module J = JavaScript

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
            if J.HasOwnProperty data h then
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
            J.HasOwnProperty data (h k)

        [<JavaScript>]
        member this.Count with [<Inline>] get () = count

        [<JavaScript>]
        member this.Item
            with get (k: 'K) : 'V =
                let k = h k
                if J.HasOwnProperty data k then
                    let x : KVP<'K, 'V> = (?) data k
                    x.Value
                else
                    notPresent ()
            and set (k: 'K) (v: 'V) =
                let h = h k
                if not (J.HasOwnProperty data h) then
                    count <- count + 1
                (?<-) data h (new KVP<'K,'V>(k, v))

        [<JavaScript>]
        member this.GetEnumerator() =
            let s = J.GetFieldValues data
            (As<seq<obj>> s).GetEnumerator()

        [<JavaScript>]
        member this.Remove(k: 'K) =
            let h = h k
            if J.HasOwnProperty data h then
                J.Delete data h
                count <- count - 1
                true
            else
                false
