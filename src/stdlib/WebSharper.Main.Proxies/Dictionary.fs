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
open System.Collections
open System.Collections.Generic

type private KVP<'K,'V> = KeyValuePair<'K,'V>
type private D<'K,'V> = Dictionary<'K,'V>

[<JavaScript>]
module internal DictionaryUtil =

    let notPresent () =
        failwith "The given key was not present in the dictionary."

    let alreadyAdded () =
        failwith "An item with the same key has already been added."

    let equals (c: IEqualityComparer<'T>) =
        FuncWithArgs(fun (x, y) -> c.Equals(x, y))

    [<Inline>]
    let genEquals<'T when 'T : equality> () = 
        FuncWithArgs(fun (x :'T, y) -> x = y)

    let getHashCode (c: IEqualityComparer<'T>) x =
        c.GetHashCode x

open DictionaryUtil
open System.Runtime.InteropServices

// not really used, a sequence enumerator is cast to this type instead
// proxy is needed so calls against it compile
// TODO: lazy iterating
[<Proxy(typeof<D<_,_>.KeyCollection.Enumerator>)>]
[<Name "WebSharper.Collections.KeyCollection.Enumerator">]
type private KeyCollectionEnumeratorProxy<'K,'V> =
    [<Name "Current">]
    abstract get_Current: unit -> 'K
    [<Name "MoveNext">]
    abstract MoveNext: unit -> bool
    [<Name "Dispose">]
    abstract Dispose: unit -> unit

// not really used, a sequence enumerator is cast to this type instead
// proxy is needed so calls against it compile
// TODO: lazy iterating
[<Proxy(typeof<D<_,_>.ValueCollection.Enumerator>)>]
[<Name "WebSharper.Collections.ValueCollection.Enumerator">]
type private ValueCollectionEnumeratorProxy<'K,'V> =
    [<Name "Current">]
    abstract get_Current: unit -> 'V
    [<Name "MoveNext">]
    abstract MoveNext: unit -> bool
    [<Name "Dispose">]
    abstract Dispose: unit -> unit

[<Name "WebSharper.Collections.KeyCollection">]
[<Proxy(typeof<D<_,_>.KeyCollection>)>]
type private KeyCollectionProxy<'K,'V> (d: D<'K,'V>) =
    member this.Count = d.Count 

    [<Name "GetEnumerator">]
    member this.GetEnumerator() =
        As<D<'K,'V>.KeyCollection.Enumerator>(
            (d |> Seq.map(fun kvp -> kvp.Key)).GetEnumerator())
            
    interface IEnumerable<'K> with
        [<Inline>]
        member this.GetEnumerator() = As<IEnumerator<'K>>(this.GetEnumerator())
        [<Inline>]
        member this.GetEnumerator() = As<IEnumerator>(this.GetEnumerator())

[<Name "WebSharper.Collections.ValueCollection">]
[<Proxy(typeof<D<_,_>.ValueCollection>)>]
type private ValueCollectionProxy<'K,'V> (d: D<'K,'V>) =
    member this.Count = d.Count 

    [<Name "GetEnumerator">]
    member this.GetEnumerator() =
        As<D<'K,'V>.ValueCollection.Enumerator>(
            (d |> Seq.map(fun kvp -> kvp.Value)).GetEnumerator())
            
    interface IEnumerable<'V> with
        [<Inline>]
        member this.GetEnumerator() = As<IEnumerator<'V>>(this.GetEnumerator())
        [<Inline>]
        member this.GetEnumerator() = As<IEnumerator>(this.GetEnumerator())

[<Proxy(typeof<D<_,_>.Enumerator>)>]
[<Name "WebSharper.Collections.Dictionary.Enumerator">]
type private DictionaryEnumeratorProxy<'K,'V> =
    [<Name "Current">]
    abstract get_Current: unit -> KVP<'K, 'V>
    [<Name "MoveNext">]
    abstract MoveNext: unit -> bool
    [<Name "Dispose">]
    abstract Dispose: unit -> unit

/// Implements a proxy for the .NET dictionary.
[<Name "WebSharper.Collections.Dictionary">]
[<Proxy(typeof<D<_,_>>)>]
type private Dictionary<'K,'V when 'K : equality>

    private (init   : seq<KVP<'K,'V>>,
             equals : FuncWithArgs<'K * 'K, bool>,
             hash   : 'K -> int) =

        let mutable count = 0
        let mutable data  = As<Array<Array<KVP<'K, 'V>>>> [||]

        let get k =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                d.Self |> Array.pick (fun (KeyValue(dk, v)) -> 
                    if equals.Call(dk, k) then Some v else None
                ) 
            else
                notPresent()

        let set k v =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                match d.Self |> Array.tryFindIndex (fun (KeyValue(dk, _)) -> equals.Call(dk, k)) with
                | Some i ->
                    d.[i] <- KVP(k, v) 
                | None ->
                    count <- count + 1
                    d.Push(KVP(k, v)) |> ignore
            else
                count <- count + 1
                data.[h] <- Array(KVP(k, v))
                    
        let add k v =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                if d.Self |> Array.exists (fun (KeyValue(dk, _)) -> equals.Call(dk, k)) then
                    alreadyAdded()                    
                count <- count + 1
                d.Push(KVP(k, v)) |> ignore
            else
                count <- count + 1
                data.[h] <- Array(KVP(k, v))
                    
        let remove k =
            let h = hash k 
            let d = data.[h]
            if As<bool> d then
                let r = d.Self |> Array.filter (fun (KeyValue(dk, _)) -> not (equals.Call(dk, k)))
                if r.Length < d.Length then                  
                    count <- count - 1
                    data.[h] <- r.JS
                    true
                else
                    false
            else
                false

        do for x in init do
            set x.Key x.Value

        [<Inline>]
        new () = new Dictionary<'K,'V>([||], genEquals<'K>(), hash)

        [<Inline>]
        new (capacity: int) = new Dictionary<'K,'V>()

        [<Inline>]
        new (comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>([||], equals comparer, getHashCode comparer)

        [<Inline>]
        new (capacity: int, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(comparer)

        [<Inline>]
        new (dictionary: IDictionary<'K,'V>) =
            new Dictionary<'K,'V>(dictionary, genEquals<'K>(), hash)

        [<Inline>]
        new (dictionary: IDictionary<'K,'V>, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(
                dictionary,
                equals comparer,
                getHashCode comparer
            )

        member this.Add(k: 'K, v: 'V) =
            add k v

        member this.Clear() =
            data <- Array()
            count <- 0

        member this.ContainsKey(k: 'K) =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                d.Self |> Array.exists (fun (KeyValue(dk, _)) -> 
                    equals.Call(dk, k)
                ) 
            else
                false

        member this.Count with [<Inline>] get () = count

        member this.Item
            with get (k: 'K) : 'V = get k
            and set (k: 'K) (v: 'V) = set k v

        [<Name "GetEnumerator">]
        member this.GetEnumerator() =
            let s = JS.GetFieldValues data
            (As<KeyValuePair<'K,'V>[][]> s |> Array.concat).GetEnumerator()
            |> As<D<'K,'V>.Enumerator>

        interface System.Collections.IEnumerable with
            [<Inline>]
            member this.GetEnumerator() = As<IEnumerator>(this.GetEnumerator())
            
        interface IEnumerable<KeyValuePair<'K,'V>> with
            [<Inline>]
            member this.GetEnumerator() = As<IEnumerator<KeyValuePair<'K,'V>>> (this.GetEnumerator())

        member this.Remove(k: 'K) =
            remove k

        member this.TryGetValue(k: 'K, [<Out>] res : byref<'V>) =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                let v =
                    d.Self |> Array.tryPick (fun (KeyValue(dk, v)) -> 
                        if equals.Call(dk, k) then Some v else None
                    ) 
                match v with 
                | Some v -> 
                    res <- v
                    true
                | _ -> false
            else
                false

        member this.Values =
            ValueCollectionProxy(As<D<'K,'V>>this)

        member this.Keys =
            KeyCollectionProxy(As<D<'K,'V>>this)
