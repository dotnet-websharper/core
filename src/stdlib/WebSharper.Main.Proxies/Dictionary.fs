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
open System.Collections
open System.Collections.Generic

type private KVP<'K,'V> = KeyValuePair<'K,'V>
type private D<'K,'V> = Dictionary<'K,'V>

[<JavaScript>]
module internal DictionaryUtil =

    let notPresent () =
        raise (KeyNotFoundException())

    let alreadyAdded () =
        raise (System.ArgumentException("An item with the same key has already been added."))

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
[<Stub>]
type private KeyCollectionEnumeratorProxy<'K,'V> [<JavaScript(false)>] () =
    member this.Current with get() = X<'K>        
    member this.MoveNext() = X<bool>
    member this.Dispose() = ()

// not really used, a sequence enumerator is cast to this type instead
// proxy is needed so calls against it compile
// TODO: lazy iterating
[<Proxy(typeof<D<_,_>.ValueCollection.Enumerator>)>]
[<Stub>]
type private ValueCollectionEnumeratorProxy<'K,'V> [<JavaScript(false)>] () =
    member this.Current with get() = X<'V>        
    member this.MoveNext() = X<bool>
    member this.Dispose() = ()

[<Name "WebSharper.Collections.KeyCollection">]
[<Proxy(typeof<D<_,_>.KeyCollection>)>]
type private KeyCollectionProxy<'K,'V> (d: D<'K,'V>) =
    [<Name "Count">]
    member this.Count = d.Count 

    member this.CopyTo(arr: 'K[], index: int) =
        (Seq.toArray this).CopyTo(arr, index)    

    [<Name "IsReadOnly">]
    member this.IsReadOnly = true

    member this.Contains(item: 'K) = d.ContainsKey(item)

    [<Name "GetEnumerator">]
    member this.GetEnumerator() =
        As<D<'K,'V>.KeyCollection.Enumerator>(
            (d |> Seq.map(fun kvp -> kvp.Key)).GetEnumerator())
            
    interface IEnumerable<'K> with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<IEnumerator<'K>>
    
    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<IEnumerator>

    interface ICollection with
        [<JavaScript(false)>]
        member this.CopyTo(array: System.Array, index: int) = ()
        [<JavaScript(false)>]
        member this.Count = X<int>
        [<JavaScript(false)>]
        member this.IsSynchronized = X<bool>
        [<JavaScript(false)>]
        member this.SyncRoot = X<obj>

    interface ICollection<'K> with
        [<JavaScript(false)>]
        member this.IsReadOnly = X<bool>
        [<JavaScript(false)>]
        member this.Count = X<int>  
        [<JavaScript(false)>]
        member this.Add(p) = ()
        [<JavaScript(false)>]
        member this.Clear() = ()
        [<JavaScript(false)>]
        member this.Contains(p) = X<bool>
        [<JavaScript(false)>]
        member this.CopyTo(arr: 'K[], index: int) = ()
        [<JavaScript(false)>]
        member this.Remove(p) = X<bool>

[<Name "WebSharper.Collections.ValueCollection">]
[<Proxy(typeof<D<_,_>.ValueCollection>)>]
type private ValueCollectionProxy<'K,'V> (d: D<'K,'V>) =
    [<Name "Count">]
    member this.Count = d.Count 

    member this.CopyTo(arr: 'V[], index: int) =
        (Seq.toArray this).CopyTo(arr, index)    

    [<Name "IsReadOnly">]
    member this.IsReadOnly = true

    member this.Contains(item: 'V) = d.ContainsValue(item)

    [<Name "GetEnumerator">]
    member this.GetEnumerator() =
        As<D<'K,'V>.ValueCollection.Enumerator>(
            (d |> Seq.map(fun kvp -> kvp.Value)).GetEnumerator())
            
    interface IEnumerable<'V> with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<IEnumerator<'V>>

    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<IEnumerator>

    interface ICollection with
        [<JavaScript(false)>]
        member this.CopyTo(array: System.Array, index: int) = ()
        [<JavaScript(false)>]
        member this.Count = X<int>
        [<JavaScript(false)>]
        member this.IsSynchronized = X<bool>
        [<JavaScript(false)>]
        member this.SyncRoot = X<obj>

    interface ICollection<'V> with
        [<JavaScript(false)>]
        member this.IsReadOnly = X<bool>
        [<JavaScript(false)>]
        member this.Count = X<int>  
        [<JavaScript(false)>]
        member this.Add(p) = ()
        [<JavaScript(false)>]
        member this.Clear() = ()
        [<JavaScript(false)>]
        member this.Contains(p) = X<bool>
        [<JavaScript(false)>]
        member this.CopyTo(arr: 'V[], index: int) = ()
        [<JavaScript(false)>]
        member this.Remove(p) = X<bool>

[<Proxy(typeof<D<_,_>.Enumerator>)>]
[<Stub>]
type private DictionaryEnumeratorProxy<'K,'V> [<JavaScript(false)>] () =    
    member this.Current with get() = X<KVP<'K,'V>>        
    member this.MoveNext() = X<bool>
    member this.Dispose() = ()

/// Implements a proxy for the .NET dictionary.
[<Name "WebSharper.Collections.Dictionary">]
[<Proxy(typeof<D<_,_>>)>]
type internal Dictionary<'K,'V when 'K : equality>

    private (init   : seq<KVP<'K,'V>>,
             equals : FuncWithArgs<'K * 'K, bool>,
             hash   : 'K -> int) =

        let mutable count = 0
        let mutable data  = As<Array<Array<KVP<'K, 'V>>>> [||]

        let get k =
            let h = hash k
            let d = data.[h]
            if d ==. null then
                notPresent()
            else
                d.Self |> Array.pick (fun (KeyValue(dk, v)) -> 
                    if equals.Call(dk, k) then Some v else None
                ) 

        let set k v =
            let h = hash k
            let d = data.[h]
            if d ==.null then
                count <- count + 1
                data.[h] <- Array(KVP(k, v))
            else
                match d.Self |> Array.tryFindIndex (fun (KeyValue(dk, _)) -> equals.Call(dk, k)) with
                | Some i ->
                    d.[i] <- KVP(k, v) 
                | None ->
                    count <- count + 1
                    d.Push(KVP(k, v)) |> ignore
                    
        let add k v =
            let h = hash k
            let d = data.[h]
            if d ==. null then
                count <- count + 1
                data.[h] <- Array(KVP(k, v))
            else
                if d.Self |> Array.exists (fun (KeyValue(dk, _)) -> equals.Call(dk, k)) then
                    alreadyAdded()                    
                count <- count + 1
                d.Push(KVP(k, v)) |> ignore
                    
        let remove k =
            let h = hash k 
            let d = data.[h]
            if d ==. null then
                false
            else
                let r = d.Self |> Array.filter (fun (KeyValue(dk, _)) -> not (equals.Call(dk, k)))
                if r.Length < d.Length then                  
                    count <- count - 1
                    data.[h] <- r.JS
                    true
                else
                    false

        do for x in init do
            set x.Key x.Value

        new () = new Dictionary<'K,'V>([||], genEquals<'K>(), hash)

        new (capacity: int) = new Dictionary<'K,'V>()

        new (comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>([||], equals comparer, getHashCode comparer)

        new (capacity: int, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(comparer)

        new (dictionary: IDictionary<'K,'V>) =
            new Dictionary<'K,'V>(dictionary, genEquals<'K>(), hash)

        new (dictionary: IDictionary<'K,'V>, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(
                dictionary,
                equals comparer,
                getHashCode comparer
            )

        [<Name "DAdd">]
        member this.Add(k: 'K, v: 'V) =
            add k v

        member this.Clear() =
            data <- Array()
            count <- 0

        member this.ContainsKey(k: 'K) =
            let h = hash k
            let d = data.[h]
            if d ==. null then
                false
            else
                d.Self |> Array.exists (fun (KeyValue(dk, _)) -> 
                    equals.Call(dk, k)
                ) 

        member this.ContainsValue(v: 'V) =
            (As<D<'K,'V>> this) |> Seq.exists (fun (KeyValue(_, x)) -> Unchecked.equals x v)

        member this.Count with [<Inline>] get () = count

        [<Name("Item")>]
        member this.Item
            with get (k: 'K) : 'V = get k
            and set (k: 'K) (v: 'V) = set k v

        [<Name("GetEnumerator")>]
        member this.GetEnumerator() = 
            let s = JS.GetFieldValues data
            As<D<'K,'V>.Enumerator> ((As<KeyValuePair<'K,'V>[][]> s |> Array.concat).GetEnumerator())
            
        interface System.Collections.IEnumerable with
            [<JavaScript(false)>]
            member this.GetEnumerator() = X<_>            
        
        interface IEnumerable<KeyValuePair<'K,'V>> with
            [<JavaScript(false)>]
            member this.GetEnumerator() = X<_>            

        interface ICollection<KeyValuePair<'K,'V>> with
            member this.IsReadOnly = false
            member this.Count = count  
            member this.Add(p) = this.Add(p.Key, p.Value)
            [<JavaScript(false)>]
            member this.Clear() = ()
            [<JavaScript(JavaScriptOptions.DefaultToUndefined)>]
            member this.Contains(p) =
                match this.TryGetValue(p.Key) with
                | true, v when Unchecked.equals v p.Value -> true
                | _ -> false
            member this.CopyTo(arr: KeyValuePair<'K,'V>[], index: int) =
                (Seq.toArray this).CopyTo(arr, index)
            [<JavaScript(JavaScriptOptions.DefaultToUndefined)>]
            member this.Remove(p) =
                match this.TryGetValue(p.Key) with
                | true, v when Unchecked.equals v p.Value ->
                    this.Remove p.Key
                | _ -> false

        interface ICollection with
            [<JavaScript(false)>]
            member this.CopyTo(array: System.Array, index: int) = ()
            [<JavaScript(false)>]
            member this.Count = X<int>
            [<JavaScript(false)>]
            member this.IsSynchronized = X<bool>
            [<JavaScript(false)>]
            member this.SyncRoot = X<obj>

        interface IDictionary with
            [<JavaScript(false)>]
            member this.Add(key: obj, value: obj) = ()
            [<JavaScript(false)>]
            member this.Clear() = ()
            [<JavaScript(false)>]
            member this.Contains(key: obj) = X<bool>
            [<JavaScript(false)>]
            member this.GetEnumerator() = X<IDictionaryEnumerator>
            [<JavaScript(false)>]
            member this.Remove(key: obj) = ()
            member this.IsFixedSize = false
            [<JavaScript(false)>]
            member this.IsReadOnly = X<bool>
            [<JavaScript(false)>]
            member this.Item 
                with get key = X<obj>
                and set key value = ()
            [<JavaScript(false)>]
            member this.Keys = X<ICollection>
            [<JavaScript(false)>]
            member this.Values = X<ICollection>

        [<Name("RemoveKey")>]
        member this.Remove(k: 'K) =
            remove k

        member this.TryGetValue(k: 'K, [<Out>] res : byref<'V>) =
            let h = hash k
            let d = data.[h]
            if d ==. null then
                false
            else
                let v =
                    d.Self |> Array.tryPick (fun (KeyValue(dk, v)) -> 
                        if equals.Call(dk, k) then Some v else None
                    ) 
                match v with 
                | Some v -> 
                    res <- v
                    true
                | _ -> false

        [<Name("Values")>]
        member this.Values =
            As<D<'K,'V>.ValueCollection>(ValueCollectionProxy(As<D<'K,'V>>this))

        [<Name("Keys")>]
        member this.Keys =
            As<D<'K,'V>.KeyCollection>(KeyCollectionProxy(As<D<'K,'V>>this))
