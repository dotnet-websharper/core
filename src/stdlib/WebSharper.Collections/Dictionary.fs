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
open System.Collections.Generic

type private KVP<'K,'V> = KeyValuePair<'K,'V>
type private D<'K,'V> = Dictionary<'K,'V>

[<JavaScript>]
module internal DictionaryUtil =

    let notPresent () =
        failwith "The given key was not present in the dictionary."

    let alreadyAdded () =
        failwith "An item with the same key has already been added."

    let equals (c: IEqualityComparer<'T>) x y =
        c.Equals(x, y)

    let getHashCode (c: IEqualityComparer<'T>) x =
        c.GetHashCode x

open DictionaryUtil
open System.Runtime.InteropServices

/// Implements a proxy for the .NET dictionary.
[<Name "Dictionary">]
[<Proxy(typeof<D<_,_>>)>]
type internal Dictionary<'K,'V when 'K : equality>

    private (init   : seq<KVP<'K,'V>>,
             equals : 'K -> 'K -> bool,
             hash   : 'K -> int) =

        let mutable count = 0
        let mutable data  = As<Array<Array<KVP<'K, 'V>>>> [||]

        let get k =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                d.Self |> Array.pick (fun (KeyValue(dk, v)) -> 
                    if equals dk k then Some v else None
                ) 
            else
                notPresent()

        let set k v =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                match d.Self |> Array.tryFindIndex (fun (KeyValue(dk, _)) -> equals dk k) with
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
                if d.Self |> Array.exists (fun (KeyValue(dk, _)) -> equals dk k) then
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
                let r = d.Self |> Array.filter (fun (KeyValue(dk, _)) -> not (equals dk k))
                if r.Length = 0 then
                    count <- count - 1
                    data.[h] <- JS.Undefined
                    true
                elif r.Length < d.Length then                  
                    count <- count - 1
                    data.[h] <- r.JS
                    true
                else
                    false
            else
                false

        do for x in init do
            set x.Key x.Value

        new () = new Dictionary<'K,'V>([||], (=), hash)

        new (capacity: int) = new Dictionary<'K,'V>()

        new (comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>([||], equals comparer, getHashCode comparer)

        new (capacity: int, comparer: IEqualityComparer<'K>) =
            new Dictionary<'K,'V>(comparer)

        new (dictionary: IDictionary<'K,'V>) =
            new Dictionary<'K,'V>(dictionary, (=), hash)

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
                    equals dk k
                ) 
            else
                false

        member this.Count with [<Inline>] get () = count

        member this.Item
            with get (k: 'K) : 'V = get k
            and set (k: 'K) (v: 'V) = set k v

        interface System.Collections.IEnumerable with
            member this.GetEnumerator() = 
                let s = JS.GetFieldValues data
                (As<KeyValuePair<'K,'V>[][]> s |> Array.concat).GetEnumerator()
            
        interface IEnumerable<KeyValuePair<'K,'V>> with
            member this.GetEnumerator() = As<IEnumerator<KeyValuePair<'K,'V>>> ((this :> System.Collections.IEnumerable).GetEnumerator())

        member this.Remove(k: 'K) =
            remove k

        member this.TryGetValue(k: 'K, [<Out>] res : byref<'V>) =
            let h = hash k
            let d = data.[h]
            if As<bool> d then
                let v =
                    d.Self |> Array.tryPick (fun (KeyValue(dk, v)) -> 
                        if equals dk k then Some v else None
                    ) 
                match v with 
                | Some v -> 
                    res <- v
                    true
                | _ -> false
            else
                false
