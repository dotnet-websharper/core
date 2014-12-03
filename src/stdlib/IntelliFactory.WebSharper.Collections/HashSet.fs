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

module private IntelliFactory.WebSharper.Collections.HashSet

open System.Collections
open System.Collections.Generic

open IntelliFactory.WebSharper

[<AutoOpen>]
module HashSetUtil =
    [<Direct "var r=[]; for(var k in $o) { r.push.apply(r, $o[k]) }; return r">]
    let concat (o: EcmaScript.Array<EcmaScript.Array<'T>>) = X<EcmaScript.Array<'T>>
    
open DictionaryUtil

[<Name "HashSet">]
[<Proxy(typeof<HashSet<_>>)>]
type HashSetProxy<'T when 'T : equality>

    [<JavaScript>]
    private (init   : seq<'T>,
             equals : 'T -> 'T -> bool,
             hash   : 'T -> int) =

        let mutable data  = EcmaScript.Array<EcmaScript.Array<'T>>()
        let mutable count = 0

        [<JavaScript>]
        let arrContains (item: 'T) (arr: EcmaScript.Array<'T>)  =
            let mutable c = true
            let mutable i = 0
            let l = arr.Length
            while c && i < l do
                if equals arr.[i] item then
                    c <- false
                else
                    i <- i + 1
            not c

        [<JavaScript>]
        let arrRemove (item: 'T) (arr: EcmaScript.Array<'T>)  =
            let mutable c = true
            let mutable i = 0
            let l = arr.Length
            while c && i < l do
                if equals arr.[i] item then
                    arr.Splice(i, 1) |> ignore
                    c <- false
                else
                    i <- i + 1
            not c

        [<JavaScript>]
        let add (item: 'T) =
            let h = hash item
            let arr = data.[h]
            if arr ==. null then
                data.[h] <- As [| item |]
                count <- count + 1
                true
            else
                if arrContains item arr then false else    
                    arr.Push item |> ignore
                    count <- count + 1
                    true

        do for x in init do add x |> ignore

        [<JavaScript>]
        new () = HashSetProxy<'T>(Seq.empty, (=), hash)

        [<JavaScript>]
        new (init: seq<'T>) = new HashSetProxy<'T>(init, (=), hash)

        [<JavaScript>]
        new (comparer: IEqualityComparer<'T>) =
            new HashSetProxy<'T>(Seq.empty, equals comparer, getHashCode comparer)

        [<JavaScript>]
        new (init: seq<'T>, comparer: IEqualityComparer<'T>) =
            new HashSetProxy<'T>(init, equals comparer, getHashCode comparer)

        [<JavaScript>]
        member this.Add(item: 'T) = add item

        [<JavaScript>]
        member this.Clear() =
            data <- EcmaScript.Array()
            count <- 0

        [<JavaScript>]
        member x.Contains(item: 'T) =
            let arr = data.[hash item]
            if arr ==. null then false else arrContains item arr

        [<JavaScript>]
        member x.CopyTo(arr: 'T[]) =
            let mutable i = 0
            let all = concat data 
            for i = 0 to all.Length - 1 do 
                arr.[i] <- all.[i]

        [<JavaScript>]
        member x.Count = count

        [<JavaScript>]
        member x.ExceptWith(xs: seq<'T>) =
            for item in xs do
                x.Remove(item) |> ignore

        [<JavaScript>]
        member this.GetEnumerator() =
           (As<seq<'T>>(concat data)).GetEnumerator()

        interface IEnumerable with
            member this.GetEnumerator() = X<IEnumerator>
        
        interface IEnumerable<'T> with
            member this.GetEnumerator() = X<IEnumerator<'T>>

        // TODO: optimize methods by checking if other collection
        // is a HashSet with the same IEqualityComparer
        
        [<JavaScript>]
        member x.IntersectWith(xs: seq<'T>) =
            let other = HashSetProxy(xs, equals, hash) 
            let all = concat data
            for i = 0 to all.Length - 1 do
                let item = all.[i]
                if other.Contains(item) |> not then
                    x.Remove(item) |> ignore

        [<JavaScript>]
        member x.IsProperSubsetOf(xs: seq<'T>) =
            let other = xs |> Array.ofSeq
            count < other.Length && x.IsSubsetOf(other)

        [<JavaScript>]
        member x.IsProperSupersetOf(xs: seq<'T>) =
            let other = xs |> Array.ofSeq
            count > other.Length && x.IsSupersetOf(other)

        [<JavaScript>]
        member x.IsSubsetOf(xs: seq<'T>) =
            let other = HashSetProxy(xs, equals, hash)
            As<_[]>(concat data) |> Array.forall other.Contains

        [<JavaScript>]
        member x.IsSupersetOf(xs: seq<'T>) =
            xs |> Seq.forall x.Contains

        [<JavaScript>]
        member x.Overlaps(xs: seq<'T>) =
            xs |> Seq.exists x.Contains

        [<JavaScript>]
        member x.Remove(item: 'T) =
            let h = hash item
            let arr = data.[h]
            if arr ==. null then false else
                if arrRemove item arr then
                    count <- count - 1
                    true
                else false

        [<JavaScript>]
        member x.RemoveWhere(cond: 'T -> bool) =
            let all = concat data
            for i = 0 to all.Length - 1 do
                let item = all.[i]
                if cond item then
                    x.Remove(item) |> ignore

        [<JavaScript>]
        member x.SetEquals(xs: seq<'T>) =
            let other = HashSetProxy(xs, equals, hash)
            x.Count = other.Count && x.IsSupersetOf(other)

        [<JavaScript>]
        member x.SymmetricExceptWith(xs: seq<'T>) =
            for item in xs do
                if x.Contains item then
                    x.Remove(item) |> ignore
                else
                    x.Add(item) |> ignore

        [<JavaScript>]
        member x.UnionWith(xs: seq<'T>) =
            for item in xs do
                x.Add(item) |> ignore
