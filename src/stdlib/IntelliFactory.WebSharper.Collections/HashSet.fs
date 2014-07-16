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

module private IntelliFactory.WebSharper.Collections.HashSet

open System.Collections
open System.Collections.Generic

open IntelliFactory.WebSharper

[<AutoOpen>]
module HashSetUtil =
    [<Direct "var r=[]; for(var k in $o) { r.push.apply(r, $o[k]) }; return r">]
    let concat (o: 'T[][]) = X<'T[]>

open DictionaryUtil

[<Name "HashSet">]
[<Proxy(typeof<HashSet<_>>)>]
type HashSetProxy<'T when 'T : equality>

    [<JavaScript>]
    private (init   : seq<'T>,
             equals : 'T -> 'T -> bool,
             hash   : 'T -> int) =

        let mutable data  = [||]
        let mutable count = 0

        [<JavaScript>]
        let arrContains (item: 'T) (arr: 'T[])  =
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
        let arrRemove (item: 'T) (arr: 'T[])  =
            let mutable c = true
            let mutable i = 0
            let l = arr.Length
            while c && i < l do
                if equals arr.[i] item then
                    ResizeArray.splice arr i 1 [||] |> ignore
                    c <- false
                else
                    i <- i + 1
            not c

        [<JavaScript>]
        let add (item: 'T) =
            let h = hash item
            let arr = data.[h]
            if arr ==. null then
                data.[h] <- [| item |] 
                count <- count + 1
                true
            else
                if arrContains item arr then false else    
                    ResizeArray.push arr item
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
            data <- [||]
            count <- 0

        [<JavaScript>]
        member x.Contains(item: 'T) =
            let arr = data.[hash item]
            if arr ==. null then false else arrContains item arr

        [<JavaScript>]
        member x.CopyTo(arr: 'T[]) =
            let mutable i = 0
            for item in concat data do
                arr.[i] <- item
                i <- i + 1

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
            for item in concat data do
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
            concat data |> Seq.forall other.Contains

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
            for item in concat data do
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
