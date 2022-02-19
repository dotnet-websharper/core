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

module private WebSharper.Collections.LinkedList

open System.Collections
open System.Collections.Generic

open WebSharper
open WebSharper.JavaScript

type LL<'T> = LinkedList<'T>
type LLN<'T> = LinkedListNode<'T>
type LLE<'T> = LinkedList<'T>.Enumerator

[<Proxy(typeof<LLN<_>>)>]
[<Name "WebSharper.Collections.LinkedListNode">]
type NodeProxy<'T> =
    member this.Previous with [<Inline "$this.p">] get () = X<LLN<'T>>
    member this.Next     with [<Inline "$this.n">] get () = X<LLN<'T>>
    member this.Value    with [<Inline "$this.v">] get () = X<'T>

[<Inline "{p: $p, n: $n, v: $v}">]
let newNode<'T> (p: LLN<'T>) (n: LLN<'T>) (v: 'T) = X<LLN<'T>>

[<Inline "$node.p = $p" >]
let setPrev (node: LLN<'T>) (p: LLN<'T>) = ()

[<Inline "$node.n = $n" >]
let setNext (node: LLN<'T>) (n: LLN<'T>) = ()

[<Proxy(typeof<LLE<_>>)>]
[<Name "WebSharper.Collections.LinkedListEnumerator">]
type EnumeratorProxy<'T> [<JavaScript>] (l: LLN<'T>) =
    let mutable c = l

    interface IEnumerator<'T> with
        member this.Current = 
            if JS.In "c" c then
                failwith "Enumeration has not started. Call MoveNext."
            elif c ==. null then
                failwith "Enumeration already finished."
            else
                c.Value
        
        member this.Current =
            (As<System.Collections.Generic.IEnumerator<obj>> this).Current  

        member this.MoveNext() =
            c <- c.Next
            c <> null

        member this.Dispose() = ()

        [<JavaScript(false)>]
        member this.Reset() = ()

[<Proxy(typeof<LL<_>>)>]
[<Name "WebSharper.Collections.LinkedList">]
type ListProxy<'T> [<JavaScript>] (coll: 'T seq) =
    let mutable c = 0
    let mutable n = null
    let mutable p = null

    do  let ie = coll.GetEnumerator()
        if ie.MoveNext() then
            n <- newNode null null ie.Current
            p <- n
            c <- 1
        while ie.MoveNext() do
            let node = newNode p null ie.Current
            setNext p node
            p <- node
            c <- c + 1
            
    new () = ListProxy(Seq.empty)          

    [<Inline>]
    member this.Count = c

    [<Inline>]
    member this.First = n

    [<Inline>]
    member this.Last = p

    member this.AddAfter(after: LLN<'T>, value) =
        let before = after.Next
        let node = newNode after before value
        if after.Next = null then p <- node
        setNext after node
        if before <> null then setPrev before node
        c <- c + 1
        node

    member this.AddBefore(before: LLN<'T>, value) =
        let after = before.Previous
        let node = newNode after before value
        if before.Previous = null then n <- node 
        setPrev before node
        if after <> null then setNext after node
        c <- c + 1
        node

    member this.AddFirst(value) =
        if c = 0 then
            let node = newNode null null value
            n <- node
            p <- n 
            c <- 1
            node
        else this.AddBefore(n, value)

    member this.AddLast(value) =
        if c = 0 then
            let node = newNode null null value
            n <- node
            p <- n 
            c <- 1
            node
        else this.AddAfter(p, value)

    member this.Clear() =
        c <- 0
        n <- null
        p <- null

    member this.Contains(value: 'T) =
        let mutable found = false
        let mutable node = n
        while node <> null && not found do
            if node.Value ==. value then found <- true 
            else node <- node.Next
        found
            
    member this.Find(value: 'T) =
        let mutable node = n
        let mutable notFound = true
        while notFound && node <> null do
            if node.Value ==. value then
                notFound <- false    
            else
                node <- node.Next
        if notFound then null else node

    member this.FindLast(value: 'T) = 
        let mutable node = p
        let mutable notFound = true
        while notFound && node <> null do
            if node.Value ==. value then
                notFound <- false    
            else
                node <- node.Previous
        if notFound then null else node
                
    [<Name("GetEnumerator")>]
    member this.GetEnumerator(): LinkedList<'T>.Enumerator =
        As (new EnumeratorProxy<_>(As this))

    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>            

    interface IEnumerable<'T> with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>            

    member this.Remove(node: LLN<'T>) =
        let before = node.Previous
        let after = node.Next
        if before = null then n <- after else setNext before after
        if after = null then p <- before else setPrev after before
        c <- c - 1
        
    member this.Remove(value) = 
        let node = this.Find(value)
        if node = null then false
        else
            this.Remove(node)
            true

    member this.RemoveFirst() = this.Remove(n)

    member this.RemoveLast() = this.Remove(p)

    member this.CopyTo(arr: 'T[], index: int) =
        let mutable node = n
        let mutable i = index
        while node <> null do
            arr[i] <- node.Value
            node <- node.Next
            i <- i + 1 
            
    interface ICollection<'T> with
        member this.IsReadOnly = false
        member this.Count = c  
        member this.Add(x) = this.AddLast(x) |> ignore
        [<JavaScript(false)>]
        member this.Clear() = ()
        [<JavaScript(false)>]
        member this.Contains(p) = X<bool>
        [<JavaScript(false)>]
        member this.CopyTo(arr: 'T[], index: int) = ()
        [<JavaScript(false)>]
        member this.Remove(x) = X<bool>
