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

module private IntelliFactory.WebSharper.Collections.LinkedList

open System.Collections
open System.Collections.Generic

open IntelliFactory.WebSharper

type LL<'T> = LinkedList<'T>
type LLN<'T> = LinkedListNode<'T>
type LLE<'T> = LinkedList<'T>.Enumerator

[<Proxy(typeof<LLN<_>>)>]
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
type EnumeratorProxy<'T> [<JavaScript>] (l: LLN<'T>) =
    let mutable c = l

    [<JavaScript>]
    member this.Current = c.Value

    [<JavaScript>]
    member this.MoveNext() =
        c <- c.Next
        c <> null

    [<JavaScript>]
    member this.Dispose() = ()

[<Proxy(typeof<LL<_>>)>]
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
            
    [<JavaScript>]
    new () = ListProxy(Seq.empty)          

    [<Inline>]
    [<JavaScript>]
    member this.Count = c

    [<Inline>]
    [<JavaScript>]
    member this.First = n

    [<Inline>]
    [<JavaScript>]
    member this.Last = p

    [<JavaScript>]
    member this.AddAfter(after: LLN<'T>, value) =
        let before = after.Next
        let node = newNode after before value
        if after.Next = null then p <- node
        setNext after node
        if before <> null then setPrev before node
        c <- c + 1
        node

    [<JavaScript>]
    member this.AddBefore(before: LLN<'T>, value) =
        let after = before.Previous
        let node = newNode after before value
        if before.Previous = null then n <- node 
        setPrev before node
        if after <> null then setNext after node
        c <- c + 1
        node

    [<JavaScript>]
    member this.AddFirst(value) =
        if c = 0 then
            let node = newNode null null value
            n <- node
            p <- n 
            c <- 1
            node
        else this.AddBefore(n, value)

    [<JavaScript>]
    member this.AddLast(value) =
        if c = 0 then
            let node = newNode null null value
            n <- node
            p <- n 
            c <- 1
            node
        else this.AddAfter(p, value)

    [<JavaScript>]
    member this.Clear() =
        c <- 0
        n <- null
        p <- null

    [<JavaScript>]
    member this.Contains(value: 'T) =
        let mutable found = false
        let mutable node = n
        while node <> null && not found do
            if node.Value ==. value then found <- true 
            else node <- node.Next
        found
            
    [<JavaScript>]
    member this.Find(value: 'T) =
        let mutable node = n
        let mutable notFound = true
        while notFound && node <> null do
            if node.Value ==. value then
                notFound <- false    
            else
                node <- node.Next
        if notFound then null else node

    [<JavaScript>]
    member this.FindLast(value: 'T) = 
        let mutable node = p
        let mutable notFound = true
        while notFound && node <> null do
            if node.Value ==. value then
                notFound <- false    
            else
                node <- node.Previous
        if notFound then null else node
                
    [<JavaScript>]
    member this.GetEnumerator(): LinkedList<'T>.Enumerator =
        As (EnumeratorProxy(As this))

    [<JavaScript>]
    member this.Remove(node: LLN<'T>) =
        let before = node.Previous
        let after = node.Next
        if before = null then n <- after else setNext before after
        if after = null then p <- before else setPrev after before
        c <- c - 1
        
    [<JavaScript>]
    member this.Remove(value) = 
        let node = this.Find(value)
        if node = null then false
        else
            this.Remove(node)
            true

    [<JavaScript>]
    member this.RemoveFirst() = this.Remove(n)

    [<JavaScript>]
    member this.RemoveLast() = this.Remove(p)
               