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

open System.Collections.Generic

open IntelliFactory.WebSharper

type LL<'T> = LinkedList<'T>
type LLN<'T> = LinkedListNode<'T>

[<Proxy(typeof<LLN<_>>)>]
type LinkedListNodeProxy<'T> =
    member this.Previous with [<Inline "$this.p">] get () = X<LLN<'T>>
    member this.Next     with [<Inline "$this.n">] get () = X<LLN<'T>>
    member this.Value    with [<Inline "$this.v">] get () = X<'T>

[<Inline "{p: $p, n: $n, v: $v}">]
let newNode<'T> (p: LLN<'T>) (n: LLN<'T>) (v: 'T) = X<LLN<'T>>

[<Inline "$node.p = $p" >]
let setPrev (node: LLN<'T>) (p: LLN<'T>) = ()

[<Inline "$node.n = $n" >]
let setNext (node: LLN<'T>) (n: LLN<'T>) = ()

[<JavaScript>]
[<Proxy(typeof<LL<_>>)>]
type LinkedListProxy<'T>(coll: 'T seq) =
    [<Name "c">] 
    let mutable count = 0
    [<Name "f">] 
    let mutable first = null
    [<Name "l">] 
    let mutable last = null

    do  let ie = coll.GetEnumerator()
        if ie.MoveNext() then
            first <- newNode null null ie.Current
            last <- first
            count <- 1
        while ie.MoveNext() do
            let node = newNode last null ie.Current
            setNext last node
            last <- node
            count <- count + 1
            
    new () = LinkedListProxy(Seq.empty)          

    [<Inline>]
    member this.Count = count

    [<Inline>]
    member this.First = first

    [<Inline>]
    member this.Last = last

    member this.AddAfter(after: LLN<'T>, value) =
        let before = after.Next
        let node = newNode after before value
        if after.Next = null then last <- node
        setNext after node
        if before <> null then setPrev before node
        count <- count + 1
        node

    member this.AddBefore(before: LLN<'T>, value) =
        let after = before.Previous
        let node = newNode after before value
        if before.Previous = null then first <- node 
        setPrev before node
        if after <> null then setNext after node
        count <- count + 1
        node

    member this.AddFirst(value) =
        if count = 0 then
            let node = newNode null null value
            first <- node
            last <- first 
            count <- 1
            node
        else this.AddBefore(first, value)

    member this.AddLast(value) =
        if count = 0 then
            let node = newNode null null value
            first <- node
            last <- first 
            count <- 1
            node
        else this.AddAfter(last, value)

    member this.Clear() =
        count <- 0
        first <- null
        last <- null

    member this.Contains(value) =
        let mutable found = false
        let mutable node = first
        while node <> null && not found do
            if node.Value ==. value then found <- true 
            else node <- node.Next
        found
            
    member this.Find(value: 'T) =
        let mutable node = first
        while node <> null && node.Value !=. value do
            node <- node.Next
        if node ==. value then node else null

    member this.FindLast(value: 'T) = 
        let mutable node = last
        while node <> null && node.Value !=. value do
            node <- node.Previous
        if node ==. value then node else null

    member this.GetEnumerator(): IEnumerator<'T> =
        (first |> Seq.unfold (function null -> None | s -> Some (s.Value, s.Next))).GetEnumerator()
            
    member this.Remove(node: LLN<'T>) =
        let before = node.Previous
        let after = node.Next
        if before = null then first <- after else setNext before after
        if after = null then last <- before else setPrev after before
        count <- count - 1
        
    member this.Remove(value) = 
        let node = this.Find(value)
        if node = null then false
        else
            this.Remove(node)
            true

    member this.RemoveFirst() = this.Remove(first)

    member this.RemoveLast() = this.Remove(last)
               