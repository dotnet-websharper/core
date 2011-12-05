// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

namespace IntelliFactory.WebSharper

[<Name "IntelliFactory.WebSharper.List.T">]
[<Proxy(typeof<list<_>>)>]
type private ListProxy<'T> =
    | [<Name "Empty">] EmptyCase
    | [<Name "Cons">]  ConsCase of 'T * List<'T>

    [<Name "Construct">]
    [<JavaScript>]
    static member Cons(head: 'T, tail: list<'T>) = head :: tail

    [<Name "Nil">]
    [<JavaScript>]
    static member Empty : list<'T> = []

    member this.Head    with [<Inline "$this.$0">] get ()     = X<'T>
    member this.Tail    with [<Inline "$this.$1">] get ()     = X<list<'T>>
    member this.IsEmpty with [<Inline "$this.$ == 0">] get () = X<bool>

    [<JavaScript>]
    member this.Length with get () = Seq.length (As this)

    [<JavaScript>]
    member this.Item with get (x: int) : 'T = Seq.nth x (As this)

    [<JavaScript>]
    member this.GetEnumerator() =
        let data = As<list<'T>> this
        Enumerator.New data (fun e ->
            match e.State with
            | x :: xs ->
                e.Current <- x
                e.State <- xs
                true
            | [] ->
                false)

