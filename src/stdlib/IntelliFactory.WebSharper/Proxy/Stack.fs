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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Stack">]
module private IntelliFactory.WebSharper.StackProxy

[<Inline "$arr.splice($offset,$len)">]
let splice (arr: obj) (offset: int) (len: int) = X<unit>

[<JavaScript>]
let Clear (stack: obj) =
    splice stack 0 (stack :?> obj []).Length

[<JavaScript>]
let Contains (stack: obj) (el: 'T) =
    Seq.exists ((=) el) (stack :?> 'T[])

[<JavaScript>]
let CopyTo (stack: obj) (array: 'T[]) (index: int) =
    Array.blit array 0 array index (stack :?> 'T[]).Length

[<Proxy(typeof<System.Collections.Generic.Stack<_>>)>]
type private StackProxy<'T when 'T : equality> =

    [<Inline "[].concat($s).reverse()">]
    new (s: 'T []) = {}

    [<Inline "[]">]
    new () = {}

    member this.Count with [<Inline "$this.length">] get () = X<int>

    [<Inline>]
    [<JavaScript>]
    member this.Clear() = Clear this

    [<Inline>]
    [<JavaScript>]
    member this.Contains(x: 'T) : bool = Contains this x

    [<Inline>]
    [<JavaScript>]
    member this.CopyTo(array: 'T [], index) = CopyTo this array index

    [<Inline "$this[0]">]
    member this.Peek() = X<'T>

    [<Inline "$this.shift()">]
    member this.Pop() = X<'T>

    [<Inline "$this.unshift($x)">]
    member this.Push(x: 'T) = X<unit>

    [<Inline "$this.slice(0)">]
    member this.ToArray() = X<'T[]>

