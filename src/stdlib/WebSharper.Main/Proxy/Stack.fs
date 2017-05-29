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

[<WebSharper.Name "Stack">]
module private WebSharper.StackProxy

open WebSharper.JavaScript

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
[<Name "WebSharper.Stack">]
type private StackProxy<'T when 'T : equality> =

    [<Inline "[].concat($s).reverse()">]
    private new (s: 'T []) = {}

    [<Inline "[]">]
    new () = {}

    [<Inline>]
    private new (s: seq<'T>) = StackProxy (Array.ofSeq s)

    member this.Count with [<Inline "$this.length">] get () = X<int>

    [<Inline>]
    member this.Clear() = Clear this

    [<Inline>]
    member this.Contains(x: 'T) : bool = Contains this x

    [<Inline>]
    member this.CopyTo(array: 'T [], index) = CopyTo this array index

    [<Inline "$this[0]">]
    member this.Peek() = X<'T>

    [<Inline "$this.shift()">]
    member this.Pop() = X<'T>

    [<Inline "$this.unshift($x)">]
    member this.Push(x: 'T) = X<unit>

    [<Inline "$this.slice(0)">]
    member this.ToArray() = X<'T[]>

