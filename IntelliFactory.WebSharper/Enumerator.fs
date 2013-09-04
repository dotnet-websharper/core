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

/// Provides an `IEnumerator` implementation.
module private IntelliFactory.WebSharper.Enumerator

module J            = IntelliFactory.WebSharper.JavaScript
type private IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
[<Name "T">]
[<JavaScript>]
type T<'S,'T>(s: 'S, c: 'T, n: T<'S,'T> -> bool) =
    member this.MoveNext() = n this
    member this.State with [<Inline>] get() = s and [<Inline>] set (v: 'S) = this?s <- v
    member this.Current with get() = c and [<Inline>] set (v: 'T) = this?c <- v

/// Constructs a new `IEnumerator` by unfolding a function.
[<Inline>]
[<JavaScript>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next)) 

[<Inline "$x.GetEnumerator()">]
let getEnumerator (x: obj) : IE<'T> = X

[<JavaScript>]
let Get (x: obj) : IE<'T> =
    if J.InstanceOf x J.Global?Array then
        let s = As<obj[]> x
        New 0 (fun e ->
            let i = e.State
            if i < s.Length then
                e.Current <- As s.[i]
                e.State <- i + 1
                true
            else
                false)
    elif J.TypeOf x = J.String then
        let s = As<string> x
        New 0 (fun e ->
            let i = e.State
            if i < s.Length then
                e.Current <- As s.[i]
                e.State <- i + 1
                true
            else
                false)
    else
        getEnumerator x

