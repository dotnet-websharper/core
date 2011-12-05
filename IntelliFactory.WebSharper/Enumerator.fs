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

/// Provides an `IEnumerator` implementation.
module private IntelliFactory.WebSharper.Enumerator

module J            = IntelliFactory.WebSharper.JavaScript
type private IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
type T<'S,'T> =
    {
        [<Name "s">] mutable State   : 'S
        [<Name "c">] mutable Current : 'T
    }

    interface System.Collections.IEnumerator with
        member this.Current    = X
        member this.MoveNext() = X
        member this.Reset()    = X

    interface IE<'T> with
        member this.Current = X

    interface System.IDisposable with
        member this.Dispose() = X

/// Constructs a new `IEnumerator` by unfolding a function.
[<JavaScript>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    let r = { State = state; Current = Unchecked.defaultof<_> }
    (?<-) r "get_Current" (fun () -> r.Current)
    (?<-) r "MoveNext" (fun () -> next r)
    r :> IE<'T>

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

