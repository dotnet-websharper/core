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

/// Provides an `IEnumerator` implementation.
module private WebSharper.Enumerator

open WebSharper.JavaScript
type IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
[<Sealed; JavaScript>]
[<Name "WebSharper.Enumerator">]
type T<'S,'T> (s: 'S, c: 'T, n: T<'S,'T> -> bool, d: T<'S,'T> -> unit) =
    [<Name "MoveNext">] 
    member this.MoveNext() = n this
    member this.State with [<Inline>] get() = s and [<Inline>] set (v: 'S) = this?s <- v
    [<Name "Current">] 
    member this.Current with get() = c and [<Inline>] set (v: 'T) = this?c <- v

    interface System.Collections.IEnumerator with
        [<Inline>]
        member this.MoveNext() = this.MoveNext() 
        [<Inline>]
        member this.Current with get() = box c
        [<JavaScript false>]
        member this.Reset() = X<unit>

    interface System.Collections.Generic.IEnumerator<'T> with
        [<Inline>]
        member this.Current with get() = c

    interface System.IDisposable with
        member this.Dispose() = if As d then d this

/// Constructs a new `IEnumerator` by unfolding a function.
[<Inline>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, As JS.Undefined)) 

[<Inline>]
let NewDisposing<'S,'T> (state: 'S) dispose (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, dispose))

[<JavaScript>]
[<Name "WebSharper.ItemEnumerator">]
// Enumerates on JavaScript objects with a length property and indexed items.
// Works on both JS Array and String.
type internal ItemEnumerator(x: obj) =
    let mutable i = 0
    
    interface System.Collections.IEnumerator with
        member this.MoveNext() =
            i <- i + 1
            i >= x?length
        [<Direct "this.x[this.i]" >]
        member this.Current with get() = X<obj>
        [<JavaScript false>]
        member this.Reset() = X<unit>

    interface System.IDisposable with
        member this.Dispose() = ()

[<Inline>]
let Get (x: seq<'T>) : IE<'T> =
    x.GetEnumerator()
