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
type internal EnumerableArray<'T> (s: 'T[]) =
    [<Name "GetEnumerator">]
    member this.GetEnumerator() : IE<'T> = 
        New 0 (fun e ->
            let i = e.State
            if i < s.Length then
                e.Current <- As s.[i]
                e.State <- i + 1
                true
            else
                false)
    
    interface System.Collections.Generic.IEnumerable<'T> with
        [<Inline>]
        member this.GetEnumerator() = this.GetEnumerator()
        
        [<Inline>]
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

[<JavaScript>]
type internal EnumerableString (s: string) =
    [<Name "GetEnumerator">]
    member this.GetEnumerator() : IE<char> = 
        New 0 (fun e ->
            let i = e.State
            if i < s.Length then
                e.Current <- As s.[i]
                e.State <- i + 1
                true
            else
                false)
    interface System.Collections.Generic.IEnumerable<char> with
        [<Inline>]
        member this.GetEnumerator() = this.GetEnumerator()
        
        [<Inline>]
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

[<Inline>]
let Get (x: seq<'T>) : IE<'T> =
    x.GetEnumerator()
