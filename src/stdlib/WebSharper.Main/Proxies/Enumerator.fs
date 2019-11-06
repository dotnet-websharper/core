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

#if JAVASCRIPT
/// Provides an `IEnumerator` implementation.
module private WebSharper.Enumerator

open WebSharper.JavaScript
type IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
[<Sealed>]
type T<'S,'T> [<JavaScript>] (s: 'S, c: 'T, n: T<'S,'T> -> bool, d: T<'S,'T> -> unit) =
    [<Inline; JavaScript>] 
    member this.MoveNext() = n this
    member this.State with [<Inline; JavaScript>] get() = s and [<Inline; JavaScript>] set (v: 'S) = this?s <- v
    member this.Current with [<Inline; JavaScript>] get() = c and [<Inline; JavaScript>] set (v: 'T) = this?c <- v

    interface System.Collections.IEnumerator with
        [<JavaScript>] 
        member this.MoveNext() = n this
        [<JavaScript>]
        member this.Current with get() = box c
        member this.Reset() = failwith "IEnumerator.Reset not supported"

    interface System.Collections.Generic.IEnumerator<'T> with
        [<JavaScript>]
        member this.Current with get() = c

    interface System.IDisposable with
        [<JavaScript>] 
        member this.Dispose() = if As d then d this

/// Constructs a new `IEnumerator` by unfolding a function.
[<Inline>]
[<JavaScript>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, As JS.Undefined)) 

[<Inline>]
[<JavaScript>]
let NewDisposing<'S,'T> (state: 'S) dispose (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, dispose))

[<Inline "$x.GetEnumerator()">]
let getEnumerator (x: obj) : IE<'T> = X

[<JavaScript>]
let ArrayEnumerator (s: obj[]) =
    New 0 (fun e ->
        let i = e.State
        if i < s.Length then
            e.Current <- As s.[i]
            e.State <- i + 1
            true
        else
            false)

[<JavaScript>]
let StringEnumerator (s: string) =
    New 0 (fun e ->
        let i = e.State
        if i < s.Length then
            e.Current <- As s.[i]
            e.State <- i + 1
            true
        else
            false)

[<JavaScript>]
let Get (x: seq<'T>) : IE<'T> =
    if x :? System.Array then
        ArrayEnumerator (As x)
    elif JS.TypeOf x = JS.String then
        StringEnumerator (As x)
    else
        getEnumerator x

[<Inline "'GetEnumerator0' in $x ? $x.GetEnumerator0() : $x.GetEnumerator()">]
let getEnumerator0 (x: obj) : System.Collections.IEnumerator = X

[<JavaScript>]
let Get0 (x: System.Collections.IEnumerable) : System.Collections.IEnumerator =
    if x :? System.Array then
        As (ArrayEnumerator (As x))
    elif JS.TypeOf x = JS.String then
        As (StringEnumerator (As x))
    else
        getEnumerator0 x
#else
namespace WebSharper
#endif