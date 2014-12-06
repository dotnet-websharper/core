// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
module private IntelliFactory.WebSharper.Enumerator

open IntelliFactory.WebSharper.JavaScript
type IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
[<Name "T">]
[<Sealed>]
type T<'S,'T> [<JavaScript>] (s: 'S, c: 'T, n: T<'S,'T> -> bool) =
    [<JavaScript>] 
    member this.MoveNext() = n this
    member this.State with [<Inline; JavaScript>] get() = s and [<Inline; JavaScript>] set (v: 'S) = this?s <- v
    member this.Current with [<JavaScript>] get() = c and [<Inline; JavaScript>] set (v: 'T) = this?c <- v

/// Constructs a new `IEnumerator` by unfolding a function.
[<Inline>]
[<JavaScript>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next)) 

[<Inline "$x.GetEnumerator()">]
let getEnumerator (x: obj) : IE<'T> = X

[<JavaScript>]
let Get (x: obj) : IE<'T> =
    if JS.InstanceOf x JS.Global?Array then
        let s = As<obj[]> x
        New 0 (fun e ->
            let i = e.State
            if i < s.Length then
                e.Current <- As s.[i]
                e.State <- i + 1
                true
            else
                false)
    elif JS.TypeOf x = JS.String then
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

