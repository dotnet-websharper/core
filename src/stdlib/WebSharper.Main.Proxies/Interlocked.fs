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

namespace WebSharper

open WebSharper.JavaScript

open System.Threading

[<Proxy(typeof<Interlocked>); Name "Interlocked">]
type private InterlockedProxy =
    [<Inline>]
    static member Add(location1: byref<int>, value: int) = location1 <- location1 + value; location1
    [<Inline>]
    static member Add(location1: byref<int64>, value: int64) = location1 <- location1 + value; location1
    [<Inline>]
    static member CompareExchange<'T>(location1: byref<'T>, value: 'T, comparand: 'T) =
        if Unchecked.equals location1 comparand then
            let orig = location1
            location1 <- value
            orig
        else
            location1
    [<Inline>]
    static member CompareExchange(location1: byref<int>, value: int, comparand: int) = InterlockedProxy.CompareExchange<int>(&location1, value, comparand)
    [<Inline>]
    static member CompareExchange(location1: byref<int64>, value: int64, comparand: int64) = InterlockedProxy.CompareExchange<int64>(&location1, value, comparand)
    [<Inline>]
    static member CompareExchange(location1: byref<single>, value: single, comparand: single) = InterlockedProxy.CompareExchange<single>(&location1, value, comparand)
    [<Inline>]
    static member CompareExchange(location1: byref<double>, value: double, comparand: double) = InterlockedProxy.CompareExchange<double>(&location1, value, comparand)
    [<Inline>]
    static member CompareExchange(location1: byref<obj>, value: obj, comparand: obj) = InterlockedProxy.CompareExchange<obj>(&location1, value, comparand)
    [<Inline>]
    static member Decrement(location: byref<int>) = location <- location - 1; location
    [<Inline>]
    static member Decrement(location: byref<int64>) = location <- location - 1L; location
    [<Inline>]
    static member Exchange<'T>(location1: byref<'T>, value: 'T) =
        let orig = location1
        location1 <- value
        orig
    [<Inline>]
    static member Exchange(location1: byref<int>, value: int) = InterlockedProxy.Exchange<int>(&location1, value)
    [<Inline>]
    static member Exchange(location1: byref<int64>, value: int64) = InterlockedProxy.Exchange<int64>(&location1, value)
    [<Inline>]
    static member Exchange(location1: byref<single>, value: single) = InterlockedProxy.Exchange<single>(&location1, value)
    [<Inline>]
    static member Exchange(location1: byref<double>, value: double) = InterlockedProxy.Exchange<double>(&location1, value)
    [<Inline>]
    static member Exchange(location1: byref<obj>, value: obj) = InterlockedProxy.Exchange<obj>(&location1, value)
    [<Inline>]
    static member Increment(location: byref<int>) = location <- location + 1; location
    [<Inline>]
    static member Increment(location: byref<int64>) = location <- location + 1L; location
