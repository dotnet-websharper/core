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

open System
open System.Linq
open System.Collections
open System.Collections.Generic
open WebSharper.Core
open WebSharper.JavaScript
open System.Runtime.InteropServices

[<Proxy(typeof<System.MemoryExtensions>)>]
[<JavaScript>]
type private MemoryExtensionsProxy =

    static member private MinMax<'T>(span: ReadOnlySpanProxy<'T>, comparer: IComparer<'T>, isMax: bool, defaultValue: 'T) : 'T =
        let values = As<'T[]> span
        let mutable value = defaultValue
        let mutable i = 0
        if As<obj> value ===. null then
            while i < values.Length && As<obj> value ===. null do
                value <- values.[i]
                i <- i + 1
        else
            if values.Length = 0 then
                raise (InvalidOperationException("Sequence contains no elements"))
            value <- values.[0]
            i <- 1
        while i < values.Length do
            let next = values.[i]
            if As<obj> next !==. null then
                let comparison = comparer.Compare(next, value)
                if (if isMax then comparison > 0 else comparison < 0) then
                    value <- next
            i <- i + 1
        value

    [<Inline>]
    static member Max<'T>(span: ReadOnlySpanProxy<'T>) : 'T =
        MemoryExtensionsProxy.MinMax(span, Comparer<'T>.Default, true, Unchecked.defaultof<'T>)

    [<Inline>]
    static member Max<'T>(span: ReadOnlySpanProxy<'T>, comparer: IComparer<'T>) : 'T =
        let comparer = if isNull comparer then Comparer<'T>.Default :> IComparer<'T> else comparer
        MemoryExtensionsProxy.MinMax(span, comparer, true, Unchecked.defaultof<'T>)

    [<Inline>]
    static member Min<'T>(span: ReadOnlySpanProxy<'T>) : 'T =
        MemoryExtensionsProxy.MinMax(span, Comparer<'T>.Default, false, Unchecked.defaultof<'T>)

    [<Inline>]
    static member Min<'T>(span: ReadOnlySpanProxy<'T>, comparer: IComparer<'T>) : 'T =
        let comparer = if isNull comparer then Comparer<'T>.Default :> IComparer<'T> else comparer
        MemoryExtensionsProxy.MinMax(span, comparer, false, Unchecked.defaultof<'T>)

    [<Inline>]
    static member Contains<'T>(span: ReadOnlySpanProxy<'T>, value: 'T) =
        Enumerable.Contains(As<'T[]>span, value)

    [<Inline>]
    static member Contains<'T>(span: ReadOnlySpanProxy<'T>, value: 'T, comparer: IEqualityComparer<'T>) =
        Enumerable.Contains(As<'T[]>span, value, comparer)

    [<Name "SequenceCompareTo">]
    static member SequenceCompareToImpl<'T>(x: 'T[], y: 'T[], comparer: IComparer<'T>): int =
        let rec comp i =
            match x.JS.Length < i + 1, y.JS.Length < i + 1 with
            | true, true -> 0
            | true, false -> -1
            | false, true -> 1
            | _ ->
                match comparer.Compare(x.JS[i], y.JS[i]) with
                | 0 -> comp (i + 1)
                | res -> res
        comp 0

    [<Inline>]
    static member SequenceCompareTo<'T>(span: ReadOnlySpanProxy<'T>, other: ReadOnlySpanProxy<'T>) =
        MemoryExtensionsProxy.SequenceCompareToImpl(As<'T[]>span, As<'T[]>other, Comparer<'T>.Default)

    [<Inline>]
    static member SequenceCompareTo<'T>(span: ReadOnlySpanProxy<'T>, other: ReadOnlySpanProxy<'T>, comparer: IComparer<'T>) =
        MemoryExtensionsProxy.SequenceCompareToImpl(As<'T[]>span, As<'T[]>other, comparer)

    [<Inline>]
    static member SequenceEqual<'T>(span: ReadOnlySpanProxy<'T>, other: ReadOnlySpanProxy<'T>) =
        Enumerable.SequenceEqual<'T>(As<'T[]>span, As<'T[]>other)     

    [<Inline>]
    static member SequenceEqual<'T>(span: ReadOnlySpanProxy<'T>, other: ReadOnlySpanProxy<'T>, comparer: IEqualityComparer<'T>) =
        Enumerable.SequenceEqual<'T>(As<'T[]>span, As<'T[]>other, comparer)     
