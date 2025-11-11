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

    [<Inline>]
    static member SequenceEqual<'T>(span: ReadOnlySpanProxy<'T>, other: ReadOnlySpanProxy<'T>, comparer: IEqualityComparer<'T>) =
        Enumerable.SequenceEqual<'T>(As<'T[]>span, As<'T[]>other, if isNull comparer then EqualityComparer.Default :> IEqualityComparer<'T> else comparer)     

    [<Inline>]
    static member Contains<'T>(span: ReadOnlySpanProxy<'T>, value: 'T) =
        Enumerable.Contains(As<'T[]>span, value)