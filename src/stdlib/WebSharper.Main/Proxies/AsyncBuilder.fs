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

#if JAVASCRIPT
open System.Collections.Generic

open WebSharper
open WebSharper.JavaScript
module C = WebSharper.Concurrency

[<Proxy "Microsoft.FSharp.Control.FSharpAsyncBuilder, \
         FSharp.Core, Culture=neutral, \
         PublicKeyToken=b03f5f7f11d50a3a">]
type private AsyncBuilderProxy [<Inline "null">]() =

    [<Inline>]
    member this.Bind(x: Async<'T1>, f: 'T1 -> Async<'T2>) : Async<'T2> =
        As (C.Bind (As x, As f))

    [<Inline>]
    member this.Delay(f: unit -> Async<'T>) : Async<'T> =
        As (C.Delay (As f))

    [<Inline>]
    member this.Return(x: 'T) : Async<'T> = As (C.Return x)

    [<Inline>]
    member this.ReturnFrom(x: Async<'T>) : Async<'T> = x

    [<Inline>]
    member this.Combine(a: Async<unit>, b: Async<'T>) : Async<'T> =
        As (C.Combine (As a, As b))

    [<Inline>]
    member this.TryFinally(a: Async<'T>, f: unit -> unit) : Async<'T> =
        As (C.TryFinally (As a, f))

    [<Inline>]
    member this.TryWith(a: Async<'T>, f: exn -> Async<'T>) : Async<'T> =
        As (C.TryWith (As a, As f))

    [<Inline>]
    member this.Using<'T, 'TResult when 'T :> System.IDisposable>
                     (x: 'T, f: 'T -> Async<'TResult>) : Async<'TResult> =
        As (C.Using (x, As f))

    [<Inline>]
    member this.Zero() : Async<unit> = As C.Zero

    [<Inline>]
    member this.While(g: unit -> bool, b:Async<unit>) : Async<unit> = 
        As (C.While (g, As b))

    [<Inline>]
    member this.For(s: seq<'T>, b: 'T -> Async<unit>) : Async<unit> =
        As (C.For (s, As b))
#endif