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

namespace IntelliFactory.WebSharper

open IntelliFactory.WebSharper
module C = IntelliFactory.WebSharper.Concurrency

[<Proxy "Microsoft.FSharp.Control.FSharpAsyncBuilder, \
         FSharp.Core, Version=2.0.0.0, Culture=neutral, \
         PublicKeyToken=b03f5f7f11d50a3a">]
type private AsyncBuilderProxy [<Inline "null">]() =

    [<Inline>]
    [<JavaScript>]
    member this.Bind(x: Async<'T1>, f: 'T1 -> Async<'T2>) : Async<'T2> =
        As (C.Bind (As x) (As f))

    [<Inline>]
    [<JavaScript>]
    member this.Delay(f: unit -> Async<'T>) : Async<'T> =
        As (C.Delay (As f))

    [<Inline>]
    [<JavaScript>]
    member this.Return(x: 'T) : Async<'T> = As (C.Return x)

    [<Inline>]
    [<JavaScript>]
    member this.ReturnFrom(x: Async<'T>) : Async<'T> = x

    [<Inline>]
    [<JavaScript>]
    member this.Combine(a: Async<unit>, b: Async<'T>) : Async<'T> =
        this.Bind(a, (fun _ -> b))

    [<Inline>]
    [<JavaScript>]
    member this.TryFinally(a: Async<'T>, f: unit -> unit) : Async<'T> =
        As (C.TryFinally (As a) f)

    [<Inline>]
    [<JavaScript>]
    member this.TryWith(a: Async<'T>, f: exn -> Async<'T>) : Async<'T> =
        As (C.TryWith (As a) (As f))

    [<Inline>]
    [<JavaScript>]
    member this.Using<'T, 'TResult when 'T :> System.IDisposable>
                     (x: 'T, f: 'T -> Async<'TResult>) : Async<'TResult> =
        this.TryFinally(f x, fun () -> (x :> System.IDisposable).Dispose())

    [<Inline>]
    [<JavaScript>]
    member this.Zero() : Async<unit> =
        this.Return()

