// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

type private CT  = System.Threading.CancellationToken
type private OCE = System.OperationCanceledException
module C = IntelliFactory.WebSharper.Concurrency

[<Proxy(typeof<Async>)>]
type private AsyncProxy =

    [<Inline>]
    [<JavaScript>]
    static member Catch(a: Async<'T>) : Async<Choice<'T,exn>>  =
        As (C.Catch (As a))

    [<Inline>]
    [<JavaScript>]
    static member Start(computation: Async<unit>, ?t: CT) : unit =
        C.Start (As computation)

    [<Inline>]
    [<JavaScript>]
    static member Ignore (computation: Async<'T>) : Async<unit> =
        As (C.Bind (As computation) (fun _ -> C.Return ()))

    [<Inline>]
    [<JavaScript>]
    static member Sleep milliseconds : Async<unit> =
        As (C.Sleep milliseconds)

    [<Inline>]
    [<JavaScript>]
    static member StartWithContinuations(op: Async<'T>,
                                         c1: 'T -> unit,
                                         c2: exn -> unit,
                                         c3: OCE -> unit,
                                         ?t: CT) : unit =
        C.StartWithContinuations (As op) c1 c2

    [<Inline>]
    [<JavaScript>]
    static member FromContinuations(callback: (('T -> unit) *
                                               (exn -> unit) *
                                               (OCE -> unit)) -> unit)
                                    : Async<'T> =
        As (C.FromContinuations (fun ok no -> callback (ok, no, ignore)))

    [<Inline>]
    [<JavaScript>]
    static member AwaitEvent(ev: IEvent<'D,'T>, ?t: unit -> unit) : Async<'T> =
        As (C.AwaitEvent (As ev))

    [<Inline>]
    [<JavaScript>]
    static member StartChild(a: Async<'T>, ?timeOut: int) : Async<Async<'T>> =
        As (C.StartChild (As a))

    [<Inline>]
    [<JavaScript>]
    static member Parallel(cs: seq<Async<'T>>) : Async<'T []> =
        As (C.Parallel (As cs))

    [<Inline>]
    [<JavaScript>]
    static member StartImmediate(c: Async<unit>, ?t: CT) : unit =
        C.Start (As c)
