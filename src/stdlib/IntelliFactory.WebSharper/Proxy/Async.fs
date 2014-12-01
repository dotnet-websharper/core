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

type private CT  = System.Threading.CancellationToken
type private CTS  = System.Threading.CancellationTokenSource
type private CTR  = System.Threading.CancellationTokenRegistration
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
        C.Start (As computation, As t)

    [<Inline>]
    [<JavaScript>]
    static member Ignore (computation: Async<'T>) : Async<unit> =
        As (C.Ignore (As computation))

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
        C.StartWithContinuations (As op, c1, c2, c3, As t)

    [<Inline>]
    [<JavaScript>]
    static member FromContinuations(callback: (('T -> unit) *
                                               (exn -> unit) *
                                               (OCE -> unit)) -> unit)
                                    : Async<'T> =
        As (C.FromContinuations callback)

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
        C.Start (As c, As t)

    [<Inline>]
    [<JavaScript>]
    static member DefaultCancellationToken : CT =
        As !C.defCTS

    [<Inline>]
    [<JavaScript>]
    static member CancelDefaultToken() : unit =
        let cts = !C.defCTS
        C.defCTS := new CTS()
        cts.Cancel()    

    [<Inline>]
    [<JavaScript>]
    static member CancellationToken : Async<CT> =
        As C.GetCT

[<Proxy(typeof<System.Action>)>]
type ActionProxy =
    [<Inline "$action">]
    new (action: unit -> unit) = {}
        
    [<Inline "$this()">]
    member this.Invoke() = ()

[<Proxy(typeof<CTR>)>]
type private CancellationTokenRegistrationProxy =
    [<Stub>] abstract member Dispose : unit -> unit

[<Proxy(typeof<CT>)>]
type private CancellationTokenProxy =
    [<Inline "$this.c">]
    member this.IsCancellationRequested = X<bool>

    [<JavaScript>]
    [<Inline>]
    member this.Register(callback: System.Action) =
        As<CTR> (C.register (As this) callback.Invoke)
        
[<Proxy(typeof<CTS>)>]
[<Name "CancellationTokenSource">]
type private CancellationTokenSourceProxy [<JavaScript>] () =
    let mutable c = false

    let mutable pending = None

    let r = [||] : (unit -> unit)[]

    [<JavaScript>]
    [<Inline>]
    member this.IsCancellationRequested = c

    member this.Token 
        with [<Inline "$this">] get() = X<CT>

    [<JavaScript>]
    member this.Cancel() =
        if not c then
            c <- true
            let errors = 
                r |> Array.choose (fun a -> 
                    try a()
                        None
                    with e -> Some e
                )
            if errors.Length > 0 then
                raise (System.AggregateException(errors))    
            
    [<JavaScript>]
    member this.Cancel(throwOnFirstException) =
        if not throwOnFirstException then
            this.Cancel()
        else
            if not c then
                c <- true
                r |> Array.iter (fun a -> a())   
 
    [<JavaScript>]
    member this.CancelAfter(delay: int) =
        if not c then
            pending |> Option.iter JavaScript.ClearTimeout
            pending <- Some <| JavaScript.SetTimeout this.Cancel delay

    [<JavaScript>]
    [<Inline>]
    member this.CancelAfter(delay: System.TimeSpan) = this.CancelAfter(As<int> delay)

    [<JavaScript>]
    static member CreateLinkedTokenSource(tokens: CT[]) =
        let cts = new CTS()
        tokens |> Array.iter (fun t -> t.Register(fun () -> cts.Cancel()) |> ignore)

    [<JavaScript>]
    static member CreateLinkedTokenSource(t1: CT, t2: CT) =
        CancellationTokenSourceProxy.CreateLinkedTokenSource [| t1; t2 |]