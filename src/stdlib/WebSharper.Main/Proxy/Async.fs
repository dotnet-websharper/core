// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

type private CT  = System.Threading.CancellationToken
type private CTS  = System.Threading.CancellationTokenSource
type private CTR  = System.Threading.CancellationTokenRegistration
type private OCE = System.OperationCanceledException
module C = WebSharper.Concurrency

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
        As (C.AwaitEvent (As ev, t))

    [<Inline>]
    [<JavaScript>]
    static member AwaitTask(t : System.Threading.Tasks.Task) : Async<unit> =
        As (C.AwaitTask t)

    [<Inline>]
    [<JavaScript>]
    static member AwaitTask(t : System.Threading.Tasks.Task<'T>) : Async<'T> =
        As (C.AwaitTask1 t)

    [<Inline>]
    [<JavaScript>]
    static member StartChild(a: Async<'T>, ?timeOut: int) : Async<Async<'T>> =
        As (C.StartChild (As a, timeOut))

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
    static member StartAsTask (a: Async<'T>, ?opt :System.Threading.Tasks.TaskCreationOptions, ?t: CT) 
        : System.Threading.Tasks.Task<'T> =
        C.StartAsTask(As a, As t)        

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

    [<Inline>]
    [<JavaScript>]
    static member OnCancel(action: unit -> unit) : Async<System.IDisposable> =
        As (C.OnCancel action)
    
    [<Inline>]
    [<JavaScript>]
    static member TryCancelled(p: Async<'T>, f: OCE -> unit) : Async<'T> =
        As (C.TryCancelled(As p, f))

[<Proxy(typeof<CT>)>]
type private CancellationTokenProxy =
    [<Inline "$this.c">]
    member this.IsCancellationRequested = X<bool>

    [<JavaScript>]
    [<Inline>]
    member this.Register(callback: System.Action) =
        As<CTR> (C.Register (As this) callback.Invoke)

    [<JavaScript>]
    [<Inline>]
    member this.ThrowIfCancellationRequested() =
        if this.IsCancellationRequested then raise (OCE()) 

    [<JavaScript>]
    [<Inline>]
    static member None = As<CT> C.noneCT
        
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
            pending |> Option.iter JS.ClearTimeout
            pending <- Some <| JS.SetTimeout this.Cancel delay

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
