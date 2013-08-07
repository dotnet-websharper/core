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

/// Implements concurrency primitives.
module internal IntelliFactory.WebSharper.Concurrency

open IntelliFactory.WebSharper

type Result<'T> =
    | Ok of 'T
    | No of exn

type Continuation<'T>   = Result<'T> -> unit
type Concurrent<'T>     = C of (Continuation<'T> -> unit)
and private C<'T>       = Concurrent<'T>

type private Queue<'T>  = System.Collections.Generic.Queue<'T>
type Milliseconds       = int

[<Inline "setTimeout($action, $ms)">]
let private schedule (ms: Milliseconds) (action: unit -> unit) =
    JavaScript.ClientSide<unit>

[<Inline "setTimeout($action, 0)">]
let private spark (action: unit -> unit) = JavaScript.ClientSide<unit>

type private Scheduler [<JavaScript>]() =
    let mutable idle    = true
    let robin           = Queue<unit->unit>()

    [<JavaScript>]
    let rec tick () =
        let t = System.DateTime.Now
        let mutable loop = true
        while loop do
            match robin.Count with
            | 0 ->
                idle <- true
                loop <- false
            | _ ->
                robin.Dequeue()()
                if System.DateTime.Now - t > System.TimeSpan.FromMilliseconds 40. then
                    spark tick
                    loop <- false

    [<JavaScript>]
    member this.Fork(action: unit -> unit) =
        robin.Enqueue action
        if idle then
            idle <- false
            spark tick

[<JavaScript>]
let private scheduler = Scheduler()

[<JavaScript>]
let private fork action = scheduler.Fork action

[<JavaScript>]
let Return x = C (fun k -> k (Ok x))

[<JavaScript>]
let Run (C run) x = run x

[<JavaScript>]
let Bind (C r) f =
    C (fun k ->
        r (function Ok x -> fork (fun () -> try Run (f x) k with e -> k (No e))
                  | No e -> k (No e)))

[<JavaScript>]
let Delay mk =
    C (fun k -> try Run (mk ()) k with e -> k (No e))

[<JavaScript>]
let TryFinally (C run) f =
    C (fun k -> run (fun r -> try f (); k r
                              with e -> k (No e)))

[<JavaScript>]
let TryWith (C r) f =
    C (fun k -> r (function Ok x -> k (Ok x)
                          | No e -> try Run (f e) k
                                    with e -> k (No e)))

[<JavaScript>]
let Catch (C r : C<'T>) : C<Choice<'T, exn>> =
    C (fun k -> try r (function Ok x -> k (Ok (Choice1Of2 x))
                              | No e -> k (Ok (Choice2Of2 e)))
                with e -> k (Ok (Choice2Of2 e)))

[<JavaScript>]
let FromContinuations subscribe =
    C (fun k -> subscribe (fun a -> k (Ok a)) (fun (e: exn) -> k (No e)))

[<JavaScript>]
let StartWithContinuations (c: C<'T>) (s: 'T -> unit) (f: exn -> unit) =
    fork (fun () -> Run c (function Ok x   -> s x
                                  | No exn -> f exn))

[<JavaScript>]
let Start (c: C<unit>) = StartWithContinuations c ignore raise

[<JavaScript>]
let AwaitEvent (e: IEvent<'T>) =
    C (fun k -> let sub = ref Unchecked.defaultof<System.IDisposable>
                sub := e.Subscribe (fun x -> (!sub).Dispose(); k (Ok x)))

[<JavaScript>]
let Sleep (ms: Milliseconds) =
    C (fun k -> schedule ms (fun () -> k (Ok ())))

[<JavaScript>]
let Parallel (cs: seq<C<'T>>) =
    let cs = Array.ofSeq cs
    C (fun k ->
        let n = Array.length cs
        let o = ref n
        let a = Array.create n Unchecked.defaultof<_>
        let accept i x =
            match !o, x with
            | 0, _     -> ()
            | 1, Ok x  -> a.[i] <- x; o := 0; k (Ok a)
            | n, Ok x  -> a.[i] <- x; o := n - 1
            | n, No e  -> o := 0; k (No e)
        Array.iteri (fun i (C run) ->
            fork (fun () -> run (accept i)))
            cs)

[<JavaScript>]
let StartChild (C r : Concurrent<'T>) =
    C (fun (k: Continuation<C<'T>>) ->
        let cached = ref None
        let queue  = Queue()
        fork (fun _ ->
            r (fun res ->
                cached := Some res
                while queue.Count > 0 do
                    queue.Dequeue() res))
        let r2 (k: Continuation<'T>) =
            match cached.Value with
            | Some x    -> k x
            | None      -> queue.Enqueue k
        k (Ok (C r2)))

