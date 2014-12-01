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

type private OCE = System.OperationCanceledException

type Result<'T> =
    | Ok of 'T
    | No of exn
    | Cc //of OCE

type CT =
    { 
        [<Name "c">] mutable IsCancellationRequested : bool 
        [<Name "r">] Registrations : (unit -> unit)[]
    }

[<JavaScript>]
let register (ct: CT) (callback: unit -> unit) =
    let i = ct.Registrations?push(callback) - 1
    New [
        "Dispose" => fun () -> ct.Registrations.[i] <- ignore
    ] : System.IDisposable

type Continuation<'T>   = Result<'T> -> unit
type Concurrent<'T>     = Continuation<'T> * CT -> unit
and private C<'T>       = Concurrent<'T>

type private Queue<'T>  = System.Collections.Generic.Queue<'T>
type Milliseconds       = int

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
                    JavaScript.SetTimeout tick 0 |> ignore
                    loop <- false

    [<JavaScript>]
    member this.Fork(action: unit -> unit) =
        robin.Enqueue action
        if idle then
            idle <- false
            JavaScript.SetTimeout tick 0 |> ignore

[<JavaScript>]
let private scheduler = Scheduler()

[<JavaScript>]
let internal defCT = ref(new System.Threading.CancellationTokenSource())

[<JavaScript>]
[<Inline>]
let private fork action = scheduler.Fork action

[<JavaScript>]
let Return (x: 'T) : C<'T> =
    fun (k, ct) -> 
        if ct.IsCancellationRequested then k Cc else k (Ok x)

[<JavaScript>]
let Bind (r: C<'T>, f: 'T -> C<'R>) =
    fun ((k, ct) as c) ->
        if ct.IsCancellationRequested then k Cc else
        r (function Ok x -> fork (fun () -> try f x c with e -> k (No e))
                  | No e -> fork (fun () -> k (No e))
                  | Cc   -> fork (fun () -> k Cc)
        , ct)

[<JavaScript>]
let Ignore (r: C<'T>): C<unit> =
    Bind (r, fun _ -> Return ())

[<JavaScript>]
let Delay (mk: unit -> C<'T>) : C<'T> =
    fun ((k, ct) as c) ->
        if ct.IsCancellationRequested then k Cc else
        try mk () c with e -> k (No e)

[<JavaScript>]
let TryFinally (run: C<'T>, f: unit -> unit) =
    fun (k, ct) ->
        if ct.IsCancellationRequested then k Cc else
        run (fun r -> try f (); k r with e -> if ct.IsCancellationRequested then k Cc else k (No e)
        , ct)

[<JavaScript>]
let TryWith (r: C<'T>, f: exn -> C<'T>) =
    fun ((k, ct) as c) ->
        if ct.IsCancellationRequested then k Cc else
        r (function Ok x -> k (Ok x)
                  | No e -> try f e c with e -> k (No e)
                  | Cc   -> k Cc
        , ct)

[<JavaScript>]
let Catch (r : C<'T>) : C<Choice<'T, exn>> =
    fun ((k, ct) as c) ->
        if ct.IsCancellationRequested then k Cc else
        try r (function Ok x -> k (Ok (Choice1Of2 x))
                      | No e -> k (Ok (Choice2Of2 e))
                      | Cc   -> k Cc
            , ct)
        with e -> k (Ok (Choice2Of2 e))

[<JavaScript>]
let GetCT : C<CT> =
    fun (k, ct) -> k (Ok ct)

[<JavaScript>]
let FromContinuations (subscribe: ('T -> unit) * (exn -> unit) * (OCE -> unit) -> unit) : C<'T> =
    fun (k, ct) -> 
        if ct.IsCancellationRequested then k Cc else
        let continued = ref false
        let once cont : unit =
            if !continued then failwith "A continuation provided by Async.FromContinuations was invoked multiple times" else
            continued := true
            fork cont   
        subscribe (
            fun a -> once (fun () -> k (Ok a))
        ,   fun e -> once (fun () -> k (No e))
        ,   fun _ -> once (fun () -> k Cc))

[<JavaScript>]
let StartWithContinuations (c: C<'T>, s: 'T -> unit, f: exn -> unit, cc: OCE -> unit, ctOpt) =
    let ct = defaultArg ctOpt (As !defCT)
    fork (fun () -> 
        c (function Ok x -> s x
                  | No e -> f e
                  | Cc   -> cc (OCE())
        , ct))

[<JavaScript>]
let Start (c: C<unit>, ctOpt) =
    StartWithContinuations (c, ignore, 
        fun exn -> JavaScript.LogMore ("WebSharper: Uncaught asynchronous exception", exn)
    , ignore, ctOpt)

[<JavaScript>]
let AwaitEvent (e: IEvent<'T>) : C<'T> =
    fun (k, ct) ->
        if ct.IsCancellationRequested then k Cc else
        let rec sub : System.IDisposable =
            e.Subscribe (fun x -> 
                sub.Dispose()
                creg.Dispose()
                fork (fun () -> k (Ok x))        
            )
        and creg : System.IDisposable = 
            register ct (fun () -> 
                sub.Dispose()
                fork (fun () -> k Cc)    
            )
        ()

[<JavaScript>]
let Sleep (ms: Milliseconds) : C<unit> =
    fun (k, ct) ->
        if ct.IsCancellationRequested then k Cc else
        let rec pending =
            JavaScript.SetTimeout (fun () -> 
                creg.Dispose()
                fork (fun () -> k (Ok ()))
            ) ms
        and creg : System.IDisposable =
            register ct (fun () -> 
                JavaScript.ClearTimeout pending
                fork (fun () -> k Cc)
            )
        ()

[<JavaScript>]
let Parallel (cs: seq<C<'T>>) =
    let cs = Array.ofSeq cs
    if cs.Length = 0 then Return [||] else
    fun ((k, ct) as c) ->
        if ct.IsCancellationRequested then k Cc else
        let n = Array.length cs
        let o = ref n
        let a = Array.create n Unchecked.defaultof<_>
        let accept i x =
            match !o, x with
            | 0, _     -> ()
            | 1, Ok x  -> a.[i] <- x; o := 0; k (Ok a)
            | n, Ok x  -> a.[i] <- x; o := n - 1
            | n, No e  -> o := 0; k (No e)
            | n, Cc    -> o := 0; k Cc
        Array.iteri (fun i run ->
            fork (fun () -> run (accept i, ct)))
            cs

[<JavaScript>]
let StartChild (r : C<'T>) : C<C<'T>> =
    fun (k: Continuation<C<'T>>, ct) ->
        let cached = ref None
        let queue  = Queue()
        fork (fun _ ->
            r (fun res ->
                cached := Some res
                while queue.Count > 0 do
                    queue.Dequeue() res
            , ct))
        let r2 (k: Continuation<'T>, _: CT) =
            match cached.Value with
            | Some x    -> k x
            | None      -> queue.Enqueue k
        k (Ok r2)

[<JavaScript>]
let Using (x: 'U, f: 'U -> C<'T>) =
    TryFinally (f x, fun () -> (x :> System.IDisposable).Dispose())

[<JavaScript>]
let rec While (g: unit -> bool, c: C<unit>) = 
    if g() then 
        Bind (c, fun () -> While (g, c)) 
    else
        Return ()

[<JavaScript>]
let rec For (s: seq<'T>, b: 'T -> C<unit>) =
    let ie = s.GetEnumerator()
    While (fun () -> ie.MoveNext()
        , Delay (fun () -> b ie.Current))
//    // if IEnumerator<_> would always have IDisposable
//    Using (s.GetEnumerator()) (fun ie ->
//        While (fun () -> ie.MoveNext())
//            (Delay (fun () -> b ie.Current)))
