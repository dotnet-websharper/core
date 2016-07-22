// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Implements concurrency primitives.
module internal WebSharper.Concurrency

open WebSharper
open WebSharper.JavaScript

type private OCE = System.OperationCanceledException

type Result<'T> =
    | Ok of 'T
    | No of exn
    | Cc of OCE
  
type CT =
    { 
        [<Name "c">] mutable IsCancellationRequested : bool 
        [<Name "r">] Registrations : (unit -> unit)[]
    }

[<Inline "$arr.push($item)">]
let private push arr item = X<int>

[<JavaScript>]
let internal noneCT =           
    { 
        IsCancellationRequested = false
        Registrations = [||]
    }

[<JavaScript>]
let internal Register (ct: CT) (callback: unit -> unit) =
    if ct ===. noneCT then
        { new System.IDisposable with
            member this.Dispose() = ()
        }
    else
        let i = push ct.Registrations callback - 1
        { new System.IDisposable with
            member this.Dispose() = ct.Registrations.[i] <- ignore
        }

[<JavaScript>]
type AsyncBody<'T> =
    {
        k  : Result<'T> -> unit
        ct : CT
    }

type Concurrent<'T>     = AsyncBody<'T> -> unit
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
                    JS.SetTimeout tick 0 |> ignore
                    loop <- false

    [<JavaScript>]
    member this.Fork(action: unit -> unit) =
        robin.Enqueue action
        if idle then
            idle <- false
            JS.SetTimeout tick 0 |> ignore

[<JavaScript>]
let private scheduler = Scheduler()

[<JavaScript>]
let internal defCTS = ref(new System.Threading.CancellationTokenSource())

[<JavaScript>]
[<Inline>]
let fork action = scheduler.Fork action

[<JavaScript>]
[<Inline>]
let private cancel c = c.k (Cc (new OCE(As<System.Threading.CancellationToken> c.ct)))

[<JavaScript>]
let private checkCancel r =
    ()
    fun c -> if c.ct.IsCancellationRequested then cancel c else r c

[<JavaScript>]
let Return (x: 'T) : C<'T> =
    checkCancel <| fun c -> c.k (Ok x)

[<JavaScript>]
let Bind (r: C<'T>, f: 'T -> C<'R>) =
    checkCancel <| fun c ->
        r { 
            k = function 
                | Ok x -> fork (fun () -> try f x c with e -> c.k (No e))
                | res  -> fork (fun () -> c.k (As res)) // error or cancellation
            ct = c.ct
        }

[<JavaScript>]
let Combine (a: C<unit>, b: C<'T>) : C<'T> = 
    Bind (a, fun _ -> b)

[<JavaScript>]
let Ignore (r: C<'T>): C<unit> =
    Bind (r, fun _ -> Return ())

[<JavaScript>]
let Delay (mk: unit -> C<'T>) : C<'T> =
    checkCancel <| fun c ->
        try mk () c with e -> c.k (No e)

[<JavaScript>]
let TryFinally (run: C<'T>, f: unit -> unit) =
    checkCancel <| fun c ->
        run {
            k = fun r -> 
                try f ()
                    c.k r 
                with e -> c.k (No e)
            ct = c.ct
        }

[<JavaScript>]
let TryWith (r: C<'T>, f: exn -> C<'T>) =
    checkCancel <| fun c ->
        r {
            k = function
                | Ok x -> c.k (Ok x)
                | No e as res -> try f e c with e -> c.k (As res)
                | res -> c.k (As res)
            ct = c.ct
        }

[<JavaScript>]
let Catch (r : C<'T>) : C<Choice<'T, exn>> =
    checkCancel <| fun c ->
        try r {
                k = function 
                    | Ok x -> c.k (Ok (Choice1Of2 x))
                    | No e -> c.k (Ok (Choice2Of2 e))
                    | res  -> c.k (As res)
                ct = c.ct
            }
        with e -> c.k (Ok (Choice2Of2 e))

[<JavaScript>]
let GetCT : C<CT> =
    checkCancel <| fun c -> c.k (Ok c.ct)

[<JavaScript>]
let FromContinuations (subscribe: ('T -> unit) * (exn -> unit) * (OCE -> unit) -> unit) : C<'T> =
    checkCancel <| fun c ->
        let continued = ref false
        let once cont : unit =
            if !continued then failwith "A continuation provided by Async.FromContinuations was invoked multiple times" else
            continued := true
            fork cont   
        subscribe (
            fun a -> once (fun () -> c.k (Ok a))
        ,   fun e -> once (fun () -> c.k (No e))
        ,   fun e -> once (fun () -> c.k (Cc e))
        )

[<JavaScript>]
let StartWithContinuations (c: C<'T>, s: 'T -> unit, f: exn -> unit, cc: OCE -> unit, ctOpt) =
    let ct = defaultArg ctOpt (As !defCTS)
    fork (fun () -> 
        c {
            k = function
                | Ok x -> s x
                | No e -> f e
                | Cc e -> cc e
            ct = ct
        }
    )

[<JavaScript>]
let Start (c: C<unit>, ctOpt) =
    StartWithContinuations (c, ignore, 
        fun exn -> Console.Log ("WebSharper: Uncaught asynchronous exception", exn)
    , ignore, ctOpt)

#nowarn "40"

[<JavaScript>]
let AwaitEvent (e: IEvent<'T>, ca: option<unit -> unit>) : C<'T> =
    checkCancel <| fun c ->
        let rec sub : System.IDisposable =
            e.Subscribe (fun x -> 
                sub.Dispose()
                creg.Dispose()
                fork (fun () -> c.k (Ok x))        
            )
        and creg : System.IDisposable = 
            Register c.ct (fun () -> 
                match ca with
                | Some ca ->
                    ca()
                | _ ->
                    sub.Dispose()
                    fork (fun () -> cancel c)    
            ) 
        ()

[<JavaScript>]
let AwaitTask (t: System.Threading.Tasks.Task) : C<unit> =
    FromContinuations (fun (ok, err, cc) ->
        if t.Status = System.Threading.Tasks.TaskStatus.Created then
            t.Start()
        t.ContinueWith(fun t ->
            if t.IsCanceled then
                cc (OCE())
            elif t.IsFaulted then
                err t.Exception
            else
                ok()   
        ) |> ignore
    )

[<JavaScript>]
let AwaitTask1 (t: System.Threading.Tasks.Task<'T>) : C<'T> =
    FromContinuations (fun (ok, err, cc) ->
        if t.Status = System.Threading.Tasks.TaskStatus.Created then
            t.Start()
        t.ContinueWith(fun (t: System.Threading.Tasks.Task<'T>) ->
            if t.IsCanceled then
                cc (OCE())
            elif t.IsFaulted then
                err t.Exception
            else
                ok t.Result  
        ) |> ignore
    )

[<JavaScript>]
let StartAsTask (c: C<'T>, ctOpt) =
    let tcs = System.Threading.Tasks.TaskCompletionSource<'T>()
    StartWithContinuations (c, tcs.SetResult, tcs.SetException, (fun _ -> tcs.SetCanceled()), ctOpt)
    tcs.Task

[<JavaScript>]
let Sleep (ms: Milliseconds) : C<unit> =
    checkCancel <|  fun c ->
        let rec pending =
            JS.SetTimeout (fun () -> 
                creg.Dispose()
                fork (fun () -> c.k (Ok ()))
            ) ms
        and creg : System.IDisposable =
            Register c.ct (fun () -> 
                JS.ClearTimeout pending
                fork (fun () -> cancel c)
            )
        ()

[<JavaScript>]
let Parallel (cs: seq<C<'T>>) : C<'T[]> =
    let cs = Array.ofSeq cs
    if cs.Length = 0 then Return [||] else
    checkCancel <| fun c ->
        let n = Array.length cs
        let o = ref n
        let a = As<'T[]>(JavaScript.Array(n))
        let accept i x =
            match !o, x with
            | 0, _     -> ()
            | 1, Ok x  -> a.[i] <- x; o := 0; c.k (Ok a)
            | n, Ok x  -> a.[i] <- x; o := n - 1
            | n, res   -> o := 0; c.k (As res)
        Array.iteri (fun i run ->
            fork (fun () -> run { k = accept i; ct = c.ct }))
            cs

[<JavaScript>]
let StartChild (r : C<'T>, t: Milliseconds option) : C<C<'T>> =
    checkCancel <| fun c ->
        let inTime = ref true
        let cached = ref None
        let queue  = Queue()
        let tReg =
            match t with
            | Some timeout ->
                JS.SetTimeout (fun () ->
                    inTime := false
                    let err = No (System.TimeoutException())
                    while queue.Count > 0 do
                        queue.Dequeue() err
                ) timeout |> Some     
            | _ -> None
        fork (fun _ ->
            r {
                k = fun res ->
                    if !inTime then
                        cached := Some res
                        match tReg with
                        | Some r -> JS.ClearTimeout r
                        | _ -> ()
                        while queue.Count > 0 do
                            queue.Dequeue() res
                ct = c.ct
            }
        )
        let r2 =            
            checkCancel <| fun c2 ->
                if !inTime then
                    match cached.Value with
                    | Some x    -> c2.k x
                    | None      -> queue.Enqueue c2.k
                else c2.k (No (System.TimeoutException()))
        c.k (Ok r2)

[<JavaScript>]
let OnCancel (action: unit -> unit) : C<System.IDisposable> =
    checkCancel <| fun c -> c.k (Ok (Register c.ct action))

[<JavaScript>]
let TryCancelled (run: C<'T>, comp: OCE -> unit) : C<'T> =
    checkCancel <| fun c ->
        run {
            k = function
                | Cc e as res ->
                    comp e
                    c.k res
                | res -> c.k res
            ct = c.ct
        }

[<JavaScript>]
let Using (x: 'U, f: 'U -> C<'T>) =
    TryFinally (f x, fun () -> (x :> System.IDisposable).Dispose())

[<JavaScript>]
let rec While (g: unit -> bool, c: C<unit>) : C<unit> = 
    if g() then 
        Bind (c, fun () -> While (g, c)) 
    else
        Return ()

[<JavaScript>]
let rec For (s: seq<'T>, b: 'T -> C<unit>) =
    Using (s.GetEnumerator(), fun ie -> 
        While ((fun () -> ie.MoveNext()), 
            Delay (fun () -> b ie.Current)))
