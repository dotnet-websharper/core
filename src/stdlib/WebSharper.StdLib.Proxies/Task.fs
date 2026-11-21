// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

open System.Threading
open System.Threading.Tasks

[<Proxy(typeof<Task>); Name "Task">]
type private TaskProxy(action: System.Action, token: CT, status, exc) =
    [<Name "status">]
    let mutable status = status
    [<Name "continuations">]
    let continuations = [||] : TaskProxy[]
    [<Name "exc">]
    let mutable exc = exc : System.AggregateException

    abstract Execute : unit -> unit
    default this.Execute() = action.Invoke()

    member this.Exception = exc

    member this.IsCanceled = 
        status = TaskStatus.Canceled

    member this.IsCompleted = 
        status = TaskStatus.RanToCompletion || status = TaskStatus.Faulted || status = TaskStatus.Canceled

    member this.IsFaulted =
        status = TaskStatus.Faulted

    member this.Status = status

    new (action) = TaskProxy(action, CT.None, TaskStatus.Created, null)

    new (action, ct) = TaskProxy(action, ct, TaskStatus.Created, null)
    
    new (action: System.Action<obj>, obj: obj) = TaskProxy((fun () -> action.Invoke(obj)), CT.None, TaskStatus.Created, null)

    new (action: System.Action<obj>, obj: obj, ct: CT) = TaskProxy((fun () -> action.Invoke(obj)), ct, TaskStatus.Created, null)

    member this.OnCompleted(cont : unit -> unit) =
        if this.IsCompleted then 
            cont()
        else 
            if this.Status = TaskStatus.Created then this.Start()
            this.ContinueWith(fun (_: Task) -> cont()) |> ignore

    member this.RunContinuations() =
        for c in continuations do
            c.StartContinuation()    

    /// The exception `await` throws for a failed or canceled task: the first inner exception, as in .NET
    member this.AwaitedException() : exn =
        match As<obj> exc with
        | :? System.AggregateException as a when a.InnerExceptions.Count > 0 -> a.InnerExceptions.[0]
        | null when status = TaskStatus.Canceled -> TaskCanceledException() :> exn
        | e -> As<exn> e

    /// The result of a completed task, or throws its exception, used by `await` in C# state machines
    member this.AwaitResult() : obj =
        if status = TaskStatus.RanToCompletion then this?result
        else raise (this.AwaitedException())

    /// Makes tasks awaitable with JavaScript `await`, the callbacks run when the task completes
    [<Name "then">]
    member this.Then(onFulfilled: obj -> unit, onRejected: obj -> unit) =
        this.OnCompleted(fun () ->
            if status = TaskStatus.RanToCompletion then onFulfilled (this?result)
            else onRejected (this.AwaitedException())
        )

    [<Inline "$p.then($onFulfilled, $onRejected)">]
    static member private PromiseThen(p: obj, onFulfilled: obj -> unit, onRejected: obj -> unit) = ()

    /// Only makes the dependency graph include a member that JavaScript calls (`then`)
    [<Inline "void 0">]
    static member private KeepMember(_: obj) = ()

    static member private FromAsyncImpl<'T>(run: unit -> obj, boxed: bool) : Task =
        // native async functions await tasks with `then`
        TaskProxy.KeepMember(fun (t: TaskProxy) -> t.Then(ignore, ignore))
        let task = TaskProxy<'T>(null, CT.None, TaskStatus.Running, null, As<'T> JS.Undefined)
        TaskProxy.PromiseThen(run(),
            (fun r ->
                task?result <- (if boxed then (As<obj[]> r).[0] else r)
                task?status <- TaskStatus.RanToCompletion
                task.RunContinuations()
            ),
            (fun e ->
                task?status <- (match e with :? OCE -> TaskStatus.Canceled | _ -> TaskStatus.Faulted)
                task?exc <- System.AggregateException(As<exn> e)
                task.RunContinuations()
            )
        )
        As<Task> task

    /// The task of a JavaScript async function, used for C# async methods
    static member FromAsync(run: unit -> obj) : Task = TaskProxy.FromAsyncImpl<obj>(run, false)

    /// Same as FromAsync, for results that may be thenable themselves, the async function returns `[result]`
    static member FromAsyncBoxed(run: unit -> obj) : Task = TaskProxy.FromAsyncImpl<obj>(run, true)

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task>) =
        this.ContinueWith(action, CT.None)

    member this.ContinueWith(action: System.Action<Task>, ct) =
        let res = TaskProxy((fun () -> action.Invoke (As<Task> this)), ct, TaskStatus.WaitingForActivation, null)
        if this.IsCompleted then
            res.StartContinuation()     
        else
            continuations.JS.Push res |> ignore
        As<Task> res

    [<Inline>]
    member this.ContinueWith(func: System.Func<Task,'T>) =
        this.ContinueWith(func, CT.None)

    member this.ContinueWith(func: System.Func<Task,'T>, ct) =
        let res = TaskProxy<'T>((fun () -> func.Invoke (As<Task> this)), ct, TaskStatus.WaitingForActivation, null, JS.Undefined)
        if this.IsCompleted then
            res.StartContinuation()     
        else
            continuations.JS.Push res |> ignore
        As<Task<'T>> res

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task, obj>, obj: obj) =
        this.ContinueWith(System.Action<Task>(fun t -> action.Invoke (t, obj)))

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task, obj>, obj: obj, ct) =
        this.ContinueWith(System.Action<Task>(fun t -> action.Invoke (t, obj)), ct)

    [<Inline>]
    member this.ContinueWith(func: System.Func<Task, obj, 'T>, obj: obj) =
        this.ContinueWith(fun t -> func.Invoke (t, obj))

    [<Inline>]
    member this.ContinueWith(func: System.Func<Task, obj, 'T>, obj: obj, ct) =
        this.ContinueWith((fun t -> func.Invoke (t, obj)), ct)

    member this.StartContinuation() =
        if status = TaskStatus.WaitingForActivation then
            status <- TaskStatus.WaitingToRun
            Concurrency.fork (fun () -> 
                if status = TaskStatus.WaitingToRun then
                    status <- TaskStatus.Running
                    try
                        this.Execute()
                        status <- TaskStatus.RanToCompletion
                    with e ->
                        exc <- System.AggregateException(e)
                        status <- TaskStatus.Faulted
                    this.RunContinuations()
            )

    member this.Start() =
        if status = TaskStatus.Created then
            status <- TaskStatus.WaitingToRun
            Concurrency.fork (fun () -> 
                status <- TaskStatus.Running
                try
                    this.Execute()
                    status <- TaskStatus.RanToCompletion
                with
                | :? OCE as e when e.CancellationToken = token ->
                    Console.Log("Task cancellation caught:", e)
                    exc <- System.AggregateException(e)
                    status <- TaskStatus.Canceled
                | e ->
                    Console.Log("Task error caught:", e)
                    exc <- System.AggregateException(e)
                    status <- TaskStatus.Faulted
                this.RunContinuations()
            )
        else
            invalidOp "Task not in initial state"
        
    static member FromCanceled ct = 
        As<Task> (TaskProxy(null, ct, TaskStatus.Canceled, System.AggregateException(TaskCanceledException())))

    static member FromCanceled(ct: CT) = 
        As<Task<_>> (TaskProxy<_>(null, ct, TaskStatus.Canceled, System.AggregateException(TaskCanceledException()), As null)) 

    static member FromException (exc: exn) =
        As<Task> (TaskProxy(null, CT.None, TaskStatus.Faulted, System.AggregateException(exc)))

    static member FromException (exc: exn) =
        As<Task<_>> (TaskProxy<_>(null, CT.None, TaskStatus.Faulted, System.AggregateException(exc), As null))

    static member FromResult (res: 'T) = 
        As<Task<'T>> (TaskProxy<'T>(null, CT.None, TaskStatus.RanToCompletion, null, res)) 

    static member CompletedTask =
        As<Task> (TaskProxy(null, CT.None, TaskStatus.RanToCompletion, null))

    [<Inline>]
    static member Run(action : System.Action) =
       TaskProxy.Run(action, CT.None)
        
    static member Run(action : System.Action, ct) =
        let res = TaskProxy(action, ct, TaskStatus.Created, null)
        res.Start()
        As<Task> res

    [<Inline>]
    static member Run(func : System.Func<Task>) =
        TaskProxy.Run(func, CT.None)

    static member Run(func : System.Func<Task>, ct: CT) =
        let task = func.Invoke()
        if ct.IsCancellationRequested then TaskProxy.FromCanceled ct : Task else
        if task.Status = TaskStatus.Created then
            task.Start()
        task

    [<Inline>]
    static member Run(func : System.Func<'T>) =
        TaskProxy.Run(func, CT.None)

    static member Run(func : System.Func<'T>, ct) =
        let res = TaskProxy<'T>(func, ct, TaskStatus.Created, null, JS.Undefined)
        res.Start()
        As<Task<'T>> res   

    [<Inline>]
    static member Run(func : System.Func<Task<'T>>) =
        TaskProxy.Run(func, CT.None)

    static member Run(func : System.Func<Task<'T>>, ct: CT) =
        let task = func.Invoke()
        if ct.IsCancellationRequested then TaskProxy.FromCanceled<'T> ct else
        if task.Status = TaskStatus.Created then
            task.Start()
        task

    static member Delay(time: int) =   
        Async.StartAsTask (Async.Sleep time) :> Task
             
    static member Delay(time: int, ct) =        
        Async.StartAsTask (Async.Sleep time, cancellationToken = ct) :> Task

    [<Inline>]
    static member Delay(time: System.TimeSpan) =        
        TaskProxy.Delay(As<int> time)
             
    [<Inline>]
    static member Delay(time: System.TimeSpan, ct) =        
        TaskProxy.Delay(As<int> time, ct)

    static member WhenAny(tasks: Task[]) =
        let tcs = System.Threading.Tasks.TaskCompletionSource<_>()
        for t in tasks do t.ContinueWith (fun t -> tcs.TrySetResult t |> ignore) |> ignore
        tcs.Task
            
    [<Inline>]                         
    static member WhenAny(tasks: seq<Task>) = TaskProxy.WhenAny(Array.ofSeq tasks)

    static member WhenAny(tasks: Task<'T>[]) =
        let tcs = System.Threading.Tasks.TaskCompletionSource<Task<'T>>()
        for t in tasks do t.ContinueWith (fun t -> tcs.TrySetResult t |> ignore) |> ignore
        tcs.Task
            
    [<Inline>]                         
    static member WhenAny(tasks: seq<Task<'T>>) = TaskProxy.WhenAny(Array.ofSeq tasks)

    static member WhenAll(tasks: Task[]) =
        let target = tasks.Length
        let completed = ref 0
        let tcs = System.Threading.Tasks.TaskCompletionSource<_>()
        for i = 0 to target - 1 do
            tasks.[i].ContinueWith (fun t -> 
                if t.IsFaulted then
                    tcs.TrySetException t.Exception |> ignore
                elif t.IsCanceled then
                    tcs.TrySetCanceled() |> ignore
                else
                    incr completed
                    if !completed = target then tcs.TrySetResult() |> ignore 
            ) |> ignore
        tcs.Task :> Task

    [<Inline>]                         
    static member WhenAll(tasks: seq<Task>) = TaskProxy.WhenAll(Array.ofSeq tasks)

    static member WhenAll(tasks: Task<'T>[]) =
        let target = tasks.Length
        let completed = ref 0
        let results = JavaScript.Array(target)
        let tcs = System.Threading.Tasks.TaskCompletionSource<_>()
        for i = 0 to target - 1 do
            tasks.[i].ContinueWith (fun (t: Task<'T>) -> 
                if t.IsFaulted then
                    tcs.TrySetException t.Exception |> ignore
                elif t.IsCanceled then
                    tcs.TrySetCanceled() |> ignore
                else
                    incr completed
                    results.[i] <- t.Result
                    if !completed = target then tcs.SetResult results.Self
            ) |> ignore
        tcs.Task

    [<Inline>]                         
    static member WhenAll(tasks: seq<Task<'T>>) = TaskProxy.WhenAll(Array.ofSeq tasks)

    static member Yield() =
        new Task(fun () -> ()) |> As<System.Runtime.CompilerServices.YieldAwaitable>  

and [<Proxy(typeof<Task<_>>); Name "Task1">] private TaskProxy<'T>(func: System.Func<'T>, token: CT, status, exc, result) =
    inherit TaskProxy(null, token, status, exc)
    
    [<Name "result">]
    let mutable result = result

    new (func) = TaskProxy<'T>(func, CT.None, TaskStatus.Created, null, As<'T> JS.Undefined)

    new (func, ct) = TaskProxy<'T>(func, ct, TaskStatus.Created, null, As<'T> JS.Undefined)

    new (func: System.Func<obj, 'T>, obj: obj) = TaskProxy<'T>((fun () -> func.Invoke obj), CT.None, TaskStatus.Created, null, As<'T> JS.Undefined)

    new (func: System.Func<obj, 'T>, obj: obj, ct: CT) = TaskProxy<'T>((fun () -> func.Invoke obj), ct, TaskStatus.Created, null, As<'T> JS.Undefined)

    member this.Result = 
        match this.Status with
        | TaskStatus.RanToCompletion -> result
        | TaskStatus.Faulted
        | TaskStatus.Canceled -> raise this.Exception 
        | _ -> invalidOp "Task has not been completed, has no Result"

    override this.Execute () =
        result <- func.Invoke()

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task<'T>>) =
        this.ContinueWith(As<System.Action<Task>> action)

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task<'T>>, ct) =
        this.ContinueWith(As<System.Action<Task>> action, ct)

    [<Inline>]
    member this.ContinueWith<'R>(func: System.Func<Task<'T>, 'R>) =
        this.ContinueWith(As<System.Func<Task, 'R>> func) 

    [<Inline>]
    member this.ContinueWith<'R>(func: System.Func<Task<'T>, 'R>, ct) =
        this.ContinueWith(As<System.Func<Task, 'R>> func, ct) 

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task<'T>, obj>, obj: obj) =
        this.ContinueWith(System.Action<Task<'T>>(fun t -> action.Invoke(t, obj)))

    [<Inline>]
    member this.ContinueWith(action: System.Action<Task<'T>, obj>, obj: obj, ct) =
        this.ContinueWith(System.Action<Task<'T>>(fun t -> action.Invoke(t, obj)), ct)

    [<Inline>]
    member this.ContinueWith<'R>(func: System.Func<Task<'T>, obj, 'R>, obj: obj) =
        this.ContinueWith(fun t -> func.Invoke(t, obj)) 

    [<Inline>]
    member this.ContinueWith<'R>(func: System.Func<Task<'T>, obj, 'R>, obj: obj, ct) =
        this.ContinueWith((fun t -> func.Invoke(t, obj)), ct) 

[<Proxy(typeof<TaskCompletionSource<_>>)>]
[<Name "TaskCompletionSource">]
type private TaskCompletionSourceProxy<'T>() =
    let task = new TaskProxy<'T>(null, CT.None, TaskStatus.WaitingForActivation, null, JS.Undefined)

    member this.Task = As<Task<'T>> task

    member this.SetCanceled() =
        if task.IsCompleted then
            failwith "Task already completed."
        task?status <- TaskStatus.Canceled
        task.RunContinuations()

    member this.SetException(exc: exn) =
        if task.IsCompleted then
            failwith "Task already completed."
        task?status <- TaskStatus.Faulted
        task?exc <- System.AggregateException(exc)
        task.RunContinuations()

    member this.SetException(exs : seq<exn>) =
        this.SetException(System.AggregateException(exs))

    member this.SetResult(res: 'T) =
        if task.IsCompleted then
            failwith "Task already completed."
        task?status <- TaskStatus.RanToCompletion
        task?result <- res 
        task.RunContinuations()

    member this.TrySetCanceled() =
        if not task.IsCompleted then
            task?status <- TaskStatus.Canceled
            task.RunContinuations()
            true
        else false

    member this.TrySetCanceled(ct: CT) =
        if not task.IsCompleted then
            task?status <- TaskStatus.Canceled
            task?token <- ct
            task.RunContinuations()
            true
        else false

    member this.TrySetException(exc: exn) =
        if not task.IsCompleted then
            task?status <- TaskStatus.Faulted
            task?exc <- System.AggregateException(exc)
            task.RunContinuations()
            true
        else false

    member this.TrySetException(exs : seq<exn>) =
        this.TrySetException(System.AggregateException(exs))

    member this.TrySetResult(res: 'T) =        
        if not task.IsCompleted then
            task?status <- TaskStatus.RanToCompletion
            task?result <- res 
            task.RunContinuations()
            true
        else false

[<Proxy(typeof<ValueTask>); Prototype false>]
type private ValueTaskProxy =

    [<Inline "$task">]
    new (task: Task) = {}

    [<Inline "$type('System.Threading.Tasks.Task').FromResult(null)">]
    new () = {}

    [<Inline "$type('System.Threading.Tasks.Task').FromResult(null)">]
    static member CompletedTask = X<ValueTask>

    [<Inline "$this">]
    member this.AsTask() = X<Task>

    [<Inline "$this">]
    member this.Preserve() = X<ValueTask>

    [<Inline "$this.status === 5">]
    member this.IsCompletedSuccessfully = X<bool>

    [<Inline "$this.status === 5 || $this.status === 7 || $this.status === 6">]
    member this.IsCompleted = X<bool>

    [<Inline "$this.status === 7">]
    member this.IsFaulted = X<bool>

    [<Inline "$this.status === 6">]
    member this.IsCanceled = X<bool>

[<Proxy(typeof<ValueTask<_>>); Prototype false>]
type private ValueTaskProxy<'T> =

    [<Inline "$task">]
    new (task: Task<'T>) = {}

    [<Inline "$type('System.Threading.Tasks.Task').FromResult($result)">]
    new (result: 'T) = {}

    [<Inline "$type('System.Threading.Tasks.Task').FromResult(null)">]
    new () = {}

    [<Inline "$this">]
    member this.AsTask() = X<Task<'T>>

    [<Inline "$this">]
    member this.Preserve() = X<ValueTask<'T>>

    [<Inline>]
    member this.Result = (As<Task<'T>> this).Result

    [<Inline "$this.status === 5">]
    member this.IsCompletedSuccessfully = X<bool>

    [<Inline "$this.status === 5 || $this.status === 7 || $this.status === 6">]
    member this.IsCompleted = X<bool>

    [<Inline "$this.status === 7">]
    member this.IsFaulted = X<bool>

    [<Inline "$this.status === 6">]
    member this.IsCanceled = X<bool>
