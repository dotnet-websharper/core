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
    
    member this.OnCompleted(cont : unit -> unit) =
        if this.IsCompleted then 
            cont()
        else 
            if this.Status = TaskStatus.Created then this.Start()
            this.ContinueWith(fun (_: Task) -> cont()) |> ignore

    member this.RunContinuations() =
        for c in continuations do
            c.StartContinuation()    

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
        let res = TaskProxy<'T>((fun () -> func.Invoke (As<Task> this)), ct, TaskStatus.WaitingForActivation, null, Unchecked.defaultof<'T>)
        if this.IsCompleted then
            res.StartContinuation()     
        else
            continuations.JS.Push res |> ignore
        As<Task<'T>> res

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
                with e ->
                    exc <- System.AggregateException(e)
                    status <- TaskStatus.Faulted
                this.RunContinuations()
            )
        else
            invalidOp "Task not in initial state"
        
    static member FromCanceled ct = 
        As<Task> (TaskProxy(null, ct, TaskStatus.Canceled, null)) 

    static member FromException exc =
        As<Task> (TaskProxy(null, CT.None, TaskStatus.Faulted, exc))

    static member FromResult (res: 'T) = 
        As<Task<'T>> (TaskProxy<'T>(null, CT.None, TaskStatus.RanToCompletion, null, res)) 

    [<Inline>]
    static member Run(action : System.Action) =
       TaskProxy.Run(action, CT.None)      
        
    static member Run(action : System.Action, ct) =
        let res = TaskProxy(action, ct, TaskStatus.Created, null)
        res.Start()
        As<Task> res

    [<Inline>]
    static member Run(func : System.Func<'T>) =
        TaskProxy.Run(func, CT.None)

    static member Run(func : System.Func<'T>, ct) =
        let res = TaskProxy<'T>(func, ct, TaskStatus.Created, null, Unchecked.defaultof<'T>)
        res.Start()
        As<Task<'T>> res

//    // TODO : return type System.Runtime.CompilerServices.YieldAwaitable 
//    static member Yield() =
//        Async.Sleep 0 |> Async.StartAsTask  

and [<Proxy(typeof<Task<_>>); Name "Task1">] private TaskProxy<'T>(func: System.Func<'T>, token: CT, status, exc, result) =
    inherit TaskProxy(null, token, status, exc)
    
    [<Name "result">]
    let mutable result = result

    member this.Result = result

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

[<Proxy(typeof<TaskCompletionSource<_>>)>]
type private TaskCompletionSourceProxy<'T>() =
    let task = new TaskProxy<'T>(null, CT.None, TaskStatus.WaitingForActivation, null, Unchecked.defaultof<'T>)

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

    // TODO : Try... methods



    