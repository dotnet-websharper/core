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

open WebSharper.JavaScript

open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices
open System

#nowarn "3501" // Invalid resumable code
#nowarn "1204" // This construct is for use by compiled F# code

[<Proxy(typeof<TaskBuilderBase>); Name "TaskBuilderBase">]
type internal TaskBuilderBaseProxy() =
    
    [<Inline>]
    member x.Delay<'TOverall, 'T>(generator: unit -> ResumableCode<TaskStateMachineData<'TOverall>, 'T>) = 
        generator
        |> As<unit -> Async<'T>>
        |> async.Delay
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>>

    [<Inline>]
    member x.Zero<'TOverall>() =
        async.Zero()
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, unit>>

    [<Inline>]
    member x.Return<'T>(value: 'T) =
        async.Return(value)
        |> As<ResumableCode<TaskStateMachineData<'T>, 'T>>

    [<Inline>]
    member x.Combine<'TOverall, 'T>(task1: ResumableCode<TaskStateMachineData<'TOverall>, unit>, task2: ResumableCode<TaskStateMachineData<'TOverall>, 'T>) =
        async.Combine(As<Async<unit>> task1, As<Async<'T>> task2)
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>>

    [<Inline>]
    member x.While<'TOverall>(condition: unit -> bool, body: ResumableCode<TaskStateMachineData<'TOverall>, unit>) =
        async.While(condition, As<Async<unit>> body)
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, unit>>    

    [<Inline>]
    member x.TryWith<'TOverall, 'T>(body: ResumableCode<TaskStateMachineData<'TOverall>, 'T>, catch: exn -> ResumableCode<TaskStateMachineData<'TOverall>, 'T>) =
        async.TryWith(As<Async<'T>> body, As<exn -> Async<'T>> catch)
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>>   

    [<Inline>]
    member x.TryFinally<'TOverall, 'T>(body: ResumableCode<TaskStateMachineData<'TOverall>, 'T> , compensation: unit -> unit) =
        async.TryFinally(As<Async<'T>> body, compensation)
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>>   

    [<Inline>]
    member x.For<'T, 'TOverall>(sequence: seq<'T>, body: 'T -> ResumableCode<TaskStateMachineData<'TOverall>, unit>) =
        async.For(sequence, As<'T -> Async<unit>> body)
        |> As<ResumableCode<TaskStateMachineData<'TOverall>, unit>>

    //[<Inline>]
    //member x.Using<'TResource, 'TOverall, 'T when 'TResource :> IAsyncDisposable>(resource: 'TResource, body: 'TResource -> ResumableCode<TaskStateMachineData<'TOverall>, 'T>) =
    //    0
    //    |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>>   

[<Proxy(typeof<TaskBuilder>); Name "TaskBuilder">]
type private TaskBuilderProxy() =
    inherit TaskBuilderBaseProxy()

    [<Inline>]
    member x.Run<'T> (code: ResumableCode<TaskStateMachineData<'T>, 'T>) =
        code
        |> As<Async<'T>>
        |> Async.StartAsTask
        //|> As<Task<'T>>

[<Name "TaskBuilderModule">]
[<Proxy "Microsoft.FSharp.Control.TaskBuilderModule, FSharp.Core">]
module internal TaskBuilderModuleProxy =
    [<Inline>]
    let task = () |> As<TaskBuilder>

[<Name "TaskBuilderExtensions">]
[<Proxy "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority, FSharp.Core">]
module internal TaskBuilderExtensionsHighPriorityProxy =
    [<Inline>]
    let ``TaskBuilderBase.Bind``<'TResult1, 'TOverall, 'TResult2> 
        (_: TaskBuilderBase, task: Task<'TResult1>, continuation: 'TResult1 -> ResumableCode<TaskStateMachineData<'TOverall>, 'TResult2>) =
            async.Bind(Async.AwaitTask task, As<'TResult1 -> Async<'TResult2>> continuation)
            |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'TResult2>> 

    [<Inline>]
    let ``TaskBuilderBase.ReturnFrom``<'T>(this: TaskBuilderBase, task: Task<'T>) =
        Async.AwaitTask task
        |> As<ResumableCode<TaskStateMachineData<'T>, 'T>>   

[<Name "TaskBuilderExtensions">]
[<Proxy "Microsoft.FSharp.Control.TaskBuilderExtensions.MediumPriority, FSharp.Core">]
module internal TaskBuilderExtensionsMediumPriorityProxy =
    [<Inline>]
    let ``TaskBuilderBase.Bind``<'TResult1, 'TOverall, 'TResult2> 
        (_: TaskBuilderBase, computation: Async<'TResult1>, continuation: 'TResult1 -> ResumableCode<TaskStateMachineData<'TOverall>, 'TResult2>) =
            async.Bind(computation, As<'TResult1 -> Async<'TResult2>> continuation)
            |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'TResult2>> 

    [<Inline>]
    let ``TaskBuilderBase.ReturnFrom``<'T>(this: TaskBuilderBase, computation: Async<'T>) =
        computation
        |> As<ResumableCode<TaskStateMachineData<'T>, 'T>>   

[<Name "TaskBuilderExtensions">]
[<Proxy "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority, FSharp.Core">]
module internal TaskBuilderExtensionsLowPriorityProxy =
    [<Inline>]
    let ``TaskBuilderBase.Using``<'TResource, 'TOverall, 'T when 'TResource :> IDisposable> 
        (_: TaskBuilderBase, resource: 'TResource, body: 'TResource -> ResumableCode<TaskStateMachineData<'TOverall>, 'T>) =
            async.Using(resource, As<'TResource -> Async<'T>> body)
            |> As<ResumableCode<TaskStateMachineData<'TOverall>, 'T>> 
