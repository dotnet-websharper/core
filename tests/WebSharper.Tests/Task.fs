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

module WebSharper.Tests.Task

open System.Threading.Tasks

open WebSharper
open WebSharper.Testing

[<JavaScript>]
type TestAsyncEnumerator(n: int) =
    let mutable i = 0

    interface System.Collections.Generic.IAsyncEnumerator<int> with
        member this.Current = i

        member this.MoveNextAsync() =
            i <- i + 1
            System.Threading.Tasks.ValueTask<bool>(i <= n)

        member this.DisposeAsync() =
            System.Threading.Tasks.ValueTask.CompletedTask

[<JavaScript>]
type TestAsyncEnumerable(n: int) =
    interface System.Collections.Generic.IAsyncEnumerable<int> with
        member this.GetAsyncEnumerator(ct: System.Threading.CancellationToken) =
            upcast TestAsyncEnumerator(n)

[<JavaScript>]
let Tests =
    TestCategory "Task" {
        Test "Run" {
            let taskDone = ref false  
            let task = Task.Run(fun () -> taskDone := true)
            do! Async.AwaitTask task
            isTrue task.IsCompleted
            isFalse task.IsCanceled
            isFalse task.IsFaulted
            isTrue !taskDone

            let task2 = Task.Run(fun () -> 1 + 2)
            let! task2res = Async.AwaitTask task2
            equal task2res 3
        }

        Test "Failed" {
            let task = Task.Run(fun () -> failwith<unit> "error")
            do! async { 
                try do! Async.AwaitTask task
                with _ -> ()
            }
            isTrue task.IsCompleted
            isTrue task.IsFaulted
            isFalse task.IsCanceled
            equal task.Exception.InnerExceptions.[0].Message "error"
        }

        Test "StartAsTask" {
            let task = Async.StartAsTask (async {return 2 })
            let! taskRes = Async.AwaitTask task
            equal taskRes 2

            let task2 = Async.StartAsTask (async { failwith "error" })
            let! task2Error =
                async {
                    try
                        let! _ = Async.AwaitTask task2
                        return Some "awaiting failing task should fail"
                    with 
                    | (:? System.AggregateException as e) ->                        
                        if (e).InnerExceptions.[0].Message = "error" then
                            return None
                        else return Some "wrong error propagated"
                    | _ -> return Some "error is not AggregateException"
                }
            equal task2Error None    
        }

        Test "ContinueWith" {
            let state = ref ""
            let task = new Task(fun () -> state := "A")
            equal !state ""
            equal task.Status TaskStatus.Created
            let task2 = task.ContinueWith(fun t -> if not t.IsFaulted then state := !state + "B")
            equal task2.Status TaskStatus.WaitingForActivation
            task.Start()
            do! Async.AwaitTask task2
            isTrue task2.IsCompleted
            equal !state "AB"
        }

        Test "Task computation expression" {
            let task1 = task { return 1 }
            let! task1Res = Async.AwaitTask task1
            equal task1Res 1
            let task2 = task { return! task1 }
            let! task2Res = Async.AwaitTask task2
            equal task2Res 1
            let task3 = task { return 2 }
            let task4 =
                task {
                    let! r1 = task1
                    let! r3 = task3
                    return r1 + r3
                }
            let! task4Res = Async.AwaitTask task4
            equal task4Res 3
        }

        Test "Task expressions with loops and exceptions" {
            let t =
                task {
                    let sum = ref 0
                    let i = ref 0
                    while i.Value < 3 do
                        let! x = Task.FromResult i.Value
                        sum.Value <- sum.Value + x
                        i.Value <- i.Value + 1
                    for j in [ 10; 20 ] do
                        let! y = Task.FromResult j
                        sum.Value <- sum.Value + y
                    try
                        failwith "err"
                    with
                    | e -> sum.Value <- sum.Value + e.Message.Length
                    return sum.Value
                }
            let! r = Async.AwaitTask t
            equal r 36
            let failing =
                task {
                    let! x = Task.FromResult 1
                    if x = 1 then failwith "boom"
                    return x
                }
            let! res = Async.Catch (Async.AwaitTask failing)
            isTrue (match res with Choice2Of2 _ -> true | _ -> false)
            isTrue failing.IsFaulted
            let nested = task { return Task.FromResult 5 }
            let! inner = Async.AwaitTask nested
            let! v = Async.AwaitTask inner
            equal v 5
        }

        Test "Async interop" {
            let async1 = async { return 1 }
            let task1 = task { return! async1 }
            let task2 =
                task {
                    let! r1 = async1
                    let! r2 = task1
                    return r1 + r2
                }
            let! task2Res = Async.AwaitTask task2
            equal task2Res 2
        }

        Test "MergeSources" {
            let taskTask =
                task {
                    let! r1 = Task.FromResult 1
                    and! r2 = Task.FromResult 2
                    return r1 + r2
                }
            let! taskTaskRes = Async.AwaitTask taskTask
            equal taskTaskRes 3

            let taskAsync =
                task {
                    let! r1 = Task.FromResult 3
                    and! r2 = async { return 4 }
                    return r1 + r2
                }
            let! taskAsyncRes = Async.AwaitTask taskAsync
            equal taskAsyncRes 7

            let asyncTask =
                task {
                    let! r1 = async { return 5 }
                    and! r2 = Task.FromResult 6
                    return r1 + r2
                }
            let! asyncTaskRes = Async.AwaitTask asyncTask
            equal asyncTaskRes 11

            let asyncAsync =
                task {
                    let! r1 = async { return 7 }
                    and! r2 = async { return 8 }
                    return r1 + r2
                }
            let! asyncAsyncRes = Async.AwaitTask asyncAsync
            equal asyncAsyncRes 15
        }

        Test "For" {
            let x = task {
                let l = ref []
                for i in 1 .. 3 do 
                    l := i :: !l
                return  !l
            }
            let! xRes = Async.AwaitTask x
            equal xRes [ 3; 2; 1 ]
        }

        Test "TryWith" {
            let ops = ref ""
            let x = task {
                try
                    ops := !ops + "A"
                    failwith "error"
                    ops := !ops + "A"
                with _ ->
                    ops := !ops + "B"
                return !ops
            }
            let! xRes = Async.AwaitTask x
            equal xRes "AB"
        }

        Test "TryFinally" {
            let ops = ref ""
            let x = task {
                try
                    try
                        ops := !ops + "A"
                        failwith "error"
                        ops := !ops + "A"
                    finally ops := !ops + "F"
                with _ -> ()
                return !ops
            }
            let! xRes = Async.AwaitTask x
            equal xRes "AF"
        }

        Test "Using" {
            let res = ref 0
            let d =
                { new System.IDisposable with
                    member x.Dispose() =
                        incr res
                }
            let x = 
                task {
                    use dd = d
                    return !res
                }
            let! xRes = Async.AwaitTask x
            equal xRes 0
            equal !res 1
        }

        Test "UsingAsync" {
            let res = ref 0
            let d =
                { new System.IAsyncDisposable with
                    member x.DisposeAsync() =
                        let disposeTask =
                            task {
                                do! Async.Sleep 1
                                incr res
                            }
                        System.Threading.Tasks.ValueTask(disposeTask :> Task)
                }
            let x =
                task {
                    use! dd = task { return d }
                    ignore dd
                    return !res
                }
            let! xRes = Async.AwaitTask x
            equal xRes 0
            equal !res 1
        }

        Test "UsingAsyncManual" {
            let res = ref 0
            let d =
                { new System.IAsyncDisposable with
                    member x.DisposeAsync() =
                        incr res
                        System.Threading.Tasks.ValueTask.CompletedTask
                }
            let x = 
                task {
                    let dd = d
                    let before = !res
                    do! Async.AwaitTask (dd.DisposeAsync().AsTask())
                    return before
                }
            let! xRes = Async.AwaitTask x
            equal xRes 0
            equal !res 1
        }

        Test "IAsyncEnumerable" {
            let seq = TestAsyncEnumerable(3)
            let x =
                task {
                    let e = (seq :> System.Collections.Generic.IAsyncEnumerable<int>).GetAsyncEnumerator(System.Threading.CancellationToken())
                    let l = ref []
                    let rec loop() = task {
                        let! has = Async.AwaitTask (e.MoveNextAsync().AsTask())
                        if has then
                            l := e.Current :: !l
                            return! loop()
                        else
                            return ()
                    }
                    do! loop()
                    do! Async.AwaitTask (e.DisposeAsync().AsTask())
                    return List.rev !l
                }
            let! res = Async.AwaitTask x
            equal res [ 1; 2; 3 ]
        }
    }
