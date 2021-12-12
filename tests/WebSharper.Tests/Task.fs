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

module WebSharper.Tests.Task

open System.Threading.Tasks

open WebSharper
open WebSharper.Testing

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
    }
