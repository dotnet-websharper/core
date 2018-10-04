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
    }
