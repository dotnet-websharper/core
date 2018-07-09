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

module WebSharper.Tests.Promise

open System.Threading.Tasks
open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
exception private MyCustomException of int

[<JavaScript>]
let rec private (|CheckInner|) (e: exn) =
    match e with
    | :? System.AggregateException as e -> (|CheckInner|) e.InnerExceptions.[0]
    | e -> e

[<JavaScript>]
let Tests =
    let mutable x = 0
    TestCategory "Promise" {

        Test "AsAsync Resolve" {
            let! res =
                Promise(fun (resolve, _) ->
                    resolve 42)
                |> Promise.AsAsync
            equalMsg res 42 "immediate"

            let! res =
                Promise(fun (resolve, _) ->
                    JS.SetTimeout (fun () -> resolve 42) 100 |> ignore)
                |> Promise.AsAsync
            equalMsg res 42 "delayed"
        }

        Test "AsAsync Reject" {
            let! res = async {
                try return! Promise(fun (_, reject) ->
                        reject (MyCustomException 42))
                    |> Promise.AsAsync
                with MyCustomException e -> return e
            }
            equalMsg res 42 "immediate exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        reject 42)
                    |> Promise.AsAsync
                with :? NonStandardPromiseRejectionException as e -> return e.Reason :?> int
            }
            equalMsg res 42 "immediate non-exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject (MyCustomException 42)) 100 |> ignore)
                    |> Promise.AsAsync
                with MyCustomException e -> return e
            }
            equalMsg res 42 "delayed exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject 42) 100 |> ignore)
                    |> Promise.AsAsync
                with :? NonStandardPromiseRejectionException as e -> return e.Reason :?> int
            }
            equalMsg res 42 "delayed non-exn"
        }

        Test "AsTask Resolve" {
            let! res =
                Promise(fun (resolve, _) ->
                    resolve 42)
                |> Promise.AsTask
                |> Async.AwaitTask
            equalMsg res 42 "immediate"

            let! res =
                Promise(fun (resolve, _) ->
                    JS.SetTimeout (fun () -> resolve 42) 100 |> ignore)
                |> Promise.AsTask
                |> Async.AwaitTask
            equalMsg res 42 "delayed"
        }

        Test "AsTask Reject" {
            let! res = async {
                try return! Promise(fun (_, reject) ->
                        reject (MyCustomException 42))
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with CheckInner (MyCustomException e) -> return e
            }
            equalMsg res 42 "immediate exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        reject 42)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with CheckInner (:? NonStandardPromiseRejectionException as e) -> return e.Reason :?> int
            }
            equalMsg res 42 "immediate non-exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject (MyCustomException 42)) 100 |> ignore)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with
                | CheckInner (MyCustomException e) -> return e
            }
            equalMsg res 42 "delayed exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject 42) 100 |> ignore)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with CheckInner (:? NonStandardPromiseRejectionException as e) -> return e.Reason :?> int
            }
            equalMsg res 42 "delayed non-exn"
        }

        Test "Computation Expression" {
            let! res = promise {
                return 42
            }
            equalMsg res 42 "return"

            let! res = promise {
                let! x = Promise.Resolve(42)
                return x
            }
            equalMsg res 42 "bind promise"

            let! res = promise {
                let! x = async { return 42 }
                return x
            }
            equalMsg res 42 "bind async"

            let! res = promise {
                let! x = Task.FromResult(42)
                return x
            }
            equalMsg res 42 "bind task"

            let! res = promise {
                return! Promise.Resolve(42)
            }
            equalMsg res 42 "returnFrom promise"

            let! res = promise {
                return! async { return 42 }
            }
            equalMsg res 42 "returnFrom async"

            let! res = promise {
                return! Task.FromResult(42)
            }
            equalMsg res 42 "returnFrom task"

            let! res = promise {
                ()
            }
            equalMsg res () "zero"

            let! res = promise {
                try
                    return! Promise.Reject(12)
                with :? NonStandardPromiseRejectionException as e
                        when e.Reason :?> int = 12 ->
                    return 42
            }
            equalMsg res 42 "tryWith"

            x <- 0
            let! res = promise {
                try
                    return! Promise.Resolve(30)
                finally
                    x <- 12
            }
            equalMsg (res + x) 42 "tryFinally success"

            x <- 0
            let p = promise {
                try
                    return! Promise.Reject(30)
                finally
                    x <- 12
            }
            let! res = p.Catch(fun e -> e :?> int)
            equalMsg (res + x) 42 "tryFinally failure"
        }

    }
