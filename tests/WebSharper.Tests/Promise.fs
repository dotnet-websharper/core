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

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
exception private MyCustomException of int

[<JavaScript>]
let Tests =
    TestCategory "Promise" {

        Test "AsAsync Resolve" {
            let! res =
                Promise(fun (resolve, _) ->
                    resolve 42)
                |> Promise.AsAsync
            equalMsg res 42 "immediate"

            let! res =
                Promise(fun (resolve, _) ->
                    JS.SetTimeout (fun () -> resolve 42) 1000 |> ignore)
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
                        JS.SetTimeout (fun () -> reject (MyCustomException 42)) 1000 |> ignore)
                    |> Promise.AsAsync
                with MyCustomException e -> return e
            }
            equalMsg res 42 "delayed exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject 42) 1000 |> ignore)
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
                    JS.SetTimeout (fun () -> resolve 42) 1000 |> ignore)
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
                with MyCustomException e -> return e
            }
            equalMsg res 42 "immediate exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        reject 42)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with :? NonStandardPromiseRejectionException as e -> return e.Reason :?> int
            }
            equalMsg res 42 "immediate non-exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject (MyCustomException 42)) 1000 |> ignore)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with MyCustomException e -> return e
            }
            equalMsg res 42 "delayed exn"

            let! res = async {
                try return! Promise(fun (_, reject) ->
                        JS.SetTimeout (fun () -> reject 42) 1000 |> ignore)
                    |> Promise.AsTask
                    |> Async.AwaitTask
                with :? NonStandardPromiseRejectionException as e -> return e.Reason :?> int
            }
            equalMsg res 42 "delayed non-exn"
        }

    }
