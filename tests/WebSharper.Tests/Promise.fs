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
module private Async =

    exception NonStandardPromiseReject of obj

    let AwaitPromise (p: Promise<'T>) : Async<'T> =
        Async.FromContinuations <| fun (ok, ko, _) ->
            p.Then(ok, function
                | :? exn as e -> ko e
                | e -> ko (NonStandardPromiseReject e)
            )

[<JavaScript>]
let Tests =
    TestCategory "Promise" {

        Test "Then" {
            let p = new Promise(fun (resolve, reject) -> resolve 42)
            let! res = Async.AwaitPromise p
            equal res 42
        }

    }
