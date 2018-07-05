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

namespace WebSharper.JavaScript

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open WebSharper

[<JavaScript>]
type NonStandardPromiseRejectionException(reason: obj) =
    inherit Exception("Promise rejected")

    member this.Reason = reason

[<JavaScript>]
module Promise =

    let OfAsync (a: Async<'T>) : Promise<'T> =
        new Promise<'T>(fun (resolve, reject) ->
            Async.StartWithContinuations(a, resolve, reject, reject)
        )

    let OfTask (t: Task<'T>) : Promise<'T> =
        new Promise<'T>(fun (resolve, reject) ->
            t.ContinueWith(fun (t: Task<'T>) ->
                if t.IsCanceled then
                    reject (TaskCanceledException())
                elif t.IsFaulted then
                    reject t.Exception
                else // RanToCompletion
                    resolve t.Result
            )
            |> ignore
        )

    let AsAsync (p: Promise<'T>) : Async<'T> =
        Async.FromContinuations(fun (ok, ko, _) ->
            p.Then(ok, fun (err: obj) ->
                match err with
                | :? exn as e -> ko e
                | e -> ko (NonStandardPromiseRejectionException e)
            )
            |> ignore
        )

    let AsTask (p: Promise<'T>) : Task<'T> =
        let tcs = System.Threading.Tasks.TaskCompletionSource<'T>()
        p.Then(tcs.SetResult, fun (err: obj) ->
            match err with
            | :? exn as e -> tcs.SetException e
            | e -> tcs.SetException (NonStandardPromiseRejectionException e)
        )
        |> ignore
        tcs.Task

    type Builder [<Inline "null">] internal () =

        member this.Bind(p: Promise<'T>, f: 'T -> Promise<'U>) : Promise<'U> =
            p.Then<'U>(f)

        member this.Return(x: 'T) : Promise<'T> =
            Promise<'T>.Resolve(x)

    let Do = Builder()

[<Extension; JavaScript>]
type PromiseExtensions =

    [<Extension; Inline>]
    static member AsAsync this = Promise.AsAsync this

    [<Extension; Inline>]
    static member AsTask this = Promise.AsTask this

    [<Extension; Inline>]
    static member AsPromise this = Promise.OfAsync this

    [<Extension; Inline>]
    static member AsPromise this = Promise.OfTask this
