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

    let private unwrapExn (x: obj) : exn =
        match x with
        | :? exn as e -> e
        | x -> NonStandardPromiseRejectionException x :> exn

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
                ko (unwrapExn err)
            )
            |> ignore
        )

    let AsTask (p: Promise<'T>) : Task<'T> =
        let tcs = System.Threading.Tasks.TaskCompletionSource<'T>()
        p.Then(tcs.SetResult, fun (err: obj) ->
            tcs.SetException(unwrapExn err)
        )
        |> ignore
        tcs.Task

    let private For (xs: seq<'T>) (f: 'T -> Promise<unit>) : Promise<unit> =
        let e = xs.GetEnumerator()
        let rec run() : Promise<unit> =
            if e.MoveNext() then
               (f e.Current).Then<unit>(run)
            else
                Promise.Resolve(())
        // Call run() in a Promise rather than immediately,
        // or .Finally wouldn't catch an exception when enumerating the first item.
        Promise<unit>(fun (resolve, reject) -> resolve (unbox<unit>(run())))
            .Finally(fun () -> e.Dispose())

    type Builder [<Inline "null">] internal () =

        [<Inline>]
        member this.Bind(p: Promise<'T>, f: 'T -> Promise<'U>) : Promise<'U> =
            p.Then<'U>(f)

        [<Inline>]
        member this.Bind(a: Async<'T>, f: 'T -> Promise<'U>) : Promise<'U> =
            (OfAsync a).Then<'U>(f)

        [<Inline>]
        member this.Bind(a: Task<'T>, f: 'T -> Promise<'U>) : Promise<'U> =
            (OfTask a).Then<'U>(f)

        [<Inline>]
        member this.Return(x: 'T) : Promise<'T> =
            Promise<'T>.Resolve(x)

        [<Inline>]
        member this.ReturnFrom(x: Promise<'T>) : Promise<'T> =
            x

        [<Inline>]
        member this.ReturnFrom(x: Async<'T>) : Promise<'T> =
            OfAsync x

        [<Inline>]
        member this.ReturnFrom(x: Task<'T>) : Promise<'T> =
            OfTask x

        [<Inline>]
        member this.Using(x: 'T when 'T :> IDisposable, f: 'T -> Promise<'U>) : Promise<'U> =
            Promise(fun (resolve, reject) -> resolve (unbox<'U> (f x)))
                .Finally(fun () -> x.Dispose())

        [<Inline>]
        member this.For(xs: seq<'T>, f: 'T -> Promise<unit>) : Promise<unit> =
            For xs f

        [<Inline>]
        member this.Zero() : Promise<unit> =
            Promise.Resolve(())

        [<Inline>]
        member this.Combine(p1: Promise<'T>, p2: Promise<'T>) : Promise<'T> =
            p1.Then<'T>(fun _ -> p2)

        [<Inline>]
        member this.TryWith(p: Promise<'T>, f: exn -> Promise<'T>) : Promise<'T> =
            p.Catch<'T>(unwrapExn >> f)

        [<Inline>]
        member this.TryFinally(p: Promise<'T>, f: unit -> unit) : Promise<'T> =
            p.Finally(fun () -> f())

        [<Inline>]
        member this.Delay(f: unit -> Promise<'T>) : Promise<'T> =
            Promise<'T>(fun (resolve, _) -> resolve (unbox<'T> (f())))

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
