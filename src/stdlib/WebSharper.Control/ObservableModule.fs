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

namespace WebSharper.Control

open System
open WebSharper

[<Proxy "Microsoft.FSharp.Control.ObservableModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
module private ObservableModule =

    [<JavaScript>]
    [<Inline>]
    let Add (f: 'T -> unit) (o: IObservable<'T>) =
        ignore (o.Subscribe (Observer.Of f))

    [<JavaScript>]
    [<Inline>]
    let Choose (c: 'T -> option<'U>) (e: IObservable<'T>) =
        Observable.Choose c e

    [<JavaScript>]
    [<Inline>]
    let Filter (ok: 'T -> bool) (e: IObservable<'T>) : IObservable<'T> =
        Observable.Filter ok e

    [<JavaScript>]
    [<Inline>]
    let Map (f: 'T -> 'U) (e: IObservable<'T>) : IObservable<'U> =
        Observable.Map f e

    [<JavaScript>]
    [<Inline>]
    let Merge (e1: IObservable<'T>) (e2: IObservable<'T>) =
        Observable.Merge e1 e2

    [<JavaScript>]
    let Pairwise (e: IObservable<'T>) : IObservable<'T * 'T> =
        Observable.New <| fun o1 ->
            let last = ref None
            let on v =
                match !last with
                | None -> ()
                | Some l -> o1.OnNext(l, v)
                last := Some v
            e.Subscribe <| Observer.New(on, o1.OnError, o1.OnCompleted)  

    [<JavaScript>]
    let Partition (f: 'T -> bool) (e: IObservable<'T>) :
            IObservable<'T> * IObservable<'T> =
        (Observable.Filter f e, Observable.filter (f >> not) e)

    [<JavaScript>]
    let Scan (fold: 'U -> 'T -> 'U) (seed: 'U) (e: IObservable<'T>) : IObservable<'U> =
        Observable.New <| fun o1 ->
            let state = ref seed
            let on v = 
                Observable.Protect (fun () -> fold !state v) 
                    (fun s -> state := s; o1.OnNext s) o1.OnError
            e.Subscribe <| Observer.New(on, o1.OnError, o1.OnCompleted)  

    [<JavaScript>]
    let Split (f: 'T -> Core.Choice<'U1,'U2>) (e: IObservable<'T>) :
            IObservable<'U1> * IObservable<'U2> =
        let left =
            e
            |> Observable.choose (fun x ->
                match f x with
                | Core.Choice1Of2 x -> Some x
                | _ -> None)
        let right =
            e
            |> Observable.choose (fun x ->
                match f x with
                | Core.Choice2Of2 x -> Some x
                | _ -> None)
        (left, right)

    [<JavaScript>]
    [<Inline>]
    let Subscribe (f: 'T -> unit) (e: IObservable<'T>) =
        e.Subscribe f
