// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Control

open System
open IntelliFactory.WebSharper

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





