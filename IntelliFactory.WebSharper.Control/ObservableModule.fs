// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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
    FSharp.Core, Version=2.0.0.0, Culture=neutral, \
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
        ((None, None), e)
        ||> Observable.scan (fun (_, o) n -> (o, Some n))
        |> Observable.choose (function
            | Some x, Some y    -> Some (x, y)
            | _                 -> None)

    [<JavaScript>]
    let Partition (f: 'T -> bool) (e: IObservable<'T>) :
            IObservable<'T> * IObservable<'T> =
        (Observable.filter f e, Observable.filter (f >> not) e)

    [<JavaScript>]
    let Scan (fold: 'U -> 'T -> 'U) (seed: 'U) (e: IObservable<'T>) :
            IObservable<'U> =
        let state = ref seed
        let f value =
            state := fold !state value
            !state
        Observable.map f e

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





