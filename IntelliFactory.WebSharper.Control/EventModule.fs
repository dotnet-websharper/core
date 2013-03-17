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

[<Proxy "Microsoft.FSharp.Control.EventModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
module EventModule =

    [<Inline>]
    [<JavaScript>]
    let Add f (e: IEvent<_,_>) = e.Add f

    [<JavaScript>]
    let Choose c (e: IEvent<_,_>) : IEvent<_> =
        // let r = Event.New ()
        let r = Event<_>()
        e.Add (fun x ->
            match c x with
            | Some y    -> r.Trigger y
            | None      -> ())
        r.Publish :> _

    [<JavaScript>]
    let Filter ok (e: IEvent<_,_>) : IEvent<_> =
        let r = Event.New ()
        e.Add (fun x -> if ok x then r.Trigger x)
        r :> _

    [<JavaScript>]
    let Map f (e: IEvent<_,_>) : IEvent<_> =
        let r = Event.New ()
        e.Add (fun x -> r.Trigger (f x))
        r :> _

    [<JavaScript>]
    let Merge<'D1,'T,'D2 when 'D1 :> Delegate
                          and 'D1 :  delegate<'T,unit>
                          and 'D2 :> Delegate
                          and 'D2 :  delegate<'T,unit>>
            (e1: IEvent<'D1,'T>) (e2: IEvent<'D2,'T>) : IEvent<_> =
        let r = Event.New ()
        e1.Add r.Trigger
        e2.Add r.Trigger
        r :> _

    [<JavaScript>]
    let Pairwise (e: IEvent<_,_>) : IEvent<_> =
        let buf = ref None
        let ev  = Event.New ()
        e.Add (fun x ->
            match buf.Value with
            | None ->
                buf := Some x
            | Some old ->
                buf := Some x
                ev.Trigger(old, x))
        ev :> _

    [<JavaScript>]
    let Partition f e =
        (Event.filter f e, Event.filter (f >> not) e)

    [<JavaScript>]
    let Scan fold seed e =
        let state = ref seed
        let f value =
            state := fold !state value
            !state
        Event.map f e

    [<JavaScript>]
    let Split (f: 'T -> Choice<'U1,'U2>) (e: IEvent<'Del,'T>) =
        (
            e |> Event.choose (fun x ->
                match f x with
                | Core.Choice1Of2 x -> Some x
                | _                 -> None),
            e |> Event.choose (fun x ->
                match f x with
                | Core.Choice2Of2 x -> Some x
                | _                 -> None)
        )

