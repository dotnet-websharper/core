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

module internal Event =
    open System
    open IntelliFactory.WebSharper

    [<Inline "$x.push($y)">]
    let private push (x: obj) (y: obj) = X<unit>

    [<Inline "$x.splice($y,1)">]
    let private cut (x: obj) (y: int) = X<unit>

    type Event<'T> = private { Handlers : obj [] } with

        [<JavaScript>]
        member this.Trigger(x: 'T) =
            for i = 0 to this.Handlers.Length - 1 do
                (this.Handlers.[i] :?> 'T -> unit) x

        [<JavaScript>]
        member this.AddHandler(h: Handler<'T>) =
            push this.Handlers h

        [<JavaScript>]
        member this.RemoveHandler(h: Handler<'T>) =
            let x =
                this.Handlers
                |> Array.mapi (fun i x ->
                    if x = (box h) then
                        Some i
                    else
                        None
                )
                |> Array.choose id
                |> Array.toList
            match x with
            | []        -> ()
            | n :: _    -> cut this.Handlers n

        [<JavaScript>]
        member this.Subscribe(observer: IObserver<'T>) =
            let h = new Handler<'T>(fun _ x -> observer.OnNext x)
            this.AddHandler h
            Disposable.Of (fun () -> this.RemoveHandler h)

        interface IDisposable with
            member this.Dispose() = X<unit>

        interface IObservable<'T> with
            member this.Subscribe observer = X<IDisposable>

        interface IDelegateEvent<Handler<'T>> with
            member this.AddHandler x = X<unit>
            member this.RemoveHandler x = X<unit>

        interface IEvent<'T>

    [<Inline>]
    [<JavaScript>]
    let New () = { Handlers = [||] }


