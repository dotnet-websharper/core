// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Control

module internal Event =
    open System
    open IntelliFactory.WebSharper

    type Event<'T> = private { Handlers : ResizeArray<Handler<'T>> } with

        [<JavaScript>]
        member this.Trigger(x: 'T) =
            for h in this.Handlers.ToArray() do
                (As<'T -> unit> h) x

        [<JavaScript>]
        member this.AddHandler(h: Handler<'T>) =
            this.Handlers.Add h

        [<JavaScript>]
        member this.RemoveHandler(h: Handler<'T>) =
            this.Handlers
            |> Seq.tryFindIndex ((=) h)
            |> Option.iter this.Handlers.RemoveAt

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
    let New () = { Handlers = ResizeArray() }


