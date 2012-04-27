// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module internal Observer =
    open System
    open IntelliFactory.WebSharper

    type Message<'T> =
        | Message of 'T
        | Error of exn
        | Completed

    type private Observer<'T> =
        {
            OnNext      : 'T -> unit
            OnError     : exn -> unit
            OnCompleted : unit -> unit
        }

        interface IObserver<'T> with
            member this.OnNext x        = X<unit>
            member this.OnError e       = X<unit>
            member this.OnCompleted()   = X<unit>

    [<JavaScript>]
    let Of f : IObserver<_> =
        upcast {
            OnNext      = fun x -> f x
            OnError     = fun x -> raise x
            OnCompleted = fun () -> ()
        }

    [<JavaScript>]
    let New (f,e,c) : IObserver<_> =
        upcast {
            OnNext      = f
            OnError     = e
            OnCompleted = c
        }
