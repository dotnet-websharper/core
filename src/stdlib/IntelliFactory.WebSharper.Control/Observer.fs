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

[<JavaScript>]
module internal Observer =

    type Message<'T> =
        | Message of 'T
        | Error of exn
        | Completed

    type private Observer<'T> =
        {
            onNext : 'T -> unit
            onError : exn -> unit
            onCompleted : unit -> unit
        }

        interface IObserver<'T> with
            member __.OnNext x = __.onNext x
            member __.OnError e = __.onError e
            member __.OnCompleted() = __.onCompleted ()

    let Of f : IObserver<_> =
        upcast {
            onNext = fun x -> f x
            onError = fun x -> raise x
            onCompleted = fun () -> ()
        }

    let New (f, e, c) : IObserver<_> =
        upcast {
            onNext = f
            onError = e
            onCompleted = c
        }
