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

open IntelliFactory.WebSharper

[<Proxy(typeof<Event<_>>)>]
type private FSharpEvent<'T> [<JavaScript>] () =
    let event = Event.New ()

    [<Inline>]
    [<JavaScript>]
    member this.Trigger(x: 'T) = event.Trigger x

    [<JavaScript>]
    member this.Publish with [<Inline>] get () = event :> IEvent<_>

[<Proxy(typeof<IDelegateEvent<_>>)>]
type private IDelegateEventProxy<'D> =
    abstract AddHandler : 'D -> unit
    abstract RemoveHandler : 'D -> unit