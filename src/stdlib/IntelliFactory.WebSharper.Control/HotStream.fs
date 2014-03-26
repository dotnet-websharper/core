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

module internal HotStream =
    open System

    /// Implementation for "hot stream".
    /// Subscribers to hot streams will only observe the latest
    /// and future values.
    type HotStream<'T> =
        internal
            {
                Latest  : ref<option<'T>>
                Event   : Event<'T>
            }
        interface IObservable<'T> with
            [<ReflectedDefinition>]
            member this.Subscribe(o) =
                if this.Latest.Value.IsSome then
                    o.OnNext this.Latest.Value.Value
                let disp =
                    this.Event.Publish.Subscribe(fun v ->
                        o.OnNext(v)
                    )
                disp

        [<ReflectedDefinition>]
        member this.Trigger(v) =
            this.Latest := Some v
            this.Event.Trigger v

        [<ReflectedDefinition>]
        static member New<'U>() : HotStream<'U> =
            {
                Latest = ref None
                Event = Event<_>()
            }
