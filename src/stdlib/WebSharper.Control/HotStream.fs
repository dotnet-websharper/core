// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

open WebSharper

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
            [<JavaScript>]
            member this.Subscribe(o) =
                if this.Latest.Value.IsSome then
                    o.OnNext this.Latest.Value.Value
                let disp =
                    this.Event.Publish.Subscribe(fun v ->
                        o.OnNext(v)
                    )
                disp

        [<JavaScript>]
        member this.Trigger(v) =
            this.Latest := Some v
            this.Event.Trigger v

        [<JavaScript>]
        static member New<'U>() : HotStream<'U> =
            {
                Latest = ref None
                Event = Event<_>()
            }
