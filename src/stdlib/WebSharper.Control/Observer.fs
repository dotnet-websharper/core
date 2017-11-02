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

open System
open WebSharper

[<JavaScript>]
module internal Observer =

    type Message<'T> =
        | Message of 'T
        | Failed of exn
        | Completed

    let Of f : IObserver<_> =
        { new IObserver<'T> with
            member __.OnNext x = f x
            member __.OnError x = raise x
            member __.OnCompleted() = ()
        }

    let New (f, e, c) : IObserver<_> =
        { new IObserver<'T> with
            member __.OnNext x = f x
            member __.OnError x = e x
            member __.OnCompleted() = c()
        }
