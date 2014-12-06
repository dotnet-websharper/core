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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Util">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Control.CommonExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.CommonExtensionsProxy

open IntelliFactory.WebSharper.JavaScript

[<Inline "$event.Subscribe($obs)">]
let subscribe (event: System.IObservable<'T>) (obs: System.IObserver<'T>) =
    X<System.IDisposable>

[<JavaScript>]
let observer (h: 'T -> unit) : System.IObserver<'T> =
    New [
        "OnCompleted" => ignore
        "OnError" => ignore
        "OnNext" => h
    ]

[<JavaScript>]
[<Name "addListener">]
let AddToObservable<'T> (event: System.IObservable<'T>) (h: 'T -> unit) =
    ignore (subscribe event (observer h))

[<JavaScript>]
[<Name "subscribeTo">]
let SubscribeToObservable<'T> (event: System.IObservable<'T>) (h: 'T -> unit) =
    subscribe event (observer h)
