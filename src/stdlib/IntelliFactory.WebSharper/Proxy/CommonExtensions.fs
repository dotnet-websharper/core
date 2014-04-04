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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Util">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Control.CommonExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.CommonExtensionsProxy

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
