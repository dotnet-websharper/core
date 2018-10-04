// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

[<WebSharper.Name "Util">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Control.CommonExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.CommonExtensionsProxy

open WebSharper.JavaScript

let observer (h: 'T -> unit) : System.IObserver<'T> =
    { new System.IObserver<'T> with 
        member this.OnCompleted() = ()
        member this.OnError _ = ()
        member this.OnNext args = h args
    }

[<Inline>]
let AddToObservable<'T> (event: System.IObservable<'T>) (h: 'T -> unit) =
    event.Subscribe(observer h) |> ignore

[<Inline>]
let SubscribeToObservable<'T> (event: System.IObservable<'T>) (h: 'T -> unit) =
    event.Subscribe(observer h)
