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

[<WebSharper.Name "WebSharper.Lazy">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Control.LazyExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.LazyExtensionsProxy

open WebSharper.JavaScript

[<JavaScript; Prototype false>]
[<Proxy(typeof<System.Lazy<_>>)>]
[<Name "WebSharper.Lazy">]
type LazyProxy<'T> =
    [<Name "c">] val mutable public created : bool
    [<Name "v">] val mutable public evalOrVal : obj
    [<Name "f">] val mutable public force : unit -> 'T

    new (f: System.Func<'T>) =
        {
            created = false
            evalOrVal = f.Invoke
            force = LazyProxy<'T>.forceLazy
        }

    new (v: 'T) =
        {
            created = true
            evalOrVal = v
            force = LazyProxy<'T>.cachedLazy
        }

    static member forceLazy () =
        let v = (JS.This<LazyProxy<'T>>.evalOrVal :?> unit -> 'T)()
        JS.This<LazyProxy<'T>>.created <- true
        JS.This<LazyProxy<'T>>.evalOrVal <- v
        JS.This<LazyProxy<'T>>.force <- As LazyProxy<'T>.cachedLazy
        v

    static member cachedLazy () =
        JS.This<LazyProxy<'T>>.evalOrVal :?> 'T

    member this.IsValueCreated
        with [<Inline>] get () = this.created

    member this.Value
        with [<Inline>] get () = this.force()

[<Inline>]
let Create (f: unit -> 'T) =
    new LazyProxy<'T>(f)

[<Inline>]
let CreateFromValue (v: 'T) =
    new LazyProxy<'T>(v)

[<Inline>]
let Force (x: LazyProxy<'T>) : 'T =
    x.force()
