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

#if JAVASCRIPT
[<WebSharper.Name "Lazy">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Control.LazyExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.LazyExtensionsProxy

open WebSharper.JavaScript

[<JavaScript; Prototype false>]
type LazyRecord<'T> =
    {
        [<Name "c">] mutable created : bool
        [<Name "v">] mutable evalOrVal : obj
        [<Name "f">] mutable force : unit -> 'T
    }

let cachedLazy<'T> () =
    JS.This.evalOrVal

let forceLazy<'T> () =
    let v = (As JS.This.evalOrVal)()
    JS.This.created <- true
    JS.This.evalOrVal <- v
    JS.This.force <- As cachedLazy
    v

let Create (f: unit -> 'T) : Lazy<'T> =
    As {
        created = false
        evalOrVal = f
        force = As forceLazy
    }

let CreateFromValue (v: 'T) : Lazy<'T> =
    As {
        created = true
        evalOrVal = v
        force = As cachedLazy
    }

let Force (x: Lazy<'T>) : 'T =
    As<LazyRecord<'T>>(x).force()
#else
namespace WebSharper
#endif