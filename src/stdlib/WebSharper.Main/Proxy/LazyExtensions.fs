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

[<WebSharper.Core.Attributes.Name "Lazy">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Control.LazyExtensions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.LazyExtensionsProxy

open WebSharper.JavaScript

[<JavaScript>]
let Create (f: unit -> 'T) : Lazy<'T> =
    let x =
        {
            value    = Unchecked.defaultof<'T>
            created  = false
            eval     = f
        }
    let get () =
        if x.created then
            x.value
        else
            x.created <- true
            x.value <- f ()
            x.value
    x.eval <- get
    As x

[<JavaScript>]
let CreateFromValue (v: 'T) : Lazy<'T> =
    let x =
        {
            value   = v
            created = true
            eval    = fun () -> v
        }
    x.eval <- fun () -> v
    As x

[<JavaScript>]
let Force (x: Lazy<'T>) : 'T =
    As<LazyProxy<'T>>(x).eval()
