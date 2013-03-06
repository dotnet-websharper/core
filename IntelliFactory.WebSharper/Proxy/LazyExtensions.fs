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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Lazy">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Control.LazyExtensions, \
     FSharp.Core, Version=2.0.0.0, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.LazyExtensionsProxy

type LazyRecord<'T> =
    {
        mutable value   : 'T
        mutable created : bool
        mutable eval    : unit -> 'T
    }

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
    As<LazyRecord<'T>>(x).eval()
