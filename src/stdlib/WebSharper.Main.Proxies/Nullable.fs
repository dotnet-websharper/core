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

namespace WebSharper

open WebSharper.JavaScript

[<JavaScript>]
module Nullable =
    let get (x: obj) =
        if x ===. null then failwith "Nullable object must have a value." else x

////    let getOrDefault<'T> (x: 'T) =
//        if x ==. null then Unchecked.defaultof<'T> else x     

    let getOrValue<'T> (x: 'T) (v: 'T)  =
        if x ===. null then v else x     

[<Proxy(typeof<System.Nullable<_>>)>]
type private NullableProxy<'T> =
    
    [<Inline "null">]
    new () = {}

    [<Inline "$v">]
    new (v: 'T) = {}

    member this.Value 
        with [<JavaScript; Inline>] get(): 'T = As<'T>(Nullable.get this)

    member this.HasValue
        with [<JavaScript; Inline>] get() = this !=. null

    [<Inline>]
    member this.GetValueOrDefault() : 'T = Nullable.getOrValue (As<'T> this) Unchecked.defaultof<'T>

    [<Inline>]
    member this.GetValueOrDefault(v: 'T) : 'T = Nullable.getOrValue (As<'T> this) v
