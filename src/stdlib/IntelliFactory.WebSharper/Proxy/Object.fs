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

namespace IntelliFactory.WebSharper

#nowarn "864"
open IntelliFactory.WebSharper.JavaScript

[<Proxy(typeof<System.Object>)>]
type private ObjectProxy =

    [<Inline "{}">]
    new () = {}

    [<Inline>]
    [<JavaScript>]
    member this.GetHashCode() = Unchecked.hash this

    [<Inline>]
    [<JavaScript>]
    member this.Equals(obj: obj) = Unchecked.equals (this :> obj) obj

    [<Inline>]
    [<JavaScript>]
    static member Equals(a: obj, b: obj) = Unchecked.equals a b

    [<Inline "$a === $b">]
    static member ReferenceEquals(a: obj, b: obj) = a ===. b

    [<Inline "String($this)">]
    member this.ToString() = X<string>
