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

[<Proxy(typeof<System.Enum>)>]
type internal EnumProxy =

    [<Inline>]
    member this.CompareTo(x: obj) =
        Unchecked.compare (this :> obj) x

    [<Inline "($0 & $1) === $1">]
    member p.HasFlag (flag: System.Enum) = false

    [<Inline "$this === $x">]
    member this.Equals(x: obj) = X<bool>

    [<Inline>]
    member this.GetHashCode() = hash this
