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

#nowarn "864"
open WebSharper.JavaScript

[<Proxy(typeof<System.Object>)>]
type private ObjectProxy =

    [<Inline "{}">]
    new () = {}

    [<Inline>]
    override this.GetHashCode() = Unchecked.hash this

    [<Inline>]
    override this.Equals(obj: obj) = Unchecked.equals (this :> obj) obj

    [<Inline>]
    static member Equals(a: obj, b: obj) = Unchecked.equals a b

    [<Inline>]
    static member ReferenceEquals(a: obj, b: obj) = a ===. b

    [<Inline>]
    static member op_Equality(a: obj, b: obj) = a ===. b

    [<Inline>]
    override this.ToString() = string this
