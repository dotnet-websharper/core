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

namespace WebSharper

open WebSharper.JavaScript

[<Proxy(typeof<System.Lazy<_>>)>]
[<Name "WebSharper.Lazy">]
[<Type "$wstype(../WebSharper.StdLib/WebSharper.LazyExtensionsProxy.LazyRecord`1)<$0>">]
type private LazyProxy<'T> =

    [<Inline; JavaScript>]
    static member CtorProxy(valueFactory: System.Func<'T>) =
        Lazy.Create valueFactory.Invoke

    member this.IsValueCreated
        with [<Inline "$this.c">] get () = X<bool>

    member this.Value
        with [<Inline "$this.f()">] get () = X<'T>


