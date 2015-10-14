// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<Proxy(typeof<FSharpFunc<_,_>>)>]
type private FuncProxy<'T,'TResult> =  
    [<Inline "$this($arg)">]
    member this.Invoke (arg: 'T) = X<'TResult>     

    [<JavaScript; Inline "$f">]
    static member FromConverter (f: System.Converter<'T, 'TResult>) = X<'T -> 'TResult>

[<Proxy(typeof<System.Action<_>>)>]
type private Action1Proxy<'T1> =
    [<Inline "$this($x1)">]
    member this.Invoke(x1: 'T1) = ()

[<Proxy(typeof<System.Action<_,_>>)>]
type private Action2Proxy<'T1, 'T2> =
    [<Inline "$this($x1, $x2)">]
    member this.Invoke(x1: 'T1, x2: 'T2) = ()

[<Proxy(typeof<System.Func<_,_>>)>]
type private Func1Proxy<'T1, 'TResult> =
    [<Inline "$this($x1)">]
    member this.Invoke(x1: 'T1) = X<'TResult>

[<Proxy(typeof<System.Func<_,_,_>>)>]
type private Func2Proxy<'T1, 'T2, 'TResult> =
    [<Inline "$this($x1, $x2)">]
    member this.Invoke(x1: 'T1, x2: 'T2) = X<'TResult>
