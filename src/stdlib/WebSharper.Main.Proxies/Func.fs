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

open System
open WebSharper.JavaScript

[<Proxy(typeof<FSharpFunc<_,_>>)>]
type private FuncProxy<'T,'TResult> =  
    [<Inline "$this($arg)">]
    member this.Invoke (arg: 'T) = X<'TResult>     

    [<Inline "$f">]
    static member FromConverter (f: Converter<'T, 'TResult>) = X<'T -> 'TResult>

    [<Inline "$f">]
    static member ToConverter (f: 'T -> 'TResult) = X<Converter<'T, 'TResult>>

    [<Inline "$f">]
    static member op_Implicit (f: Converter<'T, 'TResult>) = X<'T -> 'TResult>

    [<Inline "$f">]
    static member op_Implicit (f: 'T -> 'TResult) = X<Converter<'T, 'TResult>>

[<Proxy(typeof<FuncConvert>)>]
type private FuncConvertProxy =
    [<Inline>]
    static member FromAction(del: Action) = del.Invoke            
    [<Inline>]
    static member FromAction(del: Action<_>) = del.Invoke            
    [<Inline>]
    static member FromAction(del: Action<_,_>) = fun a b -> del.Invoke(a, b)            
    [<Inline>]
    static member FromAction(del: Action<_,_,_>) = fun a b c -> del.Invoke(a, b, c)            
    [<Inline>]
    static member FromAction(del: Action<_,_,_,_>) = fun a b c d -> del.Invoke(a, b, c, d)            
    [<Inline>]
    static member FromAction(del: Action<_,_,_,_,_>) = fun a b c d e -> del.Invoke(a, b, c, d, e)            
    [<Inline>]
    static member FromFunc(del: Func<_>) = del.Invoke            
    [<Inline>]
    static member FromFunc(del: Func<_,_>) = del.Invoke            
    [<Inline>]
    static member FromFunc(del: Func<_,_,_>) = fun a b -> del.Invoke(a, b)            
    [<Inline>]
    static member FromFunc(del: Func<_,_,_,_>) = fun a b c -> del.Invoke(a, b, c)            
    [<Inline>]
    static member FromFunc(del: Func<_,_,_,_,_>) = fun a b c d -> del.Invoke(a, b, c, d)            
    [<Inline>]
    static member FromFunc(del: Func<_,_,_,_,_,_>) = fun a b c d e -> del.Invoke(a, b, c, d, e)            
    [<Inline>]
    static member FuncFromTupled(f) = fun a b -> f(a, b)            
    [<Inline>]
    static member FuncFromTupled(f) = fun a b c -> f(a, b, c)            
    [<Inline>]
    static member FuncFromTupled(f) = fun a b c d -> f(a, b, c, d)            
    [<Inline>]
    static member FuncFromTupled(f) = fun a b c d e -> f(a, b, c, d, e)            
    [<Inline>]
    static member ToFSharpFunc(del: Action<_>) = del.Invoke            
    [<Inline>]
    static member ToFSharpFunc(del: Converter<_,_>) = del.Invoke            
