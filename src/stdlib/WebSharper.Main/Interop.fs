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

module private WebSharper.InteropProxy

open WebSharper.JavaScript

[<Proxy(typeof<FuncWithArgs<_,_>>)>]
type FuncWithArgsProxy<'TArgs, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithArgs($func)">]
    new (func: 'TArgs -> 'TResult) = {}
    
    member this.Length with [<Inline "$this.length">] get() = X<int>
       
    [<Inline "$this.apply(null, $args)">]
    member this.Call (args: 'Args) = X<'TResult>

[<Proxy(typeof<FuncWithThis<_,_>>)>]
type FuncWithThisProxy<'TThis, 'TFunc> =
    [<Inline "$wsruntime.CreateFuncWithThis($func)">]
    new (func: 'TThis -> 'TFunc) = {}

    member this.Length with [<Inline "$this.length">] get() = 0

    [<Inline "$wsruntime.Bind($this, $thisArg)">]
    member this.Bind (thisArg: 'TThis) = X<'TFunc>

type PA = System.ParamArrayAttribute

[<Proxy(typeof<FuncWithRest<_,_>>)>]
type FuncWithRestProxy<'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(0, $func)">]
    new (func: 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, $rest)">]
    member this.Call ([<PA>] rest: 'TRest[]) = X<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_>>)>]
type FuncWithRestProxy<'TArg, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(1, $func)">]
    new (func: 'TArg * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg].concat($rest))">]
    member this.Call (arg: 'TArg, [<PA>] rest: 'TRest[]) = X<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_,_>>)>]
type FuncWithRestProxy<'TArg1, 'TArg2, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(2, $func)">]
    new (func: 'TArg1 * 'TArg2 * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg1, $arg2].concat($rest))">]
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_,_,_>>)>]
type FuncWithRestProxy<'TArg1, 'TArg2, 'TArg3, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(3, $func)">]
    new (func: 'TArg1 * 'TArg2 * 'TArg3 * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg1, $arg2, $arg3].concat($rest))">]
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_,_,_,_>>)>]
type FuncWithRestProxy<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(4, $func)">]
    new (func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg1, $arg2, $arg3, $arg4].concat($rest))">]
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_,_,_,_,_>>)>]
type FuncWithRestProxy<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TArg5, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(5, $func)">]
    new (func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TArg5 * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg1, $arg2, $arg3, $arg4, $arg5].concat($rest))">]
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, arg5: 'TArg5, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithRest<_,_,_,_,_,_,_,_>>)>]
type FuncWithRestProxy<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TArg5, 'TArg6, 'TRest, 'TResult> =
    [<Inline "$wsruntime.CreateFuncWithRest(6, $func)">]
    new (func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TArg5 * 'TArg6 * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg1, $arg2, $arg3, $arg4, $arg5, $arg6].concat($rest))">]
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, arg5: 'TArg5, arg6: 'TArg6, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithArgsRest<_,_,_>>)>]
type FuncWithArgsRestProxy<'TArgs, 'TRest, 'TResult> =
    [<Macro(typeof<WebSharper.Macro.FuncWithArgsRest>)>]
    new (func: 'TArgs * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, $args.concat($rest))">]
    member this.Call (args: 'TArgs, [<PA>] rest: 'TRest[]) = Unchecked.defaultof<'TResult>
