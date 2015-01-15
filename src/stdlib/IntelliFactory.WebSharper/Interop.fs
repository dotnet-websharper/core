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

module private IntelliFactory.WebSharper.InteropProxy

open IntelliFactory.WebSharper.JavaScript

[<Proxy(typeof<FuncWithArgs<_,_>>)>]
type FuncWithArgsProxy<'TArgs, 'TResult> =
    [<Inline "function() { return $func(arguments); }">]
    new (func: 'TArgs -> 'TResult) = {}
    
    member this.Length with [<Inline "$this.length">] get() = X<int>
       
    [<Inline "$this.apply(null, $args)">]
    member this.Call (args: 'Args) = X<'TResult>

[<Proxy(typeof<FuncWithThis<_,_>>)>]
type FuncWithThisProxy<'TThis, 'TFunc> =
    [<Inline "function() { return $func.apply(this, arguments); }">]
    new (func: 'TThis -> 'TFunc) = {}

    member this.Length with [<Inline "$this.length">] get() = 0

    [<Inline "$this.bind($thisArg)">]
    member this.Bind (thisArg: 'TThis) = Unchecked.defaultof<'TFunc>

[<Proxy(typeof<Arguments<_>>)>]
type ArgumentsProxy<'T> =
    member this.Length with [<Inline "$this.length">] get() = 0

    member this.Item with [<Inline "$this[$i]">] get (i: int) = Unchecked.defaultof<'T>

    [<Inline "Array.prototype.slice.call($this)">]
    member this.ToArray() = Unchecked.defaultof<'T[]>

[<Proxy(typeof<FuncWithRest<_,_,_>>)>]
type FuncWithRestProxy<'TArg, 'TRest, 'TResult> =
    [<Inline "function(x) { return $func([x, Array.prototype.slice.call(arguments, 1)]); }">]
    new (func: 'TArg * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, [$arg].concat($rest))">]
    member this.Call (arg: 'TArg, [<System.ParamArray>] rest: 'TRest) = Unchecked.defaultof<'TResult>

[<Proxy(typeof<FuncWithArgsRest<_,_,_>>)>]
type FuncWithArgsRestProxy<'TArgs, 'TRest, 'TResult> =
    [<Inline "function(x) { return $func([Array.prototype.slice.call(arguments, 0, $length), Array.prototype.slice.call(arguments, $length)]); }">]
    new (length: int, func: 'TArgs * 'TRest[] -> 'TResult) = {}

    [<Inline "$this.apply(null, $args.concat($rest))">]
    member this.Call (args: 'TArgs, [<System.ParamArray>] rest: 'TRest) = Unchecked.defaultof<'TResult>
