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

namespace IntelliFactory.WebSharper.JavaScript

open IntelliFactory.WebSharper.Core.Attributes

type FuncWithArgs<'TArgs, 'TResult>(func: 'TArgs -> 'TResult) =
    member this.Length = 0      
    member this.Call(args: 'Args) = Unchecked.defaultof<'TResult>

type FuncWithThis<'TThis, 'TFunc>(func: 'TThis -> 'TFunc) =
    member this.Length = 0
    member this.Bind (thisArg: 'TThis) = Unchecked.defaultof<'TFunc>

type Arguments<'T> =
    member this.Length = 0
    member this.Item with get (i: int) = Unchecked.defaultof<'T>
    member this.ToArray() = Unchecked.defaultof<'T[]>

type FuncWithRest<'TArg, 'TRest, 'TResult>(func: 'TArg * 'TRest[] -> 'TResult) =
    member this.Call (arg: 'TArg, [<System.ParamArray>] rest: 'TRest) = Unchecked.defaultof<'TResult>

type FuncWithArgsRest<'TArgs, 'TRest, 'TResult>(length: int, func: 'TArgs * 'TRest[] -> 'TResult) =
    member this.Call (args: 'TArgs, [<System.ParamArray>] rest: 'TRest) = Unchecked.defaultof<'TResult>
