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

namespace WebSharper.JavaScript

open WebSharper.Core.Attributes

/// Thrown on the server when client-side code is being executed.
exception ClientSideOnly
    with override this.Message = "This function is intended for client-side use only."

[<AutoOpen>]
module Pervasives =
    /// Specifies a value intended for client-side use only, so that there is no
    /// .NET implementation.
    /// Raises a WebSharper.JavaScript.ClientSideOnly exception.
    let X<'T> : 'T = 
        raise ClientSideOnly

type private PA = System.ParamArrayAttribute

type Function([<System.ParamArray>] paramsAndBody: string[]) =
    member this.Length = X<int>
    member this.ApplyUnsafe(thisArg: obj) = X<obj>
    member this.ApplyUnsafe(thisArg: obj, argsArray: obj[]) = X<obj>
    member this.CallUnsafe(thisArg: obj, [<PA>] args: obj[]) = X<obj>
    member this.BindUnsafe(thisArg: obj, [<PA>] args: obj[]) = X<Function>
    
    /// Type cast.
    /// Warning: a tupled F# function is translated to JavaScript as a function with a single array argument.
    static member Of<'T, 'U>(func: 'T -> 'U) = X<Function>

type FuncWithArgs<'TArgs, 'TResult>(func: 'TArgs -> 'TResult) =
    inherit Function()    
    member this.Call(args: 'Args) = X<'TResult>

type FuncWithThis<'TThis, 'TFunc>(func: 'TThis -> 'TFunc) =
    inherit Function()
    member this.Bind (thisArg: 'TThis) = X<'TFunc>

type FuncWithOnlyThis<'TThis, 'TResult>(func: 'TThis -> 'TResult) =
    inherit FuncWithThis<'TThis, unit -> 'TResult>(X)
    member this.Bind (thisArg: 'TThis) = X<'TFunc>

type FuncWithRest<'TRest, 'TResult>(func: 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call ([<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg, 'TRest, 'TResult>(func: 'TArg * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg: 'TArg, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg1, 'TArg2, 'TRest, 'TResult>(func: 'TArg1 * 'TArg2 * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg1, 'TArg2, 'TArg3, 'TRest, 'TResult>(func: 'TArg1 * 'TArg2 * 'TArg3 * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TRest, 'TResult>(func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TArg5, 'TRest, 'TResult>(func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TArg5 * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, arg5: 'TArg5, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithRest<'TArg1, 'TArg2, 'TArg3, 'TArg4, 'TArg5, 'TArg6, 'TRest, 'TResult>(func: 'TArg1 * 'TArg2 * 'TArg3 * 'TArg4 * 'TArg5 * 'TArg6 * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (arg1: 'TArg1, arg2: 'TArg2, arg3: 'TArg3, arg4: 'TArg4, arg5: 'TArg5, arg6: 'TArg6, [<PA>] rest: 'TRest[]) = X<'TResult>

type FuncWithArgsRest<'TArgs, 'TRest, 'TResult>(func: 'TArgs * 'TRest[] -> 'TResult) =
    inherit Function()
    member this.Call (args: 'TArgs, [<PA>] rest: 'TRest[]) = X<'TResult>
