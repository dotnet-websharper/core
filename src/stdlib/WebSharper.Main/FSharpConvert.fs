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

open System
open WebSharper.JavaScript

[<JavaScript; Sealed>]
type FSharpConvert =
    [<Inline>]
    static member Fun(del: Action) = del.Invoke            
    [<Inline>]
    static member Fun(del: Action<_>) = del.Invoke            
    [<Inline>]
    static member Fun(del: Action<_,_>) = fun a b -> del.Invoke(a, b)            
    [<Inline>]
    static member Fun(del: Action<_,_,_>) = fun a b c -> del.Invoke(a, b, c)            
    [<Inline>]
    static member Fun(del: Action<_,_,_,_>) = fun a b c d -> del.Invoke(a, b, c, d)            
    [<Inline>]
    static member Fun(del: Action<_,_,_,_,_>) = fun a b c d e -> del.Invoke(a, b, c, d, e)            
    [<Inline>]
    static member Fun(del: Action<_,_,_,_,_,_>) = fun a b c d e f -> del.Invoke(a, b, c, d, e, f)            
    [<Inline>]
    static member Fun(del: Action<_,_,_,_,_,_,_>) = fun a b c d e f g -> del.Invoke(a, b, c, d, e, f, g)            
    [<Inline>]
    static member Fun(del: Action<_,_,_,_,_,_,_,_>) = fun a b c d e f g h -> del.Invoke(a, b, c, d, e, f, g, h)            
    [<Inline>]
    static member Fun(del: Func<_>) = del.Invoke            
    [<Inline>]
    static member Fun(del: Func<_,_>) = del.Invoke            
    [<Inline>]
    static member Fun(del: Func<_,_,_>) = fun a b -> del.Invoke(a, b)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_>) = fun a b c -> del.Invoke(a, b, c)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_,_>) = fun a b c d -> del.Invoke(a, b, c, d)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_,_,_>) = fun a b c d e -> del.Invoke(a, b, c, d, e)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_,_,_,_>) = fun a b c d e f -> del.Invoke(a, b, c, d, e, f)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_,_,_,_,_>) = fun a b c d e f g -> del.Invoke(a, b, c, d, e, f, g)            
    [<Inline>]
    static member Fun(del: Func<_,_,_,_,_,_,_,_,_>) = fun a b c d e f g h -> del.Invoke(a, b, c, d, e, f, g, h)            
    [<Inline>]
    static member Option<'T >(value: 'T) = if obj.ReferenceEquals(value, null) then None else Some value
    [<Inline>]
    static member Option<'T when 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType>(value: Nullable<'T>) = if value.HasValue then Some value.Value else None
    [<Inline>]
    static member Some<'T>(value: 'T) = Some value
    [<Inline>]
    static member List([<ParamArray>] elems) = List.ofArray elems
    [<Inline>]
    static member List(elems) = List.ofSeq elems
    [<Inline>]
    static member Ref<'T>() = ref Unchecked.defaultof<'T>
    [<Inline>]
    static member Ref<'T>(value) = ref<'T> value
    [<Inline>]
    static member Async(task: System.Threading.Tasks.Task) : Async<unit> = Async.AwaitTask (As<System.Threading.Tasks.Task<unit>> task) // hack needed for FSharp 3.0
    [<Inline>]
    static member Async(task: System.Threading.Tasks.Task<'T>) = Async.AwaitTask task
    [<Inline>]
    static member AsyncValue(value: 'T) = async.Return value