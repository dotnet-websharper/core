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

module WebSharper.Collections.Tests.Interop

open WebSharper

[<JavaScript>]
type Record =
    {
        mutable A : int
        [<OptionalField>]
        B : option<string>
    }

    member this.X () = this.A + 1

    member this.Y () =
        match this with
        | {B = b} -> b

[<JavaScript>]
type Union =
    | A of int
    | B of name: string
    | C

    member this.X () =
        match this with
        | A a -> a
        | _ -> 0

open System
open System.Runtime.InteropServices

[<JavaScript>]
type Class() =
    let ev = Event<int>() 
    let dev = DelegateEvent<Action<int>>() 

    member this.ParamsSum([<ParamArray>] ps: int[]) = Array.sum ps
    
    member this.FSharpOptionals(?x : int, ?y: int) =
        defaultArg x 1 + defaultArg y 2    
    
    member this.Optionals([<Optional; DefaultParameterValue 1>] x : int, [<Optional; DefaultParameterValue 2>] y: int, [<Optional>] z: int) =
        x + y + z

    [<Inline>]
    member this.OptionalsInl([<Optional; DefaultParameterValue 1>] x : int, [<Optional; DefaultParameterValue 2>] y: int, [<Optional>] z: int) =
        x + y + z

    [<CLIEvent>]
    member this.Event = ev.Publish
    member this.TriggerEvent x = ev.Trigger x

    [<CLIEvent>]
    member this.DelEvent = dev.Publish
    member this.TriggerDelEvent (x: int) = dev.Trigger [|x|]

[<JavaScript>]
module Module =
    let ParamsLet ([<ParamArray>] ps: int[]) = Array.sum ps

    let ReturnsFunc () =
        ()
        fun (x: int) -> x

    let ReturnsFunc2 () =
        ()
        fun x y -> x + y

    let InvokeFunc f x = f x

    let InvokeFunc2 f x y = f x y

    let BigTuple() = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

    let ValueA = 1

    let mutable ValueB = 2

    let ErasedUnion1 = JavaScript.Union<int, string>.Union1Of2 42
    let ErasedUnion2 = JavaScript.Union<int, string>.Union2Of2 "hi"
    let UndefVal = JavaScript.Undefined : JavaScript.Optional<int>
    let DefVal = JavaScript.Defined 42

[<JavaScript>]
type GenericClass<'T>() =
    member this.GenericMethod<'U>(x: 'U) = x