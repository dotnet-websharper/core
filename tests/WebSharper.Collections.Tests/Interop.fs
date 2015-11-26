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

module WebSharper.Tests.Interop

open WebSharper

[<JavaScript>]
type Record =
    {
        A : int
        [<OptionalField>]
        B : option<string>
    }

    member this.X () = this.A + 1

    member this.Y () =
        match this with
        | {B = b} -> b

type Union =
    | A of int
    | B of string

    member this.X () =
        match this with
        | A a -> a
        | _ -> 0


