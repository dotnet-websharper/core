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

/// Common utilities for testing.
[<AutoOpen>]
module IntelliFactory.WebSharper.Testing.Pervasives

open IntelliFactory.WebSharper

let private X<'T> : 'T = Unchecked.defaultof<_>

[<Inline "test($name,$action)">]
let private run (name: string) (action: unit -> unit) = ()

/// Tests a given assertion.
[<Inline "ok($state,$message)">]
let ( |? ) (state: bool) (message: string) = ()

/// A syntactic utility for delaying test code.
type TestBuilder [<JavaScript>] (name: string) =

    [<JavaScript>]
    member this.Delay(f : unit -> unit) = run name f

    [<JavaScript>]
    member this.Zero() = ()

/// Creates a test with a given name.
[<JavaScript>]
let Test name = TestBuilder name

/// Starts a new test section.
[<Inline "module($name)">]
let Section (name: string) : unit = X

/// Performs an equality test.
[<Name "Is">]
[<JavaScript>]
let ( =? ) (a: 'T) (b: 'T) : unit =
    if not (Unchecked.equals a b) then
        let msg = "Equality test failed."
        JavaScript.Log(msg, a, b)
        false |? msg
    else
        true |? "Pass."

/// Performs an inequality test.
[<Name "Isnt">]
[<JavaScript>]
let ( <>? ) (a: 'T) (b: 'T) =
    if Unchecked.equals a b then
        let msg = "Inequality test failed."
        JavaScript.Log(msg, a, b)
        false |? msg
    else
        true |? "Pass."
