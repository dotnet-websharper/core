// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
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
