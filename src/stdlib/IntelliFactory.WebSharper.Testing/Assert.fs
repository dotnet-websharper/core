// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Implements testing assertions.
module IntelliFactory.WebSharper.Testing.Assert

open IntelliFactory.WebSharper

[<JavaScript>]
let Raises (f: unit -> unit) =
    try f ()
        true |? "Pass."
    with _ -> 
        false |? "Assert raises exception test failed."

[<JavaScript>]
let For<'T> (times: int) (gen: Random.Generator<'T>) (attempt: 'T -> unit) =
    for i = 0 to gen.Base.Length - 1 do
        attempt gen.Base.[i]
    for i = 1 to times do
        attempt (gen.Next())
