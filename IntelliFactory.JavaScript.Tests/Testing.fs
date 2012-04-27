// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

[<AutoOpen>]
module IntelliFactory.JavaScript.Test.Testing

let mutable private section = ""
let mutable private status = 0
let mutable private count = 0

exception private Fail of string

let private run (name: string) test =
    try
        test ()
        count <- count + 1
    with
    | Fail reason ->
        status <- 1
        stderr.WriteLine("FAIL: {0}.{1} -- {2}", section, name, reason)
    | e ->
        status <- 1
        stderr.WriteLine("FAIL: {0}.{1}", section, name)
        stderr.WriteLine(e)

type Test(name: string) =
    member this.Delay(f : unit -> unit) = run name f
    member this.Zero() = ()

let Section name =
    section <- name

let Throws<'T when 'T :> exn> f =
    try
        f ()
        raise (Fail (sprintf "Does not throw: %O" typeof<'T>))
    with
    | :? 'T ->
        ()

let ( =? ) a b =
    if a <> b then
        raise (Fail (sprintf "Expected %A and got %A." b a))

let ( <>? ) a b =
    if a = b then
        raise (Fail (sprintf "Unexpected %A." a))

let Report () =
    if status = 0 then
        stdout.WriteLine("OK, {0} tests passed.", count)
    status
