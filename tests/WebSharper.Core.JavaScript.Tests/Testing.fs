// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

[<AutoOpen>]
module WebSharper.Core.JavaScript.Test.Testing

let mutable private section = ""
let mutable private failures = []
let mutable private count = 0

exception private Fail of string

let private run (name: string) test =
    try
        test ()
        count <- count + 1
    with
    | Fail reason ->
        failures <- sprintf "%s.%s" section name :: failures
        stderr.WriteLine("FAIL: {0}.{1} -- {2}", section, name, reason)
    | e ->
        failures <- sprintf "%s.%s" section name :: failures
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

/// Call at the end of each NUnit test: fails it if any Test since the previous call failed.
/// All Tests still run; each failure is printed to stderr as it happens.
let Report () =
    let failed = List.rev failures
    failures <- []
    let passed = count
    count <- 0
    if List.isEmpty failed then
        stdout.WriteLine("OK, {0} tests passed.", passed)
    else
        NUnit.Framework.Assert.Fail(sprintf "%d test(s) failed: %s" failed.Length (String.concat ", " failed))
