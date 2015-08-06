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

module WebSharper.Tests.Proxy

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<Proxy(typeof<System.Text.StringBuilder>)>]
type StringBuilderProxy [<JavaScript>] () =
    let mutable c = ""

    [<JavaScript>]
    [<Name "append">]
    member this.Append(s: string) =
        c <- c + s
        As<System.Text.StringBuilder> this

    [<JavaScript>]
    [<Name "toString">]
    override this.ToString() = c

[<JavaScript>]
let Tests =

    TestCategory "Custom proxy" {

        Test "StringBuilder" {
            let sb = System.Text.StringBuilder()
            isTrueMsg (sb?append !==. JS.Undefined) "[<Name>] attribute on proxy method"
            sb.Append("foo") |> ignore
            sb.Append("bar") |> ignore
            equal (sb.ToString()) "foobar"
        }

    }
