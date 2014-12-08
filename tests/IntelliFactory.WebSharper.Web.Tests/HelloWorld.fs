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

namespace IntelliFactory.WebSharper.Web.Tests

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
module A = IntelliFactory.WebSharper.Core.Attributes
module H = IntelliFactory.WebSharper.Html.Default


[<Proxy(typeof<System.Text.StringBuilder>)>]
type StringBuilder [<JavaScript>] () =
    let mutable c = ""

    [<JavaScript>]
    member this.Append(s: string) =
        c <- c + s
        As<System.Text.StringBuilder> this

    [<JavaScript>]
    [<Name "toString">]
    override this.ToString() = c

module Client =
    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let test () =
        let sb = StringBuilder()
        sb.Append("foo")
        |> ignore
        sb.Append("bar")
        |> ignore
        Console.Log(sb.ToString())

type HelloWorld() =
    inherit IntelliFactory.WebSharper.Web.Control()

    [<A.JavaScript>]
    override this.Body =
        Client.test()
        let o = obj ()
        JS.Set o "a" 1
        JS.Set o "b" 2
        let k = JS.Get<int> "b" o
        let t = string k
        H.Div [H.Text t] :> _
