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

namespace IntelliFactory.WebSharper.Web.Tests

open IntelliFactory.WebSharper
module A = IntelliFactory.WebSharper.Core.Attributes
module H = IntelliFactory.WebSharper.Html.Default
module J = IntelliFactory.WebSharper.JavaScript


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
        JavaScript.Log(sb.ToString())

type HelloWorld() =
    inherit IntelliFactory.WebSharper.Web.Control()

    [<A.JavaScript>]
    override this.Body =
        Client.test()
        let o = obj ()
        J.Set o "a" 1
        J.Set o "b" 2
        let k = J.Get<int> "b" o
        let t = string k
        H.Div [H.Text t] :> _
