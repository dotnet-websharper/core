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

namespace WebSharper.Web.Tests

open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
type Text(txt) =
    interface IControlBody with
        member this.ReplaceInDom elt =
            elt.ParentNode.ReplaceChild(
                JS.Document.CreateTextNode(txt), elt)
            |> ignore

type HelloWorld() =
    inherit WebSharper.Web.Control()

    [<JavaScript>]
    override this.Body =
        let o = obj ()
        JS.Set o "a" 1
        JS.Set o "b" 2
        let k = JS.Get<int> "b" o
        let t = string k
        Text t :> _
