// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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
open WebSharper.Collections.Tests.SplitProxy
open WebSharper.JavaScript
open WebSharper.Testing

module R = WebSharper.Testing.RandomValues

[<JavaScript false>]
type IIsClient =
    abstract member IsClient: unit -> bool

[<Proxy(typeof<IIsClient>)>]
type internal IIsClientProxy =
    [<Inline>]
    member this.IsClient() = true

type IIsClientClass [<JavaScript>] () =
    interface IIsClient with
        member this.IsClient() = false

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

        Test "Interface with JavaScript false" {
            isTrue ((IIsClientClass() :> IIsClient).IsClient())
        }

        Test "Split proxy compilation" {
            equalMsg (ClassInfoMergeTestType.Member1 ()) "member1" "Member1 compiled"
            equalMsg (ClassInfoMergeTestType.Member2 ()) "member2" "Member2 compiled"
        }

        Test "Internal proxy do not carry over projects" {
            raises (System.Net.WebUtility.UrlDecode("Hello%20world"))
        }
    }
