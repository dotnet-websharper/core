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

/// Declares server-side content utilities.
module Website.Content

open WebSharper
open WebSharper.Sitelets
open WebSharper.Sitelets.Tests.Server
module SampleSite = WebSharper.Sitelets.Tests.SampleSite

type FullAction =
    | Site of Actions.Action
    | SiteletsTests of SampleSite.Action

let HomePage (ctx: Context<_>) =
    Content.Page(
        Title = "WebSharper tests",
        Body = [
            Elt("h1", Text "WebSharper tests")
            Elt("ul",
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (Site Actions.Tests)),
                        Text "Client-side test suite"
                    )
                ),
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (SiteletsTests SampleSite.Home)),
                        Text "Sitelets test minisite"
                    )
                )
            )
        ]
    )

let TestsPage =
    Content.Page(
        Title = "WebSharper client-side tests",
        Body = [
            Testing.Runner.Run [
                typeof<WebSharper.Collections.Tests.Dictionary.Foo>.Assembly
                typeof<WebSharper.Tests.Object.O>.Assembly
                typeof<WebSharper.Web.Tests.HelloWorld>.Assembly
                typeof<WebSharper.Html5.Tests.Elt>.Assembly
            ]
            ClientSide <@ Client.RunAllTests() @> :> _
        ]
    )

let MainSite ctx = function
    | Actions.Home -> HomePage ctx
    | Actions.Tests -> TestsPage

let Main =
    Sitelet.Sum [
        Sitelet.InferPartialInUnion <@ FullAction.Site @> MainSite
        Sitelet.Shift "sitelet-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.SiteletsTests @> SampleSite.EntireSite
    ]
