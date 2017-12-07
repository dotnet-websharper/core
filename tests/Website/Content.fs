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

/// Declares server-side content utilities.
module WebSharper.Tests.Website.Content

open WebSharper
open WebSharper.Sitelets
open WebSharper.Sitelets.Tests.Server
module PerformanceTests = WebSharper.Sitelets.Tests.PerformanceTests
module SampleSite = WebSharper.Sitelets.Tests.SampleSite

[<NoComparison>]
type FullAction =
    | Site of Actions.Action
    | SiteletsTests of SampleSite.Action
    | CSharpSiteletsTests of obj
    | PerformanceTests of PerformanceTests.Action

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
                ),
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (CSharpSiteletsTests "/")),
                        Text "C# Sitelets test minisite"
                    )
                ),
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (CSharpSiteletsTests WebSharper.CSharp.Sitelets.Tests.SiteletTest.JohnDoe)),
                        Text "C# Sitelets test minisite - John Doe"
                    )
                )
            )
        ]
    )

let TestsPage (ctx: Context<FullAction>) =
    let t12 = (1, 2)
    let jsonBaseUri =
        Tests.Json.String ""
        |> SampleSite.Json
        |> SiteletsTests
        |> ctx.Link
    let apiBaseUri =
        Tests.Api.Action.GetPerson 1
        |> SampleSite.Api
        |> SiteletsTests
        |> ctx.Link
    let jsonBaseUri = jsonBaseUri.[..jsonBaseUri.LastIndexOf '/']
    let apiBaseUri = apiBaseUri.[..apiBaseUri.LastIndexOf '/']
    Content.Page(
        Title = "WebSharper client-side tests",
        Body = (
            [
                ClientSide <@ WebSharper.Tests.Main.RunTests() @>
                ClientSide <@ WebSharper.Collections.Tests.Main.RunTests() @>
                WebSharper.CSharp.Tests.InlineControlTest.RunTestsControl
                ClientSide <@ Client.ClientSideTupleTest t12 @>
                ClientSide <@ WebSharper.Html5.Tests.Main.RunTests() @>
                ClientSide <@ WebSharper.Sitelets.Tests.ApiTests.RunTests apiBaseUri @> 
                ClientSide <@ WebSharper.Web.Tests.Main.RunTests jsonBaseUri true @>
            ] : list<Web.Control>
        )
    )

let MainSite ctx = function
    | Actions.Home -> HomePage ctx
    | Actions.Tests -> TestsPage ctx

let Main =
    Sitelet.Sum [
        Sitelet.InferPartialInUnion <@ FullAction.Site @> MainSite
        Sitelet.Shift "sitelet-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.SiteletsTests @> SampleSite.EntireSite
        Sitelet.Shift "csharp-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.CSharpSiteletsTests @>
                WebSharper.CSharp.Sitelets.Tests.SiteletTest.Main
        Sitelet.Shift "perf-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.PerformanceTests @> PerformanceTests.Site
    ]
