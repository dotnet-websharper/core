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

/// Declares server-side content utilities.
module WebSharper.Tests.Website.Content

open WebSharper
open WebSharper.Sitelets
open WebSharper.Sitelets.Tests.Server
open WebSharper.Core.Json

module PerformanceTests = WebSharper.Sitelets.Tests.PerformanceTests
module SampleSite = WebSharper.Sitelets.Tests.SampleSite
module DateTimeFormatInfer = WebSharper.Sitelets.Tests.DateTimeFormatInfer

[<NoComparison>]
type FullAction =
    | Site of Actions.Action
    | SiteletsTests of SampleSite.Action
    | CSharpSiteletsTests of obj
    | PerformanceTests of PerformanceTests.Action
    | DateTimeFormatTest of DateTimeFormatInfer.EndPoint

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
                ),
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (Site <| Actions.TestQuery(6, "qwe"))),
                        Text "Query test link"
                    )
                ),
                Elt("li",
                    Elt("a",
                        Attr("href", ctx.Link (Site <| Actions.TestQuery2(6, "qwe"))),
                        Text "Query test link 2"
                    )
                )
            )
        ]
    )

let TestQueryPage (ctx: Context<_>) (a: int, b: string) =
    Content.Page(
        Title = "WebSharper tests",
        Body = [
            Elt("h1", Text <| sprintf "WebSharper tests %i %s" a b)
        ]
    )

let TestsPage runServerTests autoStart (ctx: Context<FullAction>) =
    let t12 = (1, 2)
    let jsonBaseUri =
        Tests.Json.String ""
        |> ParseRequestResult.Success
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
    let corsBaseUri =
        //System.Configuration.ConfigurationManager.AppSettings.["CorsTestUrl"]
        //|> Option.ofObj
        None : string option
    Content.Page(
        Title = "WebSharper client-side tests",
        Body = (
            [
                yield Web.InlineControl ( WebSharper.Tests.Main.RunTests runServerTests autoStart ) :> Web.Control
                yield Web.InlineControl ( WebSharper.Collections.Tests.Main.RunTests() ) :> Web.Control
                yield WebSharper.CSharp.Tests.InlineControlTest.RunTestsControl runServerTests
                yield Web.InlineControl ( Client.ClientSideTupleTest t12 ) :> Web.Control
                yield Web.InlineControl ( WebSharper.Html5.Tests.Main.RunTests true ) :> Web.Control
                yield Web.InlineControl ( WebSharper.Sitelets.Tests.ClientServerTests.RunTests apiBaseUri corsBaseUri runServerTests ) :> Web.Control
                if runServerTests then
                    yield Web.InlineControl ( WebSharper.Sitelets.Tests.ApiTests.RunTests apiBaseUri ) :> Web.Control
                    yield Web.InlineControl ( WebSharper.Module.Tests.Main.RunTests() ) :> Web.Control
                yield Web.InlineControl ( WebSharperWebTestsMain.RunTests jsonBaseUri runServerTests ) :> Web.Control
            ] : list<Web.Control>
        )
    )

let MainSite runServerTests ctx = function
    | Actions.Home -> HomePage ctx
    | Actions.Tests -> TestsPage runServerTests true ctx
    | Actions.ConsoleTests -> TestsPage runServerTests false ctx
    | Actions.TestQuery (a, b) -> TestQueryPage ctx (a, b)
    | Actions.TestQuery2 (a, b) -> TestQueryPage ctx (a, b)

let Main runServerTests =
    System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- new System.Globalization.CultureInfo("en-US")
    Sitelet.Sum [
        Sitelet.InferPartialInUnion <@ FullAction.Site @> (MainSite runServerTests)
        Sitelet.Shift "sitelet-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.SiteletsTests @> SampleSite.EntireSite
        Sitelet.Shift "csharp-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.CSharpSiteletsTests @>
                WebSharper.CSharp.Sitelets.Tests.SiteletTest.Main
        Sitelet.Shift "perf-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.PerformanceTests @> PerformanceTests.Site
        Sitelet.Shift "datetimeformat-test" <|
            Sitelet.EmbedInUnion <@ FullAction.DateTimeFormatTest @> DateTimeFormatInfer.Site
    ]
