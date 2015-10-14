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
open WebSharper.Html.Server
open WebSharper.Sitelets
module SampleSite = WebSharper.Sitelets.Tests.SampleSite

type FullAction =
    | Site of Actions.Action
    | SiteletsTests of SampleSite.Action

let ( => ) text url =
    A [HRef url] -< [Text text]

let Menu (ctx: Context<_>) =
    [
        LI ["Home" => ctx.Link (Site Actions.Home)]
        LI ["Client and Remoting Tests" => ctx.Link (Site Actions.Tests)]
        LI ["Sitelets Tests" => ctx.Link (SiteletsTests SampleSite.Home)]
    ]

let HomePage =
    Skin.WithTemplate "Home" Menu <| fun ctx ->
        let front = Skin.RenderFront ctx
        [
            yield! front (Skin.Page.Default "Home")
        ]

let TestsPage =
    Skin.WithTemplate "Tests" Menu <| fun ctx ->
        [
            Div [
                Testing.Runner.Run [
                    typeof<WebSharper.Collections.Tests.Dictionary.Foo>.Assembly
                    typeof<WebSharper.Tests.Object.O>.Assembly
                    typeof<WebSharper.Web.Tests.HelloWorld>.Assembly
                    typeof<WebSharper.Html5.Tests.Samples>.Assembly
                ]
                ClientSide <@ Client.RunAllTests() @> :> _
            ]
        ]

let MainSite ctx = function
    | Actions.Home -> HomePage ctx
    | Actions.Tests -> TestsPage ctx

let Main =
    Sitelet.Sum [
        Sitelet.InferPartialInUnion <@ FullAction.Site @> MainSite
        Sitelet.Shift "sitelet-tests" <|
            Sitelet.EmbedInUnion <@ FullAction.SiteletsTests @> SampleSite.EntireSite
    ]
