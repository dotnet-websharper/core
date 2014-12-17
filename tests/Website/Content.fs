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

/// Declares server-side content utilities.
module Website.Content

open IntelliFactory.WebSharper.Sitelets.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

let ( => ) text url =
    A [HRef url] -< [Text text]

let Menu (ctx: Context<Actions.Action>) =
    [
        LI ["Home" => ctx.Link Actions.Home]
        LI ["Tests" => ctx.Link Actions.Tests]
    ]

let HomePage =
    Skin.WithTemplate "Home" Menu <| fun ctx ->
        let front = Skin.RenderFront ctx
        [
            yield! front (Skin.Page.Default "Home")
        ]

let TestsPage =
    Skin.WithTemplate "Tests"  Menu <| fun ctx ->
        [
            Div [Text "Tests"]
            Div [Id "qunit"]
            Div [Id "qunit-fixture"]
            Div [new Controls.Tests()]
            Div [new Web.Tests.RemotingTests()]
        ]

let Main =
    Sitelet.Sum [
        Sitelet.Content "/" Actions.Home HomePage
        Sitelet.Content "/tests" Actions.Tests TestsPage
    ]
