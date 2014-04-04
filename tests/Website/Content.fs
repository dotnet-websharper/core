/// Declares server-side content utilities.
module Website.Content

open IntelliFactory.Html
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
        ]

let Main =
    Sitelet.Sum [
        Sitelet.Content "/" Actions.Home HomePage
        Sitelet.Content "/tests" Actions.Tests TestsPage
    ]
