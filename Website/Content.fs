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
    ]

let HomePage =
    Skin.WithTemplate "Home" Menu <| fun ctx ->
        [
        ]

let Main =
    Sitelet.Sum [
        Sitelet.Content "/" Actions.Home HomePage
    ]
