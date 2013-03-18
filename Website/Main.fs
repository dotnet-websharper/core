namespace Website

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

[<Sealed>]
type WebsiteEntryPoint() =
    interface IWebsite<Actions.Action> with
        member this.Sitelet = Content.Main
        member this.Actions = [Actions.Home]

[<assembly: Website(typeof<WebsiteEntryPoint>)>]
do ()
