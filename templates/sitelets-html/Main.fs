namespace $safeprojectname$

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

[<Sealed>]
type EmptyWebsite() =
    interface IWebsite<unit> with
        member this.Actions = []
        member this.Sitelet = Sitelet.Empty

[<assembly: Website(typeof<EmptyWebsite>)>]
do ()
