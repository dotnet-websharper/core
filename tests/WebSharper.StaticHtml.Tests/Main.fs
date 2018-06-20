namespace WebSharper.StaticHtml.Tests

open WebSharper.Sitelets

type WebsiteEntryPoint() =
    inherit WebSharper.Tests.Website.WebsiteEntryPoint()

    override this.Sitelet = WebSharper.Tests.Website.Content.Main false

[<assembly: Website(typeof<WebsiteEntryPoint>)>]
do ()
