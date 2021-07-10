namespace WebSharper.StaticHtml.Tests.NetStandard

open WebSharper
open WebSharper.JavaScript
open WebSharper.Sitelets.Tests.Client

[<JavaScript>]
module Client =

    let Main () =

        Elt "div" [| Text "And this too." |]
