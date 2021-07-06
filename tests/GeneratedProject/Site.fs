namespace SynthUI

module Site =
    open WebSharper
    open WebSharper.Sitelets
    open WebSharper.Sitelets.Tests.Server


    [<Website>]
    let Main =
        Application.SinglePage (fun ctx ->
                let HomePage (ctx: Context<_>) =
                    Content.Page(
                        Title = "WebSharper tests",
                        Body = [
                            Elt("h1", Text "WebSharper tests")
                            ]
                            )
                HomePage ctx
            )

