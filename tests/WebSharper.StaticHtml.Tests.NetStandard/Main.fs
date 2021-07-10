namespace WebSharper.StaticHtml.Tests.NetStandard

open WebSharper
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /about">] About

module Site =
    open WebSharper.Sitelets.Tests
    open WebSharper.Sitelets.Tests.Server

    /// A helper function to create a hyperlink
    let private ( => ) title href =
        Elt("a", Attr("style", "padding-right:5px"), Attr("href", href), Text title)

    let HomePage (ctx: Context<EndPoint>) =
        Content.Page(
            Title = "Home",
            Body = [
                Elt("h1", Text "Welcome to our site!")
                "About us" => ctx.Link EndPoint.About
                Elt("div", ClientSide <@ Client.Elt "b" [|Client.Text "It's working baby"|] @>)
                Elt("div",
                    Text """This should say 'Checking "attribute" encoding':""",
                    Elt("input", Attr("placeholder", """Checking "attribute" encoding"""))
                )
                Elt("div",
                    ClientSide
                        <@ Client.Elt "i" [|
                            Client.Text "On the "
                            Client.Elt "b" [|Client.Text "client side"|]
                            Client.Text " too!"
                        |] @>)
                Elt("div", ClientSide <@ Client.Main() @>)
            ]
        )

    let AboutPage (ctx: Context<EndPoint>) =
        Content.Page(
            Title = "About",
            Body = [
                Elt("h1", Text "About")
                Elt("p", Text "This is a template WebSharper generated html application.")
            ]
        )

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx action ->
            match action with
            | Home -> HomePage ctx
            | About -> AboutPage ctx
        )

[<Sealed>]
type Website() =
    interface IWebsite<EndPoint> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home; About]

[<assembly: Website(typeof<Website>)>]
do ()
