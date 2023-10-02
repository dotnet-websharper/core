namespace WebSharper.StaticHtml.Tests.NetStandard

open WebSharper
open WebSharper.Sitelets
open WebSharper.Web

type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /about">] About

module Site =
    [<AbstractClass>]
    type RequiresNoResources() =
        interface IRequiresResources with
            member this.Requires(_) = Seq.empty
            member this.Encode(_, _) = []

    type Elt(name, [<System.ParamArray>] contents: INode[]) =
        let attributes, children =
            contents |> Array.partition (fun n -> n.IsAttribute)
        interface IRequiresResources with
            member this.Requires(meta) = children |> Seq.collect (fun c -> c.Requires(meta))
            member this.Encode(meta, json) =  children |> Seq.collect (fun c -> c.Encode(meta, json))
        interface INode with
            member this.Write(ctx, w) =
                w.WriteBeginTag(name)
                attributes |> Array.iter (fun n -> n.Write(ctx, w))
                if Array.isEmpty children && WebSharper.Core.Resources.HtmlTextWriter.IsSelfClosingTag(name) then
                    w.Write(WebSharper.Core.Resources.HtmlTextWriter.SelfClosingTagEnd)
                else
                    w.Write(WebSharper.Core.Resources.HtmlTextWriter.TagRightChar)
                    children |> Array.iter (fun n -> n.Write(ctx, w))
                    w.WriteEndTag(name)
            member this.IsAttribute = false

    type Attr(name, value) =
        inherit RequiresNoResources()
        interface INode with
            member this.Write(ctx, w) =
                w.WriteAttribute(name, value)
            member this.IsAttribute = true

    type Text(txt) =
        inherit RequiresNoResources()
        interface INode with
            member this.Write(ctx, w) =
                w.WriteEncodedText(txt)
            member this.IsAttribute = false

    /// A helper function to create a hyperlink
    let private ( => ) title href =
        Elt("a", Attr("style", "padding-right:5px"), Attr("href", href), Text title)

    let HomePage (ctx: Context<EndPoint>) =
        Content.Page(
            Title = "Home",
            Body = [
                Elt("h1", Text "Welcome to our site!")
                "About us" => ctx.Link EndPoint.About
                Elt("div", Web.InlineControl ( Client.Elt "b" [|Client.Text "It's working baby"|] ))
                Elt("div",
                    Text """This should say 'Checking "attribute" encoding':""",
                    Elt("input", Attr("placeholder", """Checking "attribute" encoding"""))
                )
                Elt("div",
                    Web.InlineControl
                        ( Client.Elt "i" [|
                            Client.Text "On the "
                            Client.Elt "b" [|Client.Text "client side"|]
                            Client.Text " too!"
                        |] ))
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
