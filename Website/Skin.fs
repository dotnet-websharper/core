/// Utilities for server-side markup generation.
module Website.Skin

open System
open System.Web
open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

type Page =
    {
        Body : list<Content.HtmlElement>
        Footer : list<Content.HtmlElement>
        Menu : list<Content.HtmlElement>
        Project : string
        Title : string
        Version : string
    }

let MainTemplate =
    let path = HttpContext.Current.Server.MapPath("~/Main.html")
    Content.Template<Page>(path)
        .With("body", fun x -> x.Body)
        .With("menu", fun x -> x.Menu)
        .With("footer", fun x -> x.Footer)
        .With("project", fun x -> x.Project)
        .With("title", fun x -> x.Title)
        .With("version", fun x -> x.Version)

let Footer =
    [
        Text (String.Format("Copyright (c) 2008-{0} IntelliFactory", DateTime.Now.Year))
    ]

let WithTemplate title menu body : Content<Actions.Action> =
    Content.WithTemplate MainTemplate <| fun context ->
        {
            Footer = Footer
            Body =
                Div [Style "display:none"] -< [new Controls.EntryPoint()]
                :: body context
            Menu = menu context
            Project = "Site"
            Title = title
            Version = Config.Version
        }
