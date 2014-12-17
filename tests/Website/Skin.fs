// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

/// Utilities for server-side markup generation.
module Website.Skin

open System
open System.Web
open IntelliFactory.WebSharper.Sitelets.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

let Footer =
    [
        Text (String.Format("Copyright (c) 2008-{0} IntelliFactory", DateTime.Now.Year))
    ]

type Page =
    {
        Body : list<Content.HtmlElement>
        Footer : list<Content.HtmlElement>
        Menu : list<Content.HtmlElement>
        Project : string
        Title : string
        Version : string
    }

    static member Default(title) =
        {
            Footer = Footer
            Body = [Div [Style "display:none"] -< [new Controls.EntryPoint()]]
            Menu = []
            Project = "Site"
            Title = title
            Version = Config.Version
        }

let MainTemplate =
    Content.Template<Page>("~/Main.html")
        .With("body", fun x -> x.Body)
        .With("menu", fun x -> x.Menu)
        .With("footer", fun x -> x.Footer)
        .With("project", fun x -> x.Project)
        .With("title", fun x -> x.Title)
        .With("version", fun x -> x.Version)

let FrontTempalte =
    Content.Template<Page>("~/Front.html")
        .With("body", fun x -> x.Body)
        .With("menu", fun x -> x.Menu)
        .With("footer", fun x -> x.Footer)
        .With("project", fun x -> x.Project)
        .With("title", fun x -> x.Title)
        .With("version", fun x -> x.Version)

let RenderFront (ctx: Context<Actions.Action>) =
    let front = FrontTempalte.Compile(ctx.RootFolder)
    fun x -> front.Run(x, ctx.RootFolder)

let WithTemplate title menu body : Content<Actions.Action> =
    Content.WithTemplate MainTemplate <| fun context ->
        let p = Page.Default title
        {
            p with
                Menu = p.Menu @ menu context
                Body = p.Body @ body context
        }
