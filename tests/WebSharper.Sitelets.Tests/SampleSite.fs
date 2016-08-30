// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Sitelets.Tests

open WebSharper

module Client =
    open WebSharper.JavaScript

    [<JavaScript>]
    type Elt(name, text) =
        let e = JS.Document.CreateElement(name)
        do e.AppendChild(JS.Document.CreateTextNode(text)) |> ignore

        interface IControlBody with
            member this.ReplaceInDom x =
                x.ParentNode.ReplaceChild(e, x) |> ignore

    [<Sealed>]
    type SignupSequenceControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Elt("div", "SIGNUP-SEQUENCE") :> _

    [<Sealed>]
    type LoginControl(link: string) =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Elt("div", "LOGIN: " + link) :> _

    [<JavaScript>]
    let Widget () =
        Elt("button", "click me!")

/// A mini server-side HTML language
module Server =
    open WebSharper.Web

    [<AbstractClass>]
    type RequiresNoResources() =
        interface IRequiresResources with
            member this.Requires = Seq.empty
            member this.Encode(_, _) = []

    type Elt(name, [<System.ParamArray>] contents: INode[]) =
        let attributes, children =
            contents |> Array.partition (fun n -> n.IsAttribute)
        interface INode with
            member this.Write(meta, w) =
                w.WriteBeginTag(name)
                attributes |> Array.iter (fun n -> n.Write(meta, w))
                if Array.isEmpty children && System.Web.UI.HtmlTextWriter.IsSelfClosingTag(name) then
                    w.Write(System.Web.UI.HtmlTextWriter.SelfClosingTagEnd)
                else
                    w.Write(System.Web.UI.HtmlTextWriter.TagRightChar)
                    children |> Array.iter (fun n -> n.Write(meta, w))
                    w.WriteEndTag(name)
            member this.Requires = children |> Seq.collect (fun c -> c.Requires)
            member this.IsAttribute = false
            member this.AttributeValue = None
            member this.Name = Some name
            member this.Encode(meta, json) =  children |> Seq.collect (fun c -> c.Encode(meta, json)) |> List.ofSeq

    type Attr(name, value) =
        inherit RequiresNoResources()
        interface INode with
            member this.Write(meta, w) =
                w.WriteAttribute(name, value)
            member this.IsAttribute = true
            member this.AttributeValue = Some value
            member this.Name = Some name

    type Text(txt) =
        inherit RequiresNoResources()
        interface INode with
            member this.Write(meta, w) =
                w.WriteEncodedText(txt)
            member this.IsAttribute = false
            member this.AttributeValue = None
            member this.Name = None

/// The website definition.
module SampleSite =
    open WebSharper.Web
    open WebSharper.Sitelets
    open Server

    /// Actions that corresponds to the different pages in the site.
    type Action =
        | Home
        | Contact
        | Protected
        | Login of option<Action>
        | Logout
        | Echo of string
        | Api of Api.Action
        | [<EndPoint "GET /test.png">] TestImage
        | [<Method "POST">] Json of ActionEncoding.DecodeResult<Json.Action>
        | [<EndPoint "POST /files">] Files

    /// A helper function to create a hyperlink
    let private ( => ) title href =
        Elt("a", Attr("style", "padding-right:5px"), Attr("href", href), Text title)

    /// A helper function to create a 'fresh' url with a random get parameter
    /// in order to make sure that browsers don't show a cached version.
    let private RandomizeUrl url =
        url + "?d=" + System.Uri.EscapeUriString (System.DateTime.Now.ToString())

    /// User-defined widgets.
    module Widgets =

        /// Widget for displaying login status or a link to login.
        let LoginInfo (ctx: Context<Action>) =
            async {
                let! user = ctx.UserSession.GetLoggedInUser ()
                return [
                    (
                        match user with
                        | Some email ->
                            "Log Out (" + email + ")" =>
                                (RandomizeUrl <| ctx.Link Action.Logout)
                        | None ->
                            "Login" => (ctx.Link <| Action.Login None)
                    )
                ]
            }

    type Template =
        {
            Title: string
            Body: seq<Web.INode>
            Menu: seq<Web.INode>
            Login: seq<Web.INode>
        }

    let Tpl (t: Async<Template>) =
        async {
            let! t = t
            return! Content.Page(
                Title = t.Title,
                Body = [
                    yield! t.Login
                    yield! t.Menu
                    yield! t.Body
                    yield Elt("h1", Text "Client-side control:") :> _
                    yield ClientSide <@ Client.Widget () @> :> _
                ]
            )
        }

    /// A template function that renders a page with a menu bar, based on the `Skin` template.
    let Template title main (ctx: Context<Action>) =
        Tpl <|
            let menu =
                let ( ! ) x = ctx.Link x
                [
                        "Home" => !Action.Home
                        "Contact" => !Action.Contact
                        "Say Hello" => !(Action.Echo "Hello")
                        "Protected" => (RandomizeUrl <| !Action.Protected)
                ]
                |> List.map (fun link ->
                    Elt("label", Attr("class", "menu-item"), link)
                )
            async {
                let! login = Widgets.LoginInfo ctx
                return {
                    Title = title
                    Menu = Seq.cast menu
                    Login = Seq.cast login
                    Body = main ctx
                }
            }

    /// The pages of this website.
    module Pages =

        /// The home page.
        let HomePage =
            Template "Home" <| fun ctx ->
                [
                    Elt("h1", Text "Welcome to our site!")
                    "Let us know how we can contact you" => ctx.Link Action.Contact
                    Elt("h2", Text "Multiple file upload")
                    Elt("form",
                        Attr("action", ctx.Link Action.Files),
                        Attr("method", "post"),
                        Attr("enctype", "multipart/form-data"),
                        Elt("input", Attr("type", "file"), Attr("name", "input_1"), Attr("multiple", "multiple")),
                        Elt("input", Attr("type", "file"), Attr("name", "input_2"), Attr("multiple", "multiple")),
                        Elt("input", Attr("type", "submit"))
                    )
                ]

        /// A page to collect contact information.
        let ContactPage =
            Template "Contact" <| fun ctx ->
                [
                    Elt("h1", Text "Contact Form")
                    Elt("div", new Client.SignupSequenceControl())
                ]

        /// A simple page that echoes a parameter.
        let EchoPage param =
            Template "Echo" <| fun ctx ->
                [
                    Elt("h1", Text param)
                ]

        /// A simple login page.
        let LoginPage (redirectAction: option<Action>) =
            Template "Login" <| fun ctx ->
                let redirectLink =
                    match redirectAction with
                    | Some action -> action
                    | None -> Action.Home
                    |> ctx.Link
                [
                    Elt("h1", Text "Login")
                    Elt("p",
                        Text "Login with any username and password='",
                        Elt("i", Text "password"),
                        Text "'."
                    )
                    new Client.LoginControl(redirectLink)
                ]

        /// A simple page that users must log in to view.
        let ProtectedPage =
            Template "Protected" <| fun ctx ->
                [
                    Elt("h1", Text "This is protected content - thanks for logging in!")
                ]

    /// The sitelet that corresponds to the entire site.
    let EntireSite =
        // A simple sitelet for the home page, available at the root of the application.
        let home =
            Sitelet.Content "/" Action.Home Pages.HomePage

        // An automatically inferred sitelet created for the basic parts of the application.
        let basic =
            Sitelet.Infer <| fun ctx action ->
                match action with
                | Action.Contact ->
                    Pages.ContactPage ctx
                | Action.Echo param ->
                    Pages.EchoPage param ctx
                | Action.Login action->
                    Pages.LoginPage action ctx
                | Action.Logout ->
                    // Logout user and redirect to home
                    async {
                        do! ctx.UserSession.Logout ()
                        return! Content.RedirectTemporary Action.Home
                    }
                | Action.Home ->
                    Content.RedirectPermanent Action.Home
                | Action.Protected ->
                    Content.ServerError
                | Action.Api _ ->
                    Content.ServerError
                | Action.Json (ActionEncoding.Success a) ->
                    WebSharper.Sitelets.Tests.Json.Content a
                | Action.Json err ->
                    Content.Json err
                    |> Content.SetStatus Http.Status.NotFound
                | Action.TestImage ->
                    Content.File "~/image.png"
                    |> Content.WithContentType "image/png"
                | Action.Files ->
                    Content.Page(
                        Body = [
                            Elt("h1", Text "Posted files:")
                            Elt("ol",
                                [|
                                    for k, fs in ctx.Request.Files.ToList() do
                                        for f in fs do
                                            yield Elt("li", Text(sprintf "key = %s, filename = %s, length = %i, type = %s" k f.FileName f.ContentLength f.ContentType)) :> INode
                                |]
                            )
                        ]
                    )

        // A sitelet for the protected content that requires users to log in first.
        let authenticated =
            let filter : Sitelet.Filter<Action> =
                {
                    VerifyUser = fun _ -> true
                    LoginRedirect = Some >> Action.Login
                }

            Sitelet.Protect filter <|
                Sitelet.Content "/protected" Action.Protected Pages.ProtectedPage

        // A sitelet wrapping the API sitelet into the main action.
        let api = Sitelet.EmbedInUnion <@ Action.Api @> Api.Sitelet

        // Compose the above sitelets into a larger one.
        [
            home
            authenticated
            Sitelet.Shift "api" api
            basic
        ]
        |> Sitelet.Sum
