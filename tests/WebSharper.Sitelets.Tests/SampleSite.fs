// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open System
open WebSharper

module Client =
    open WebSharper.JavaScript

    [<JavaScript>]
    type Node =
        | Elt of string * Node[]
        | Text of string
        | Attr of string * string

        member this.ToNode() =
            match this with
            | Text t -> Choice1Of2 (JS.Document.CreateTextNode(t) :> Dom.Node)
            | Elt (n, ch) ->
                let e = JS.Document.CreateElement(n)
                for ch in ch do
                    match ch.ToNode() with
                    | Choice1Of2 n -> e.AppendChild(n) |> ignore
                    | Choice2Of2 a -> e.SetAttributeNode(a) |> ignore
                Choice1Of2 (e :> Dom.Node)
            | Attr (n, v) -> Choice2Of2 (JS.Document.CreateAttribute(n, Value = v))

        interface IControlBody with
            member this.ReplaceInDom x =
                match this.ToNode() with
                | Choice1Of2 n -> x.ParentNode.ReplaceChild(n, x) |> ignore
                | Choice2Of2 _ -> x.ParentNode.RemoveChild(x) |> ignore

    [<JavaScript>]
    let Elt n ([<ParamArray>] ch) = Node.Elt(n, ch)

    [<JavaScript>]
    let Text t = Node.Text(t)

    [<JavaScript>]
    let Attr n v = Node.Attr(n, v)

    [<Sealed>]
    type SignupSequenceControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Elt "div" [|Text "SIGNUP-SEQUENCE"|] :> _

    
    [<JavaScript>]
    type LoginLink(link: string) =
        member this.Link = link

    [<Sealed>]
    type LoginControl(link: LoginLink) =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Elt "form" [|
                Attr "method" "post"
                Attr "action" link.Link
                Elt "div" [|Text ("LOGIN: " + link.Link)|]
                Elt "input" [| Attr "name" "login"; Attr "placeholder" "Username" |]
                Elt "input" [| Attr "name" "password"; Attr "placeholder" "Password" |]
                Elt "input" [| Attr "type" "submit"; Attr "value" "Log in" |]
            |] :> _

    [<JavaScript>]
    type ButtonText(text: string) =
        member this.Text = text

    [<JavaScript>]
    let Widget (buttonText: ButtonText) =
        Elt "button" [|Text buttonText.Text|]

/// A mini server-side HTML language
module Server =
    open WebSharper.Web

    [<AbstractClass>]
    type RequiresNoResources() =
        interface IRequiresResources with
            member this.Requires(_, _, _) = Seq.empty

    type Elt(name, [<System.ParamArray>] contents: INode[]) =
        let attributes, children =
            contents |> Array.partition (fun n -> n.IsAttribute)
        interface IRequiresResources with
            member this.Requires(meta, json, getId) = children |> Seq.collect (fun c -> c.Requires(meta, json, getId))
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
        | Login
        | [<FormData("login", "password")>] DoLogin of login: string * password: string
        | [<Query("firstName", "lastName", "message")>] FormResultGet of firstName: string * lastName: string * message: string
        | [<FormData("firstName", "lastName", "message")>] FormResultPost of firstName: string * lastName: string * message: string
        | Logout
        | Echo of string
        | Api of Api.Action
        | [<EndPoint "GET /test.png">] TestImage
        | [<Method "POST">] Json of ParseRequestResult<Json.Action>
        | [<EndPoint "/"; Wildcard>] AnythingElse of string

    /// A helper function to create a hyperlink
    let private ( => ) title href =
        Elt("a", Attr("style", "padding-right:5px"), Attr("href", href), Text title)

    /// A helper function to create a 'fresh' url with a random get parameter
    /// in order to make sure that browsers don't show a cached version.
    let private RandomizeUrl url =
        url + "?d=" + System.Uri.EscapeDataString (System.DateTime.Now.ToString())

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
                            "Login" => (ctx.Link Action.Login)
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
            let bt = Client.ButtonText("click me!")
            return! Content.Page(
                Title = t.Title,
                Body = [
                    yield! t.Login
                    yield! t.Menu
                    yield! t.Body
                    yield Web.InlineControl ( Client.Widget bt ) :> _
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
                    Elt("div", Web.InlineControl (Client.Elt "b" [|Client.Text "It's working baby"|] ))
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
                ]

        /// A page to collect contact information.
        let ContactPage =
            let form isGet =
                let method = if isGet then "get" else "post" 
                let action = "/sitelet-tests/FormResult" + if isGet then "Get" else "Post"  
                Elt("div",
                    Elt("form", Attr("action", action), Attr("method", method),
                        Text("First name: "),
                        Elt("input", Attr("type", "text"), Attr("name", "firstName")),
                        Text("Last name: "),
                        Elt("input", Attr("type", "text"), Attr("name", "lastName")),
                        Elt("textarea", Attr("name", "message")),
                        Elt("input", Attr("type", "submit"), Attr("value", "Submit with " + method))
                    )
                )
            Template "Contact" <| fun ctx ->
                [
                    Elt("h1", Text "Contact Form")
                    Elt("div", new Client.SignupSequenceControl())

                    form true
                    form false
                ]

        /// A simple page that echoes a parameter.
        let EchoPage param =
            Template "Echo" <| fun ctx ->
                [
                    Elt("h1", Text param)
                ]

        /// A simple page that echoes two form fields.
        let FormResult x y z =
            Template "Form result" <| fun ctx ->
                [
                    Elt("h1", Text (x + " " + y))
                    Elt("code", Text z)
                ]
       
        /// A simple login page.
        let LoginPage =
            Template "Login" <| fun ctx ->
                let redirectLink = Client.LoginLink (ctx.Link (Action.DoLogin ("", "")))
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
                | Action.Login ->
                    Pages.LoginPage ctx
                | Action.DoLogin (user, pass) ->
                    async {
                        if pass = "password" then
                            do! ctx.UserSession.LoginUser user
                            return! Content.RedirectTemporary Action.Home
                        else
                            return! Content.Text "Invalid password"
                                |> Content.SetStatus Http.Status.Forbidden
                    }
                | Action.Logout ->
                    // Logout user and redirect to home
                    async {
                        do! ctx.UserSession.Logout ()
                        return! Content.RedirectTemporary Action.Home
                    }
                | Action.FormResultGet (x, y, z)
                | Action.FormResultPost (x, y, z) ->
                    Pages.FormResult x y z ctx
                | Action.Home ->
                    Content.RedirectPermanent Action.Home
                | Action.Protected ->
                    Content.ServerError
                | Action.Api _ ->
                    Content.ServerError
                | Action.Json (ParseRequestResult.Success a) ->
                    WebSharper.Sitelets.Tests.Json.Content a
                | Action.Json err ->
                    Content.Json err
                    |> Content.SetStatus Http.Status.NotFound
                | Action.TestImage ->
                    Content.File "~/image.png"
                    |> Content.WithContentType "image/png"
                | Action.AnythingElse p ->
                    Content.Text ("Unmatched path: " + p)
                    |> Content.SetStatus Http.Status.NotFound

        // A sitelet for the protected content that requires users to log in first.
        let authenticated =
            let filter : Sitelet.Filter<Action> =
                {
                    VerifyUser = fun _ -> true
                    LoginRedirect = fun _ -> Action.Login
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
