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

namespace WebSharper.Sitelets.Tests

module Client =
    open WebSharper
    open WebSharper.Html.Client

    [<Sealed>]
    type SignupSequenceControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Div [Text "SIGNUP-SEQUENCE"] :> _

    [<Sealed>]
    type LoginControl(link: string) =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Div [Text ("LOGIN: " + link)] :> _

    [<JavaScript>]
    let Widget () =
        Button [Text "click me!"]

/// The website definition.
module SampleSite =
    open WebSharper
    open WebSharper.Html.Server
    open WebSharper.Sitelets

    /// Actions that corresponds to the different pages in the site.
    type Action =
        | Home
        | Contact
        | Protected
        | Login of option<Action>
        | Logout
        | Echo of string
        | Api of Api.Action
        | [<Method "POST">] Json of ActionEncoding.DecodeResult<Json.Action>

    /// A helper function to create a hyperlink
    let private ( => ) title href =
        A [HRef href] -< [Text title]

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
            Body: seq<Html.Element>
            Menu: seq<Html.Element>
            Login: seq<Html.Element>
        }

    let Tpl =
        Content.Template.FromHtmlElement(
            HTML [
                Head [
                    Tags.Title [Text "${title}"]
                    Meta [Attr.Data "replace" "scripts"]
                ]
                Body [
                    Div [Attr.Data "replace" "login"]
                    Div [Attr.Data "replace" "menu"]
                    Div [Attr.Data "replace" "body"]
                    Div [ClientSide <@ Client.Widget () @>]
                ]
            ])
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)
            .With("menu", fun x -> x.Menu)
            .With("login", fun x -> x.Login)

    /// A template function that renders a page with a menu bar, based on the `Skin` template.
    let Template title main ctx =
        Content.WithTemplateAsync Tpl <|
            let menu =
                let ( ! ) x = ctx.Link x
                [
                        "Home" => !Action.Home
                        "Contact" => !Action.Contact
                        "Say Hello" => !(Action.Echo "Hello")
                        "Protected" => (RandomizeUrl <| !Action.Protected)
                        "ASPX Page" => ctx.ResolveUrl "http://www.nba.com/~joel/file.html"
                ]
                |> List.map (fun link ->
                    Label [Class "menu-item"] -< [link]
                )
            async {
                let! login = Widgets.LoginInfo ctx
                return {
                    Title = title
                    Menu = menu
                    Login = login
                    Body = main ctx
                }
            }

    /// The pages of this website.
    module Pages =

        /// The home page.
        let HomePage =
            Template "Home" <| fun ctx ->
                [
                    H1 [Text "Welcome to our site!"]
                    "Let us know how we can contact you" => ctx.Link Action.Contact
                 ]

        /// A page to collect contact information.
        let ContactPage =
            Template "Contact" <| fun ctx ->
                [
                    H1 [Text "Contact Form"]
                    Div [new Client.SignupSequenceControl()]
                ]

        /// A simple page that echoes a parameter.
        let EchoPage param =
            Template "Echo" <| fun ctx ->
                [
                    H1 [Text param]
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
                    H1 [Text "Login"]
                    P [
                        Text "Login with any username and password='"
                        I [Text "password"]
                        Text "'."
                    ]
                    Div [
                        new Client.LoginControl(redirectLink)
                    ]
                ]

        /// A simple page that users must log in to view.
        let ProtectedPage =
            Template "Protected" <| fun ctx ->
                [
                    H1 [Text "This is protected content - thanks for logging in!"]
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

///// Expose the main sitelet so that it can be served.
///// This needs an IWebsite type and an assembly level annotation.
//type SampleWebsite() =
//    interface WebSharper.Sitelets.IWebsite<SampleSite.Action> with
//        member this.Sitelet = SampleSite.EntireSite
//        member this.Actions = []
//
//[<assembly: WebSharper.Sitelets.WebsiteAttribute(typeof<SampleWebsite>)>]
//do ()
