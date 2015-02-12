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

namespace IntelliFactory.WebSharper.Sitelets.Tests

module Client =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html.Client

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

/// The website definition.
module SampleSite =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html.Server
    open IntelliFactory.WebSharper.Sitelets

    /// Actions that corresponds to the different pages in the site.
    type Action =
        | Home
        | Contact
        | Protected
        | Login of option<Action>
        | Logout
        | Echo of string
        | [<CompiledName "api">] Api of Api.Action

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
            let user = UserSession.GetLoggedInUser ()
            [
                (
                    match user with
                    | Some email ->
                        "Log Out (" + email + ")" =>
                            (RandomizeUrl <| ctx.Link Action.Logout)
                    | None ->
                        "Login" => (ctx.Link <| Action.Login None)
                )
            ]

    /// A template function that renders a page with a menu bar, based on the `Skin` template.
    let Template title main : Content<Action> =
        let menu (ctx: Context<Action>)=
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
        Content.PageContent (fun ctx ->
            let login = Widgets.LoginInfo ctx
            let body =
                Div [
                    Div login
                    Div (menu ctx)
                    Div (main ctx)
                ]
            {
                Page.Default with
                    Title = Some title
                    Body = [body]
            })

    /// The pages of this website.
    module Pages =

        /// The home page.
        let HomePage : Content<Action> =
            Template "Home" <| fun ctx ->
                [
                    H1 [Text "Welcome to our site!"]
                    "Let us know how we can contact you" => ctx.Link Action.Contact
                 ]

        /// A page to collect contact information.
        let ContactPage : Content<Action> =
            Template "Contact" <| fun ctx ->
                [
                    H1 [Text "Contact Form"]
                    Div [new Client.SignupSequenceControl()]
                ]

        /// A simple page that echoes a parameter.
        let EchoPage param : Content<Action> =
            Template "Echo" <| fun ctx ->
                [
                    H1 [Text param]
                ]

        /// A simple login page.
        let LoginPage (redirectAction: option<Action>): Content<Action> =
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
        let ProtectedPage : Content<Action> =
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
            Sitelet.Infer <| fun action ->
                match action with
                | Action.Contact ->
                    Pages.ContactPage
                | Action.Echo param ->
                    Pages.EchoPage param
                | Action.Login action->
                    Pages.LoginPage action
                | Action.Logout ->
                    // Logout user and redirect to home
                    UserSession.Logout ()
                    Content.Redirect Action.Home
                | Action.Home ->
                    Content.Redirect Action.Home
                | Action.Protected ->
                    Content.ServerError
                | Action.Api _ ->
                    Content.ServerError

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
            api
            basic
        ]
        |> Sitelet.Sum

///// Expose the main sitelet so that it can be served.
///// This needs an IWebsite type and an assembly level annotation.
//type SampleWebsite() =
//    interface IntelliFactory.WebSharper.Sitelets.IWebsite<SampleSite.Action> with
//        member this.Sitelet = SampleSite.EntireSite
//        member this.Actions = []
//
//[<assembly: IntelliFactory.WebSharper.Sitelets.WebsiteAttribute(typeof<SampleWebsite>)>]
//do ()
