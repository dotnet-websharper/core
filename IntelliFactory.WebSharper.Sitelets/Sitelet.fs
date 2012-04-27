// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Sitelets

open System
open System.Collections.Generic
open System.Web.UI

/// Represents a self-contained website parameterized by the type of actions.
/// A sitelet combines a router, which is used to match incoming requests to
/// actions and actions to URLs, and a controller, which is used to handle
/// the actions.
type Sitelet<'T when 'T : equality> =
    {
        Router : Router<'T>
        Controller : Controller<'T>
    }

    /// Combines two sitelets, with the leftmost taking precedence.
    static member ( <|> ) (s1: Sitelet<'Action>, s2: Sitelet<'Action>) =
        {
            Router = s1.Router <|> s2.Router
            Controller =
                {
                    Handle = fun action ->
                        match s1.Router.Link action with
                        | Some _ -> s1.Controller.Handle action
                        | None -> s2.Controller.Handle action
                }
        }

/// Provides combinators over sitelets.
module Sitelet =

    /// Creates an empty sitelet.
    let Empty<'Action when 'Action : equality> : Sitelet<'Action> =
        {
            Router = Router.New (fun _ -> None) (fun _ -> None)
            Controller =
                {
                    Handle = fun action ->
                        Content.CustomContent <| fun _ ->
                            {
                                Status = Http.Status.NotFound
                                Headers = []
                                WriteBody = ignore
                            }
                }
        }

    /// Represents filters for protecting sitelets.
    type Filter<'Action> =
        {
            VerifyUser : string -> bool;
            LoginRedirect : 'Action -> 'Action
        }

    /// Constructs a protected sitelet given the filter specification.
    let Protect (filter: Filter<'Action>) (site: Sitelet<'Action>)
        : Sitelet<'Action> =
        {
            Router = site.Router
            Controller =
                {
                    Handle = fun action ->
                        let prot = filter
                        match UserSession.GetLoggedInUser () with
                        | Some user ->
                            if prot.VerifyUser user then
                                site.Controller.Handle action
                            else
                                Content.Redirect (prot.LoginRedirect action)
                        | None ->
                            Content.Redirect (prot.LoginRedirect action)
                }
        }

    /// Constructs a singleton sitelet that contains exactly one action
    /// and serves a single content value at a given location.
    let Content (location: string) (action: 'Action) (cnt: Content<'Action>) =
        {
            Router = Router.Table [action, location]
            Controller = { Handle = fun _ -> cnt}
        }

    /// Maps over the sitelet action type. Requires a bijection.
    let Map (f: 'T1 -> 'T2) (g: 'T2 -> 'T1) (s: Sitelet<'T1>) =
        {
            Router = Router.Map f g s.Router
            Controller =
                {
                    Handle = fun action ->
                        match s.Controller.Handle <| g action with
                        | Content.CustomContent genResp ->
                            CustomContent <| fun ctx ->
                                {
                                    ResolveUrl = ctx.ResolveUrl
                                    ApplicationPath = ctx.ApplicationPath
                                    Link = fun a -> ctx.Link (f a)
                                    Json = ctx.Json
                                    Metadata = ctx.Metadata
                                    ResourceContext = ctx.ResourceContext
                                    Request = ctx.Request
                                }
                                |> genResp
                        | Content.PageContent genPage ->
                            PageContent <| fun ctx ->
                                {
                                    ResolveUrl = ctx.ResolveUrl
                                    ApplicationPath = ctx.ApplicationPath
                                    Json = ctx.Json
                                    Link = fun a -> ctx.Link (f a)
                                    Metadata = ctx.Metadata
                                    ResourceContext = ctx.ResourceContext
                                    Request = ctx.Request
                                }
                                |> genPage
                }
        }

    /// Shifts all sitelet locations by a given prefix.
    let Shift (prefix: string) (sitelet: Sitelet<'T>) =
        {
            Router = Router.Shift prefix sitelet.Router
            Controller = sitelet.Controller
        }

    /// Combines several sitelets, leftmost taking precedence.
    /// Is equivalent to folding with the choice operator.
    let Sum (sitelets: seq<Sitelet<'T>>) : Sitelet<'T> =
        if Seq.isEmpty sitelets then Empty else
            Seq.reduce (<|>) sitelets

    /// Serves the sum of the given sitelets under a given prefix.
    /// This function is convenient for folder-like structures.
    let Folder<'T when 'T : equality> (prefix: string)
                                      (sitelets: seq<Sitelet<'T>>) =
        Shift prefix (Sum sitelets)

    /// Boxes the sitelet action type to Object type.
    let Upcast (sitelet: Sitelet<'T>) : Sitelet<obj> =
        Map box unbox sitelet

    /// Reverses the Upcast operation on the sitelet.
    let UnsafeDowncast<'T when 'T : equality> (sitelet: Sitelet<obj>) =
        Map unbox box sitelet

    /// Constructs a sitelet with an inferred router and a given controller
    /// function.
    let Infer<'T when 'T : equality> (handle : 'T -> Content<'T>) =
        {
            Router = Router.Infer()
            Controller = { Handle = handle }
        }
