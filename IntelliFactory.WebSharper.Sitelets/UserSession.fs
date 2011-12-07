// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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
open System.Configuration
open System.Diagnostics
open System.IO
open System.Reflection
open System.Web

module UserSession =
    open System
    open System.Security.Principal
    open System.Web.Security
    open System.Web

    /// Refreshes the user cookie.
    let internal Refresh() =
        match HttpContext.Current.Request.Cookies.[FormsAuthentication.FormsCookieName] with
        | null -> HttpContext.Current.User <- null
        | cookie ->
            let ticket = FormsAuthentication.Decrypt cookie.Value
            let principal = GenericPrincipal(FormsIdentity(ticket), [||])
            HttpContext.Current.User <- principal

    /// Gets the currently logged in user.
    let GetLoggedInUser () =
        let getUser () =
            match HttpContext.Current.User with
            | null ->
                None
            | x ->
                if x.Identity.IsAuthenticated then
                    x.Identity.Name
                    |> Some
                else
                    None
        match getUser () with
        | Some user ->
            Some user
        | None      ->
            // Refresh the cookie and try again
            Refresh ()
            getUser ()

    /// Login user.
    let LoginUser (user: string) =
        FormsAuthentication.SetAuthCookie(user, false)

    /// Logout current user.
    let Logout () =
        FormsAuthentication.SignOut()

