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

