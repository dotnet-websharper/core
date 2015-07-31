// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

namespace WebSharper.Sitelets

open System

[<Obsolete "\
    In Rpc functions, use WebSharper.Web.Remoting.GetContext().UserSession. \
    In Sitelets, use context.UserSession.">]
module UserSession =
    open WebSharper.Web

    /// Gets the currently logged in user.
    /// Warning: Must be called from the same thread as the request.
    let GetLoggedInUser () =
        Remoting.GetContext().UserSession.GetLoggedInUser() |> Async.RunSynchronously

    /// Login user.
    /// Warning: Must be called from the same thread as the request.
    let LoginUser (user: string) =
        Remoting.GetContext().UserSession.LoginUser(user, false) |> Async.RunSynchronously

    /// Login user and persist the login across browser sessions.
    /// Warning: Must be called from the same thread as the request.
    let LoginUserPersistent (user: string) =
        Remoting.GetContext().UserSession.LoginUser(user, true) |> Async.RunSynchronously

    /// Logout current user.
    /// Warning: Must be called from the same thread as the request.
    let Logout () =
        Remoting.GetContext().UserSession.Logout() |> Async.RunSynchronously

