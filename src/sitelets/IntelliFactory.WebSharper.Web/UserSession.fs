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

namespace IntelliFactory.WebSharper.Web

/// Manages user sessions in a web application.
type IUserSession =

    /// Check whether user sessions are currently available.
    abstract member IsAvailable : bool

    /// Retrieve the currently logged in user.
    abstract GetLoggedInUser : unit -> Async<option<string>>

    /// Log in the given user.
    /// Set `persistent` to true to persist the login across browser sessions.
    abstract LoginUser : string * ?persistent: bool -> Async<unit>

    /// Log out the current user.
    abstract Logout : unit -> Async<unit>

[<AutoOpen>]
module IUserSessionExtensions =

    type IUserSession with
        static member NotAvailable =
            { new IUserSession with
                member this.IsAvailable = false
                member this.GetLoggedInUser() = async { return None }
                member this.LoginUser(name, ?persistent) = async { return () }
                member this.Logout() = async { return () }
            }
