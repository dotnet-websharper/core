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

namespace WebSharper.Web

/// Provides context about the web request being replied to.
type IContext =
    /// The full path to the application's root folder.
    abstract member RootFolder : string

    /// The URI of the web request.
    abstract member RequestUri : System.Uri

    /// Manage user login sessions.
    abstract member UserSession : IUserSession

module Remoting =

    let internal context =
        new System.Threading.ThreadLocal<option<IContext>>(fun () -> None)

    /// Retrieve the current web context in an Rpc function. This function must be called
    /// from the thread from which the Rpc function is originally called. The returned
    /// object can be used throughout the Rpc function.
    let GetContext() =
        match context.Value with
        | None -> failwith "No remoting context available."
        | Some c -> c

[<AutoOpen>]
module RemotingExtensions =

    type WebSharper.Core.Remoting.Server with

        /// Handle a request with the given web context.
        member this.HandleRequest(req, context) =
            Remoting.context.Value <- Some context
            let res = this.HandleRequest(req)
            Remoting.context.Value <- None
            res
