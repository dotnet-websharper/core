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

/// Implements the client-side part of remote procedure call support.
module WebSharper.Remoting

module R = WebSharper.Core.Remoting

/// The URL to the remoting endpoint. Defaults to the current URL:
/// the server that serves the application usually also handles
/// remoting requests. In mobile scenarios this property is set to
/// the deployed address of the server application.
val mutable EndPoint : string

/// Set the Endpoint to use HTTPS even if the current page was
/// server via HTTP.
/// Return true if the endpoint needed to be changed, false if
/// the current page is already HTTPS.
val UseHttps : unit -> bool

/// Represents URLs.
type Url = string

/// Represents headers as a JavaScript object with string values.
type Headers = obj

/// Represents data in client-server communication.
type Data = string

/// Defines an implementation of AJAX - HTTP POST request client.
type IAjaxProvider =

    /// Performs an async POST request to a given URL.
    abstract member Async : Url -> Headers -> Data ->
                            (Data -> unit) -> (exn -> unit) -> unit

    /// Performs an sync POST request to a given URL.
    abstract member Sync : Url -> Headers -> Data -> Data

/// The IAjaxProvider used by the library. Defaults to an
/// XMLHttpRequest implementation. Can be reset.
val mutable AjaxProvider : IAjaxProvider

/// This interface only exists for documentation purposes.
/// A remoting provider must implement its members as *static* members.
/// (see AjaxRemotingProvider)
type IRemotingProvider =

    /// Calls a remote method synchronously.
    abstract member Sync : string -> obj[] -> obj

    /// Calls a remote method asynchronously.
    abstract member Async : string -> obj[] -> Async<obj>

    /// Calls a remote method asynchronously.
    abstract member Task : string -> obj[] -> System.Threading.Tasks.Task<obj>

    /// Calls a remote method asynchronously and ignores the response.
    abstract member Send : string -> obj[] -> unit

/// Implements remote method calls via AJAX.
[<Class>]
type AjaxRemotingProvider =
    abstract EndPoint : string
    interface IRemotingProvider

val private ajax : bool -> Url -> Headers -> Data -> (Data -> unit) ->
    (exn -> unit) -> (unit -> unit) -> unit
val private makeHeaders : string -> obj
val private makePayload : obj [] -> string
val private ( ?<- ) : obj -> string -> obj -> unit
