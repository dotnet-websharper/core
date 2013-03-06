// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Implements the client-side part of remote procedure call support.
module IntelliFactory.WebSharper.Remoting

module R = IntelliFactory.WebSharper.Core.Remoting

/// The URL to the remoting endpoint. Defaults to the current URL:
/// the server that serves the application usually also handles
/// remoting requests. In mobile scenarios this property is set to
/// the deployed address of the server application.
val mutable EndPoint : string

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

/// Calls a remote method asynchronously.
val private Async : string -> obj [] -> Async<obj>

/// Calls a remote method synchronously.
val private Call : string -> obj [] -> obj

/// Calls a remote method asynchronously and ignores the response.
val private Send : string -> obj [] -> unit

val private ajax : bool -> Url -> Headers -> Data -> (Data -> unit) ->
    (exn -> unit) -> unit
val private makeHeaders : string -> obj
val private makePayload : obj [] -> string
val private ( ?<- ) : obj -> string -> obj -> unit
