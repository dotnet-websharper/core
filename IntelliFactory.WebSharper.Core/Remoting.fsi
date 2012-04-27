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

/// Implements server-side remote procedure call support.
module IntelliFactory.WebSharper.Core.Remoting

module A = IntelliFactory.WebSharper.Core.Attributes
module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Reflection

/// Represents the response.
type Response =
    {
        Content : string
        ContentType : string
    }

/// Represents read-only access to HTTP headers.
type Headers = string -> option<string>

/// Represents an incoming request.
type Request =
    {
        Body : string
        Headers : Headers
    }

/// Tests if the given request is marked as a
/// WebSharper remote procedure call request.
val IsRemotingRequest : Headers -> bool

/// Constructs RPC handlers.
type IHandlerFactory =

    /// Creates a new handler based on its type.
    abstract member Create : System.Type -> option<obj>

/// Sets the default RPC handler factory.
val SetHandlerFactory : IHandlerFactory -> unit

/// Handles remote procedure call requests.
[<Sealed>]
type Server =

    /// Creates a new instance.
    static member Create : option<IHandlerFactory> -> M.Info -> Server

    /// Handles a request.
    member HandleRequest : Request -> Async<Response>
