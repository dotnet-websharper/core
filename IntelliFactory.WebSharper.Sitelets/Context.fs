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

/// Provides services available to handlers at run-time.
type Context<'Action> =
    {

        /// Application path
        ApplicationPath : string

        /// Generates a (possibly relative) URL to a given action.
        Link : 'Action -> string

        /// The typed JSON provider for interacting with the client.
        Json : IntelliFactory.WebSharper.Core.Json.Provider

        /// WebSharper metadata required for serializing controls.
        Metadata : IntelliFactory.WebSharper.Core.Metadata.Info

        // Generates a URL respecting the application path.
        ResolveUrl : string -> string

        /// WebSharper resource rendering context required for resources.
        ResourceContext : IntelliFactory.WebSharper.Core.Resources.Context

        /// HTTP Request object
        Request : Http.Request
    }

