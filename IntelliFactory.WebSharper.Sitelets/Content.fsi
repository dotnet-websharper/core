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

/// Represents server responses to actions. The Page response is special-cased
/// for combinators to have access to it.
type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | PageContent of (Context<'Action> -> Page)

/// Provides combinators for modifying content.
module Content =

    /// Generates an HTTP response.
    val ToResponse<'T> : Content<'T> -> Context<'T> -> Http.Response

    /// Eliminates the PageContent case. This member is obsolete.
    /// Use ToResponse instead.
    [<Obsolete>]
    val ToCustomContent<'T> : Content<'T> -> Content<'T>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponse<'T> : (Http.Response -> Http.Response) -> Content<'T> -> Content<'T>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeaders<'T> : seq<Http.Header> -> Content<'T> -> Content<'T>

    /// Sets the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    val SetStatus<'T> : status: Http.Status -> Content<'T> -> Content<'T>

    /// Redirects to a given action.
    val Redirect<'T> : action: 'T -> Content<'T>

    /// Redirects to a given URL.
    val RedirectToUrl : url: string -> Content<'T>

    /// Constructs a 401 Unauthorized response.
    val Unauthorized<'T> : Content<'T>

    /// Constructs a 403 Forbidden response.
    val Forbidden<'T> : Content<'T>

    /// Constructs a 404 Not Found response.
    val NotFound<'T> : Content<'T>

    /// Constructs a 500 Server Error response.
    val ServerError<'T> : Content<'T>
