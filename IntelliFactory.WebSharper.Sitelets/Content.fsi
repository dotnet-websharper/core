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
module XT = IntelliFactory.Xml.Templating

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

    module H = IntelliFactory.Html.Html

    /// HTML template utilities.
    module Template =

        /// Defines how frequently a template should be
        /// loaded from disk.
        type LoadFrequency =

            /// Loading happens once per application start.
            | Once

            /// Loading happens once per every request, which
            /// is useful for development.
            | PerRequest

    /// A type of HTML elements.
    type HtmlElement = H.IElement<Control>

    /// A type of HTML nodes.
    type HtmlNode = H.INode<Control>

    /// <summary>Defines a new page template.  Template files are parsed as XML
    /// and then analyzed for placeholders.  There are text placeholders
    /// <c>${foo}</c> that can appear inside text nodes and attributes, and
    /// node or node-list placeholders such as
    /// <c>&lt;div data-hole="bar"&gt;</c> or <c>&lt;div data-replace="bar"&gt;</c>.
    /// Node placeholders get replaced during
    /// expansion. This mechanism allows to populate placeholders with example
    /// content and validate templates as HTML5 during development.</summary>
    [<Sealed>]
    type Template<'T> =

        /// Constructs a new template from an XML file at a given path.
        new : path: string -> Template<'T>

        /// Constructs a new template from an XML file at a given path,
        /// also specifying the load frequency (defaults to "once").
        new : path: string * freq: Template.LoadFrequency -> Template<'T>

        /// <summary>Adds a text-valued hole accessible in the
        /// template as <c>${name}</c>.</summary>
        member With : hole: string * def: Func<'T,string> -> Template<'T>

        /// <summary>Adds an element-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,#HtmlElement> -> Template<'T>

        /// <summary>Adds an element-list-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,#seq<#HtmlElement>> -> Template<'T>

        /// Compiles the template as a simple template. Recommended to use before Run
        /// for early detection of errors.
        member Compile : unit -> Template<'T>

        /// Expands the template as a simple element template.
        member Run : value: 'T -> HtmlNode

    /// Applies a template as a page template for sitelet content.
    /// An extra placeholder called "scripts" is available with WebSharper-determined
    /// dependencies.
    val WithTemplate<'Action,'T> :
        template: Template<'T> ->
        content: (Context<'Action> -> 'T) ->
        Content<'Action>
