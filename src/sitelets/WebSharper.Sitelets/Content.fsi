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

namespace WebSharper.Sitelets

open System
module H = WebSharper.Html.Server.Html
module XT = IntelliFactory.Xml.Templating

/// Represents server responses to actions. The Page response is special-cased
/// for combinators to have access to it.
type Content<'Action> =
    | [<Obsolete "Use Content.Custom">]
      CustomContent of (Context<'Action> -> Http.Response)
    | [<Obsolete "Use Content.Custom">]
      CustomContentAsync of (Context<'Action> -> Async<Http.Response>)
    | [<Obsolete "Use Content.Page">]
      PageContent of (Context<'Action> -> Page)
    | [<Obsolete "Use Content.Page">]
      PageContentAsync of (Context<'Action> -> Async<Page>)

    /// Creates a JSON content from the given object.
    static member Json : 'U -> Async<Content<'Action>>

    /// Creates an HTML content.
    static member Page
        : ?Body: #seq<H.Element>
        * ?Head: #seq<H.Element>
        * ?Title: string
        * ?Doctype: string
        -> Async<Content<'Action>>

    /// Creates an HTML content.
    static member Page : Page -> Async<Content<'Action>>

    /// Creates an HTML content from an <html> element.
    static member Page : H.Element -> Async<Content<'Action>>

    /// Creates a plain text content.
    static member Text : string * ?encoding: System.Text.Encoding -> Async<Content<'Action>>

    /// Creates a content that serves a file from disk.
    static member File : path: string * ?allowOutsideRootFoolder: bool -> Async<Content<'Action>>

    /// Creates a custom content.
    static member Custom : Http.Response -> Async<Content<'Action>>

    /// Creates a custom content.
    static member Custom
        : ?Status: Http.Status
        * ?Headers: seq<Http.Header>
        * ?WriteBody: (System.IO.Stream -> unit)
        -> Async<Content<'Action>>

/// Provides combinators for modifying content.
module Content =

    /// Generates an HTTP response.
    val ToResponse<'T> : Content<'T> -> Context<'T> -> Async<Http.Response>

    /// Generates an HTTP response.
    [<Obsolete "Use ToResponse">]
    val ToResponseAsync<'T> : Content<'T> -> Context<'T> -> Async<Http.Response>

    /// Wraps an asynchronous content.
    val FromAsync<'T> : Async<Content<'T>> -> Content<'T>

    /// Generates JSON content from the given object.
    [<Obsolete "Use Content.Json">]
    val JsonContent<'T, 'U> : (Context<'T> -> 'U) -> Content<'T>

    /// Generates JSON content from the given object.
    [<Obsolete "Use Content.Json">]
    val JsonContentAsync<'T, 'U> : (Context<'T> -> Async<'U>) -> Content<'T>

    /// Eliminates the PageContent case.
    [<Obsolete "Use ToResponse">]
    val ToCustomContent<'T> : Content<'T> -> Content<'T>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponse<'T> : (Http.Response -> Http.Response) -> Async<Content<'T>> -> Async<Content<'T>>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponseAsync<'T> : (Http.Response -> Async<Http.Response>) -> Async<Content<'T>> -> Async<Content<'T>>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeaders<'T> : seq<Http.Header> -> Async<Content<'T>> -> Async<Content<'T>>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    val SetHeaders<'T> : seq<Http.Header> -> Async<Content<'T>> -> Async<Content<'T>>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    val SetStatus<'T> : status: Http.Status -> Async<Content<'T>> -> Async<Content<'T>>

    /// Set the body writing function of the generated response.
    /// Transforms any content to 'CustomContent'.
    val SetBody<'T> : writeBody: (System.IO.Stream -> unit) -> Async<Content<'T>> -> Async<Content<'T>>

    /// Redirects permanently (301 Moved Permanently) to a given action.
    val RedirectPermanent<'T> : action: 'T -> Async<Content<'T>>

    /// Redirects permanently (301 Moved Permanently) to a given URL.
    val RedirectPermanentToUrl : url: string -> Async<Content<'T>>

    /// Redirects temporarily (307 Redirect Temporary) to a given action.
    val RedirectTemporary<'T> : action: 'T -> Async<Content<'T>>

    /// Redirects temporarily (307 Redirect Temporary) to a given URL.
    val RedirectTemporaryToUrl : url: string -> Async<Content<'T>>

    /// Constructs a 401 Unauthorized response.
    val Unauthorized<'T> : Async<Content<'T>>

    /// Constructs a 403 Forbidden response.
    val Forbidden<'T> : Async<Content<'T>>

    /// Constructs a 404 Not Found response.
    val NotFound<'T> : Async<Content<'T>>

    /// Constructs a 500 Server Error response.
    val ServerError<'T> : Async<Content<'T>>

    /// Constructs a 405 Method Not Allowed response.
    val MethodNotAllowed<'T> : Async<Content<'T>>

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

            /// Loading detects file changes and only happens
            /// when necessary, using System.IO.FileSystemWatcher
            /// to detect changes in the file system.
            | WhenChanged

    /// A type of HTML elements.
    type HtmlElement = H.Element

    /// <summary>Defines a new page template.  Template files are parsed as XML
    /// and then analyzed for placeholders.  There are text placeholders
    /// <c>${foo}</c> that can appear inside text nodes and attributes, and
    /// node or node-list placeholders such as
    /// <c>&lt;div data-hole="bar"&gt;</c> or <c>&lt;div data-replace="bar"&gt;</c>.
    /// Node placeholder elements get completely replaced (data-replace),
    /// or get their contents replaced (data-hole) during expansion.
    /// This mechanism allows to populate placeholders with example
    /// content and validate templates as HTML5 during development.</summary>
    [<Sealed>]
    type Template<'T> =

        /// Constructs a new template from an XML file at a given path.
        new : path: string -> Template<'T>

        /// Constructs a new template from an XML file at a given path,
        /// also specifying the load frequency (defaults to WhenChanged).
        new : path: string * freq: Template.LoadFrequency -> Template<'T>

        /// Create a page from an <html> element.
        static member FromHtmlElement : HtmlElement -> Template<'T>

        /// <summary>Adds a text-valued hole accessible in the
        /// template as <c>${name}</c>.</summary>
        member With : hole: string * def: Func<'T,string> -> Template<'T>

        /// <summary>Adds an element-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,HtmlElement> -> Template<'T>

        /// <summary>Adds an element-list-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,#seq<HtmlElement>> -> Template<'T>

        /// <summary>Adds a text-valued hole accessible in the
        /// template as <c>${name}</c>.</summary>
        member With : hole: string * def: Func<'T,Async<string>> -> Template<'T>

        /// <summary>Adds an element-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,Async<HtmlElement>> -> Template<'T>

        /// <summary>Adds an element-list-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,Async<#seq<HtmlElement>>> -> Template<'T>

        /// Compiles the template as a simple template. Recommended to use before Run
        /// for early detection of errors. Optionally pass the root folder.
        member Compile : ?root: string -> Template<'T>

        /// Expands the template on a given value. Optionally pass the root folder.
        member Run : value: 'T * ?root: string -> seq<HtmlElement>

    /// Asynchronously applies a template as a page template for sitelet content.
    /// Extra placeholders called "scripts", "styles" and "meta" are available
    /// with WebSharper-determined dependencies. If only "scripts" is present,
    /// then it will be filled with meta, styles and scripts, in this order.
    val WithTemplateAsync<'Action,'T> :
        template: Template<'T> ->
        content: Async<'T> ->
        Async<Content<'Action>>

    /// Applies a template as a page template for sitelet content.
    /// Extra placeholders called "scripts", "styles" and "meta" are available
    /// with WebSharper-determined dependencies. If only "scripts" is present,
    /// then it will be filled with meta, styles and scripts, in this order.
    val WithTemplate<'Action,'T> :
        template: Template<'T> ->
        content: 'T ->
        Async<Content<'Action>>
