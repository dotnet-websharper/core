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

namespace WebSharper.Sitelets

open System
open WebSharper

/// Represents server responses to actions. The Page response is special-cased
/// for combinators to have access to it.
type Content<'Action> =
    | [<Obsolete "Use Content.Custom">]
      CustomContent of (Context<'Action> -> Http.Response)
    | [<Obsolete "Use Content.Custom">]
      CustomContentAsync of (Context<'Action> -> Async<Http.Response>)

    /// Creates a JSON content from the given object.
    static member Json : 'U -> Async<Content<'Action>>

    /// Creates an HTML content.
    static member Page
        : ?Body: #seq<#Web.INode>
        * ?Head: #seq<#Web.INode>
        * ?Title: string
        * ?Doctype: string
        -> Async<Content<'Action>>

    /// Creates an HTML content.
    static member Page : Page -> Async<Content<'Action>>

    /// Creates a plain text content.
    static member Text : string * ?encoding: System.Text.Encoding -> Async<Content<'Action>>

    /// Creates a content that serves a file from disk.
    static member File : path: string * ?AllowOutsideRootFoolder: bool * ?ContentType: string -> Async<Content<'Action>>

    /// Creates a custom content.
    static member Custom : Http.Response -> Async<Content<'Action>>

    /// Creates a custom content.
    static member Custom
        : ?Status: Http.Status
        * ?Headers: seq<Http.Header>
        * ?WriteBody: (System.IO.Stream -> unit)
        -> Async<Content<'Action>>

/// Provides combinators for modifying content.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =

    /// Creates Content that depends on the Sitelet context.
    val FromContext : (Context<'T> -> Async<Content<'T>>) -> Async<Content<'T>>

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

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponse<'T> : (Http.Response -> Http.Response) -> Async<Content<'T>> -> Async<Content<'T>>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponseAsync<'T> : (Http.Response -> Async<Http.Response>) -> Async<Content<'T>> -> Async<Content<'T>>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeaders<'T> : seq<Http.Header> -> Async<Content<'T>> -> Async<Content<'T>>

    /// Add a header to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeader<'T> : name: string -> value: string -> Async<Content<'T>> -> Async<Content<'T>>

    /// Set the Content-Type header.
    val WithContentType<'T> : string -> Async<Content<'T>> -> Async<Content<'T>>

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

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member Item : string -> string with get

    type Env =
        {
            AppPath : string
            Json : Core.Json.Provider
            Meta : Core.Metadata.Info
            Graph : Core.DependencyGraph.Graph 
            ResourceContext : Core.Resources.Context
        }

        static member Create<'T> : ctx: Context<'T> -> Env

        member GetSeparateResourcesAndScripts : seq<#IRequiresResources> -> RenderedResources

        member GetResourcesAndScripts : seq<#IRequiresResources> -> string

open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<CompiledName "Content"; Sealed; Extension>]
type CSharpContent =

    /// Creates a JSON content from the given object.
    static member Json<'T, 'U>
        : 'U
        -> Task<Content<'T>>

    /// Creates an HTML content.
    static member Page<'T>
        : [<Optional>] Body: Web.INode
        * [<Optional>] Head: Web.INode
        * [<Optional>] Title: string
        * [<Optional>] Doctype: string
        -> Task<Content<'T>>

    /// Creates an HTML content.
    static member Page<'T>
        : Page
        -> Task<Content<'T>>

    /// Creates a plain text content.
    static member Text<'T>
        : string
        * [<Optional>] Encoding: System.Text.Encoding
        -> Task<Content<'T>>

    /// Creates a content that serves a file from disk.
    static member File<'T>
        : path: string
        * [<Optional>] AllowOutsideRootFolder: bool
        * [<Optional>] ContentType: string
        -> Task<Content<'T>>

    /// Creates a custom content.
    static member Custom<'T>
        : Http.Response
        -> Task<Content<'T>>

    /// Creates a custom content.
    static member Custom<'T>
        : [<Optional>] Status: Http.Status
        * [<Optional>] Headers: seq<Http.Header>
        * [<Optional>] WriteBody: Action<IO.Stream>
        -> Task<Content<'T>>

    /// Creates Content that depends on the Sitelet context.
    static member FromContext<'T>
        : Func<Context<'T>, Task<Content<'T>>>
        -> Task<Content<'T>>

    /// Generates an HTTP response.
    [<Extension>]
    static member ToResponse<'T>
        : Content<'T>
        * Context<'T>
        -> Task<Http.Response>

    /// Wraps an asynchronous content.
    static member FromTask<'T>
        : Task<Content<'T>>
        -> Content<'T>

    /// Wraps an asynchronous content.
    static member FromAsync<'T>
        : Async<Content<'T>>
        -> Content<'T>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member MapResponse<'T>
        : Task<Content<'T>>
        * Func<Http.Response, Http.Response>
        -> Task<Content<'T>>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member MapResponseAsync<'T>
        : Task<Content<'T>>
        * Func<Http.Response, Task<Http.Response>>
        -> Task<Content<'T>>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeaders<'T>
        : Task<Content<'T>>
        * seq<Http.Header>
        -> Task<Content<'T>>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeaders<'T>
        : Task<Content<'T>>
        * [<ParamArray>] headers: Http.Header[]
        -> Task<Content<'T>>

    /// Add a header to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeader<'T>
        : Task<Content<'T>>
        * name: string
        * value: string
        -> Task<Content<'T>>

    /// Set the Content-Type header.
    [<Extension>]
    static member WithContentType<'T>
        : Task<Content<'T>>
        * contentType: string
        -> Task<Content<'T>>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member SetHeaders<'T>
        : Task<Content<'T>>
        * seq<Http.Header>
        -> Task<Content<'T>>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member SetHeaders<'T>
        : Task<Content<'T>>
        * [<ParamArray>] headers: Http.Header[]
        -> Task<Content<'T>>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetStatus<'T>
        : Task<Content<'T>>
        * Http.Status
        -> Task<Content<'T>>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetStatus<'T>
        : Task<Content<'T>>
        * code: int
        * [<Optional>] message: string
        -> Task<Content<'T>>

    /// Set the body writing function of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetBody<'T>
        : Task<Content<'T>>
        * Action<IO.Stream>
        -> Task<Content<'T>>

    /// Redirects permanently (301 Moved Permanently) to a given action.
    static member RedirectPermanent<'T>
        : 'T
        -> Task<Content<'T>>

    /// Redirects permanently (301 Moved Permanently) to a given URL.
    static member RedirectPermanentToUrl<'T>
        : string
        -> Task<Content<'T>>

    /// Redirects temporarily (307 Redirect Temporary) to a given action.
    static member RedirectTemporary<'T>
        : 'T
        -> Task<Content<'T>>

    /// Redirects temporarily (307 Redirect Temporary) to a given URL.
    static member RedirectTemporaryToUrl<'T>
        : string
        -> Task<Content<'T>>

    /// Constructs a 401 Unauthorized response.
    static member Unauthorized<'T>
        : unit
        -> Task<Content<'T>>

    /// Constructs a 403 Forbidden response.
    static member Forbidden<'T>
        : unit
        -> Task<Content<'T>>

    /// Constructs a 404 Not Found response.
    static member NotFound<'T>
        : unit
        -> Task<Content<'T>>

    /// Constructs a 500 Server Error response.
    static member ServerError<'T>
        : unit
        -> Task<Content<'T>>

    /// Constructs a 405 Method Not Allowed response.
    static member MethodNotAllowed<'T>
        : unit
        -> Task<Content<'T>>
