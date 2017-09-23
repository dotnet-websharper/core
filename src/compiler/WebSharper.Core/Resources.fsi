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

/// Manages resources, such as dependent JavaScript and CSS files.
module WebSharper.Core.Resources

open System

type HtmlTextWriter =
#if NET461 // ASP.NET: HtmlTextWriter
    inherit System.Web.UI.HtmlTextWriter
#else
    inherit System.IO.TextWriter
    member RenderBeginTag : string -> unit
    member RenderEndTag : unit -> unit
    member WriteBeginTag : string -> unit
    member WriteFullBeginTag : string -> unit
    member WriteEndTag : string -> unit
    member WriteEncodedText : string -> unit
    member AddAttribute : string * string -> unit
    member WriteAttribute : string * string -> unit
    static member SelfClosingTagEnd : string
    static member TagLeftChar : char
    static member TagRightChar : char
#endif
    new : System.IO.TextWriter -> HtmlTextWriter
    new : System.IO.TextWriter * indent: string -> HtmlTextWriter

val AllReferencedAssemblies : Lazy<list<System.Reflection.Assembly>>

type MediaType =
    | Css
    | Js

type RenderLocation =
    | Scripts
    | Styles
    | Meta

/// Defines how to render a resource.
type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

    member Emit : HtmlTextWriter * MediaType * ?defaultToHttp: bool -> unit
    member Emit : (RenderLocation -> HtmlTextWriter) * MediaType * ?defaultToHttp: bool -> unit
    static member TryGetCdn : ctx: Context * assemblyName: string * filename: string -> option<Rendering>
    static member TryGetCdn : ctx: Context * assembly: System.Reflection.Assembly * filename: string -> option<Rendering>
    static member GetWebResourceRendering : ctx: Context * resource: System.Type * filename: string -> Rendering
    static member RenderCached : ctx: Context * resource: IResource * getWriter : (RenderLocation -> HtmlTextWriter) -> unit

/// Defines the context in which resources can be rendered.
and Context =
    {
        /// A flag indicating if debugging is enabled or not.
        DebuggingEnabled : bool

        /// Transform foreign links to starting with `//` to `http://`.
        DefaultToHttp : bool

        ///// Gets local resource hash values.
        //GetResourceHash : string * string -> int

        /// Constructs URLs to JavaScript-compiled assemblies.
        /// Assembly names are short, such as FSharp.Core.
        GetAssemblyRendering : string -> Rendering

        /// Provides a configuration settings collection.
        GetSetting : string -> option<string>

        /// Constructs URLs to point to embedded resources.
        GetWebResourceRendering : Type -> string -> Rendering

        /// Cache for resolved rendering of resources.
        RenderingCache : System.Collections.Concurrent.ConcurrentDictionary<IResource, (RenderLocation -> HtmlTextWriter) -> unit>

        /// Cache for resolved dependency lookups.
        ResourceDependencyCache : System.Collections.Concurrent.ConcurrentDictionary<Metadata.Node Set, IResource list>
    }

/// An interface for resource-defining types.
and IResource =

    /// Renders the resource to a given TextWriter.
    abstract member Render : Context -> ((RenderLocation -> HtmlTextWriter) -> unit)

/// A resource value appending nothing.
val EmptyResource : IResource

/// An interface for resources to execute custom unpack.
type IDownloadableResource =

    /// Gets the WebSharper output root directory.
    abstract Unpack : string -> unit    

/// A helper base class for resource-defining types.
type BaseResource =

    /// References an embedded resource from he current assembly.
    /// The string represents either a reference to an embedded
    /// resource from the current assembly, or a (possibly relative)
    /// URL. CSS resources are distinguished from JavaScript resources by
    /// checking for the .css suffix.
    new : string -> BaseResource

    /// References several external resources by specifying the
    /// absolute base URL and one or more relative URLs.
    /// The base URL is made configurable by consulting the application
    /// setting matching the full name of the declaring type.
    /// CSS resources are distinguished from JavaScript resources
    /// by checking for the .css syntax.
    new : string * string * [<ParamArray>] xs: string [] -> BaseResource

    interface IResource
    interface IDownloadableResource

/// Represents the runtime library resource required by all WebSharper code.
[<Sealed>]
type Runtime =
    new : unit -> Runtime
    interface IResource
    static member Instance : IResource
