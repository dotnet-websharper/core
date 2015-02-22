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

/// Manages resources, such as dependent JavaScript and CSS files.
module WebSharper.Core.Resources

open System
open System.Web
open System.Web.UI
module R = WebSharper.Core.Reflection

type MediaType =
    | Css
    | Js

/// Defines how to render a resource.
type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

    member Emit : HtmlTextWriter * MediaType * ?defaultToHttp: bool -> unit

/// Defines the context in which resources can be rendered.
type Context =
    {
        /// A flag indicating if debugging is enabled or not.
        DebuggingEnabled : bool

        /// Transform foreign links to starting with `//` to `http://`.
        DefaultToHttp : bool

        /// Constructs URLs to JavaScript-compiled assemblies.
        /// Assembly names are short, such as FSharp.Core.
        GetAssemblyRendering : R.AssemblyName -> Rendering

        /// Provides a configuration settings collection.
        GetSetting : string -> option<string>

        /// Constructs URLs to point to embedded resources.
        GetWebResourceRendering : Type -> string -> Rendering
    }

/// An interface for resource-defining types.
type IResource =

    /// Renders the resource to a given TextWriter.
    abstract member Render : Context -> HtmlTextWriter -> unit

/// A helper base class for resource-defining types.
[<AbstractClass>]
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

/// Represents the runtime library resource required by all WebSharper code.
[<Sealed>]
type Runtime =
    new : unit -> Runtime
    interface IResource
