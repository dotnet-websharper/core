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

/// Manages resources, such as dependent JavaScript and CSS files.
module IntelliFactory.WebSharper.Core.Resources

/// Defines the context in which resources can be rendered.
type Context =
    {
        /// A flag indicating if debugging is enabled or not.
        DebuggingEnabled : bool

        /// Constructs URLs to JavaScript-compiled assemblies.
        /// Assembly names are short, such as FSharp.Core.
        GetAssemblyUrl : Reflection.AssemblyName -> string

        /// Provides a configuration settings collection.
        GetSetting : string -> option<string>

        /// Constructs URLs to point to embedded resources.
        GetWebResourceUrl : System.Type -> string -> string
    }

/// An interface for resource-defining types.
type IResource =

    /// Renders the resource to a given TextWriter.
    abstract member Render : Context -> System.Web.UI.HtmlTextWriter -> unit

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
    new : string * string * [<System.ParamArray>] xs: string [] -> BaseResource

    interface IResource

/// Represents the runtime library resource required by all WebSharper code.
[<Sealed>]
type Runtime =
    new : unit -> Runtime
    interface IResource
