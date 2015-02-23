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
type private A = System.Attribute
type private T = System.AttributeTargets
type private U = System.AttributeUsageAttribute

/// Indicates that a union case in an action type must only be mapped
/// for requests that use the given HTTP method(s).
[<Sealed; U(T.Property, AllowMultiple = true)>]
type MethodAttribute([<ParamArray>] methodName: string[]) =
    inherit A()

/// Indicates that a field or a union case argument must be parsed
/// from the request body as JSON, rather than from the URL path.
[<Sealed; U(T.Property, AllowMultiple = false)>]
type JsonAttribute =
    inherit A

    /// Indicates that a field must be parsed from the request's body as JSON.
    new() = { inherit A() }

    /// Indicates that the union case argument with the given name must be parsed
    /// from the request's body as JSON.
    new(argumentName: string) = { inherit A() }

/// Indicates that a field or union case argument must be parsed
/// from the request's query parameters, rather than from the URL path.
/// The value must be a primitive value, a DateTime, or an option thereof.
[<Sealed; U(T.Property, AllowMultiple = true)>]
type QueryAttribute =
    inherit A

    /// Indicates that a field must be parsed from the request's query parameters.
    new() = { inherit A() }

    /// Indicates that the union case arguments with the given names must be parsed
    /// from the request's query parameters.
    new([<ParamArray>] argumentName: string[]) = { inherit A() }
