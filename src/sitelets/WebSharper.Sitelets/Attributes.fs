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
type private A = System.Attribute
type private T = System.AttributeTargets
type private U = System.AttributeUsageAttribute

/// Indicates the URL fragment parsed by this union case.
/// Example: type Action = | [<EndPoint "/article">] GetArticle
[<Sealed; U(T.Class ||| T.Property)>]
type EndPointAttribute(endpoint: string) =
    inherit A()

    member this.EndPoint = endpoint

/// Indicates that a union case in an action type must only be mapped
/// for requests that use the given HTTP method(s).
/// Example: type Action = | [<Method "POST">] MyPostAction
[<Sealed; U(T.Property, AllowMultiple = true)>]
type MethodAttribute([<ParamArray>] methodName: string[]) =
    inherit A()

/// Indicates that a field or a union case argument must be parsed
/// from the request body as JSON, rather than from the URL path.
[<Sealed; U(T.Property ||| T.Field, AllowMultiple = false)>]
type JsonAttribute =
    inherit A

    /// Indicates that a field must be parsed from the request's body as JSON.
    /// Example: type Action = { [<Json>] data : MyData }
    new() = { inherit A() }

    /// Indicates that the union case argument with the given name must be parsed
    /// from the request's body as JSON.
    /// Example: type Action = | [<Json "data">] MyAction of data: MyData
    new(argumentName: string) = { inherit A() }

/// Indicates that a field or union case argument must be parsed
/// from the request's query parameters, rather than from the URL path.
/// The value must be a primitive value, a DateTime, or an option thereof.
[<Sealed; U(T.Property ||| T.Field, AllowMultiple = true)>]
type QueryAttribute =
    inherit A

    /// Indicates that a field must be parsed from the request's query parameters.
    /// Example: type Action = { [<Query>] someField : string }
    new() = { inherit A() }

    /// Indicates that the union case arguments with the given names must be parsed
    /// from the request's query parameters.
    /// Example: type Action = | [<Query "someField">] MyAction of someField: string
    new([<ParamArray>] argumentName: string[]) = { inherit A() }

/// Indicates that a field or union case argument must be parsed
/// from the request's body in form post syntax, ie. with the Content-Type
/// being either application/x-www-form-urlencoded or multipart/form-data.
/// The value must be a primitive value, a DateTime, or an option thereof.
[<Sealed; U(T.Property ||| T.Field, AllowMultiple = true)>]
type FormDataAttribute =
    inherit A

    /// Indicates that a field must be parsed from the request's query parameters.
    /// Example: type Action = { [<FormData>] someField: string }
    new() = { inherit A() }

    /// Indicates that the union case arguments with the given names must be parsed
    /// from the request's query parameters.
    /// Example: type Action = | [<FormData "someField">] MyAction of someField: string
    new([<ParamArray>] argumentName: string[]) = { inherit A() }

/// Indicates that the last field or union case argument parses all the remaining
/// path segments into a list or an array.
/// Example: type Action = | [<Wildcard>] MyAction of string * list<string>
[<Sealed; U(T.Property ||| T.Class ||| T.Field)>]
type WildcardAttribute() =
    inherit A()
