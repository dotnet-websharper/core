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

[<RequireQualifiedAccess>]
type Fragment =
    | Constant of string
    | Argument of string

type EndPointParams =
    {
        Methods: string list
        InitialFragment : string
        Fragments : Fragment list
        QueryParameters: string list
    }

    static member Parse (s: string) =
        let methods, uri =
            match s.IndexOf '/' with
            | -1 -> [], s
            | i ->
                let methods =
                    s.[.. i - 1].Split([|','; ' '|],
                        StringSplitOptions.RemoveEmptyEntries)
                let uri = s.[i + 1 ..]
                List.ofArray methods, uri
        let path, queryParams =
            match uri.IndexOf '?' with
            | -1 -> uri, []
            | i ->
                let path = uri.[.. i - 1]
                let queryParams =
                    uri.[i + 1 ..].Split([|'&'|],
                        StringSplitOptions.RemoveEmptyEntries)
                path, List.ofArray queryParams
        let fragments =
            path.Split([|'/'|],
                StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun f ->
                if f.StartsWith ":" then
                    Fragment.Argument f.[1..]
                elif f.StartsWith "{" && f.EndsWith "}" then
                    Fragment.Argument f.[1..f.Length-2]
                else Fragment.Constant f)
            |> List.ofArray
        let initialFragment, fragments =
            match fragments with
            | [] -> "", []
            | Fragment.Constant f :: fs -> f, fs
            | Fragment.Argument _ :: fs ->
                failwithf "Invalid EndPoint '%s': the path must start with a constant fragment" s
        {
            Fragments = fragments
            InitialFragment = initialFragment
            Methods = methods
            QueryParameters = queryParams
        }

/// Indicates the URL fragment parsed by this union case,
/// optionally along with accepted methods and query parameters.
/// Example: type Action = | [<EndPoint "GET /article?id&slug">] GetArticle of id: string * slug: string
[<Sealed; U(T.Property, AllowMultiple = false)>]
type EndPointAttribute private (p: EndPointParams) =
    inherit A()

    member this.InitialFragment = p.InitialFragment
    member this.Fragments = p.Fragments
    member this.Methods = p.Methods
    member this.QueryParameters = p.QueryParameters

    new(endpoint: string) =
        new EndPointAttribute(EndPointParams.Parse endpoint)

/// Indicates that a union case in an action type must only be mapped
/// for requests that use the given HTTP method(s).
/// Example: type Action = | [<Method "POST">] MyPostAction
[<Sealed; U(T.Property, AllowMultiple = true)>]
type MethodAttribute([<ParamArray>] methodName: string[]) =
    inherit A()

    member internal this.Methods = List.ofArray methodName

/// Indicates that a field or a union case argument must be parsed
/// from the request body as JSON, rather than from the URL path.
[<Sealed; U(T.Property, AllowMultiple = false)>]
type JsonAttribute
    /// Indicates that the union case argument with the given name must be parsed
    /// from the request's body as JSON.
    /// Example: type Action = | [<Json "data">] MyAction of data: MyData
    (argumentName: string) =
    inherit A()

    member internal this.JsonParameter = argumentName

    /// Indicates that a field must be parsed from the request's body as JSON.
    /// Example: type Action = { [<Json>] data : MyData }
    new() = new JsonAttribute("")

/// Indicates that a field or union case argument must be parsed
/// from the request's query parameters, rather than from the URL path.
/// The value must be a primitive value, a DateTime, or an option thereof.
[<Sealed; U(T.Property, AllowMultiple = true)>]
type QueryAttribute private (queryParameters: string list) =
    inherit A()

    member internal this.QueryParameters = queryParameters

    /// Indicates that a field must be parsed from the request's query parameters.
    /// Example: type Action = { [<Query>] someField : string }
    new() =
        new QueryAttribute([])

    /// Indicates that the union case arguments with the given names must be parsed
    /// from the request's query parameters.
    /// Example: type Action = | [<Query "someField">] MyAction of someField: string
    new([<ParamArray>] argumentName: string[]) =
        new QueryAttribute(List.ofArray argumentName)

/// Indicates that a field or union case argument must be parsed
/// from the request's body in form post syntax, ie. with the Content-Type
/// being either application/x-www-form-urlencoded or multipart/form-data.
/// The value must be a primitive value, a DateTime, or an option thereof.
[<Sealed; U(T.Property, AllowMultiple = true)>]
type FormDataAttribute private (formParameters: string list) =
    inherit A()

    member internal this.FormParameters = formParameters

    /// Indicates that a field must be parsed from the request's query parameters.
    /// Example: type Action = { [<FormData>] someField: string }
    new() =
        new FormDataAttribute([])

    /// Indicates that the union case arguments with the given names must be parsed
    /// from the request's query parameters.
    /// Example: type Action = | [<FormData "someField">] MyAction of someField: string
    new([<ParamArray>] argumentName: string[]) =
        new FormDataAttribute(List.ofArray argumentName)

/// Indicates that the last field or union case argument parses all the remaining
/// path segments into a list or an array.
/// Example: type Action = | [<Wildcard>] MyAction of string * list<string>
[<Sealed; U(T.Property ||| T.Class)>]
type WildcardAttribute() =
    inherit A()
