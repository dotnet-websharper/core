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

namespace IntelliFactory.WebSharper.Sitelets

/// Defines HTTP-related functionality.
module Http =
    open System
    open System.Collections.Generic
    open System.Collections.Specialized
    open System.IO
    open System.Web
    open System.Web.UI

    /// Represents HTTP methods.
    /// See: http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.
    type Method =
        | Get
        | Post
        | Connect
        | Delete
        | Head
        | Options
        | Put
        | Trace
        | Custom of string

        override this.ToString() =
            match this with
            | Get -> "GET"
            | Post -> "POST"
            | Connect -> "CONNECT"
            | Delete -> "DELETE"
            | Head -> "HEAD"
            | Options -> "OPTIONS"
            | Put -> "PUT"
            | Trace -> "TRACE"
            | Custom s -> s

        static member OfString (s: string) =
            match s with
            | "GET" -> Get
            | "POST" -> Post
            | "CONNECT" -> Connect
            | "DELETE" -> Delete
            | "HEAD" -> Head
            | "OPTIONS" -> Options
            | "PUT" -> Put
            | "TRACE" -> Trace
            | s -> Custom s

    /// Represents the HTTP version for responses and requests.
    type Version =
        private {
            Version : float
        }

        /// Constructs a new custom version.
        static member Custom x = { Version = x }

    [<AutoOpen>]
    module private VersionUtils =
        let defaultVersion = Version.Custom 1.1

    type Version with

        /// The default version (1.1).
        static member Default = defaultVersion

    /// Represents HTTP headers.
    /// May later add active patterns and constructors
    /// for common headers.
    type Header =
        private {
            mutable name : string
            mutable value : string
        }

        member h.Name
            with get () = h.name
            and set n  = h.value <- n

        member h.Value
            with get () = h.value
            and set v  = h.value <- v

        static member Custom n v =
            { name = n; value = v }

    /// Represents parameter collections.
    [<Sealed>]
    type ParameterCollection(nameValues: NameValueCollection) =

        /// Creates a new parameter collection from s sequence of name and value pairs.
        new (nvs) =
            let nv = NameValueCollection()
            nvs
            |> Seq.iter (fun (n, v) -> nv.Add(n,v))
            ParameterCollection(nv)

        /// Get the entry with the specified key.
        // If the no entry is found the value none is returned.
        member pc.Item(name:string) =
            if nameValues.[name] = null then None else
                Some nameValues.[name]

        /// Transforms the parameter list into a list of
        /// name and value pairs.
        member pc.ToList() =
            seq {
                for k in nameValues.Keys do
                   yield (k, nameValues.[k])
            }
            |> Seq.toList

    /// Represents HTTP requests.
    type Request =
        {
            Method : Method
            Uri : System.Uri
            Headers : seq<Header>
            Post : ParameterCollection
            Get : ParameterCollection
            Cookies : HttpCookieCollection
            ServerVariables : ParameterCollection
            Body : Stream
            Files : seq<HttpPostedFileBase>
        }

    /// Represents the status of HTTP responses.
    /// TODO
    type Status =
        private {
            SCode : int
            SMessage : string
        }

        override st.ToString() =
            String.Format("{0} {1}", st.SCode, st.SMessage)

    [<AutoOpen>]
    module private StatusUtils =

        let def code message =
            { SMessage = message; SCode = code }

        let forbidden = def 403 "Forbidden"
        let internalServerError = def 500 "Internal Error"
        let notFound = def 404 "Not Found"
        let ok = def 200 "Ok"
        let unauthorized = def 401 "Unauthorized"
        let methodNotAllowed = def 405 "Method Not Allowed"

    type Status with

        /// The integer status code, such as 200.
        member st.Code = st.SCode

        /// 403 Forbidden.
        static member Forbidden = forbidden

        /// 500 Internal Error.
        static member InternalServerError = internalServerError

        /// 404 Not Found.
        static member NotFound = notFound

        /// 200 Ok.
        static member Ok = ok

        /// 401 Unauthorized.
        static member Unauthorized = unauthorized

        /// 405 Method Not Allowed.
        static member MethodNotAllowed = methodNotAllowed

        /// Custom status with an integer code and optional message.
        static member Custom n s =
            let m =
                match s with
                | Some s -> s
                | None -> ""
            def n m

    //// Represents HTTP responses.
    type Response =
        {
            Status : Status
            Headers : seq<Header>
            WriteBody : Stream -> unit
        }
