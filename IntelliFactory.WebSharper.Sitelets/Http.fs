// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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
            Files : seq<HttpPostedFile>
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
        static member Unauthorized = notFound

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
