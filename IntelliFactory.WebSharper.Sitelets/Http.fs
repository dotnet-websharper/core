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

namespace IntelliFactory.WebSharper.Sitelets

open System.Collections.Generic
open System.Web
open System.Web.UI
open System.Collections.Specialized

/// Defines HTTP-related functionality.
module Http =

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
        private {Version : float}

        /// Constructs a new custom version.
        static member Custom x = { Version = x }

        /// Constructs a default version (1.1).
        static member Default = Version.Custom 1.1

    /// Represents HTTP headers. May later add active patterns and constructors
    /// for common headers.
    type Header =
        private
            {
                mutable name : string
                mutable value : string
            }
        member this.Name
            with get () = this.name
            and set n   = this.value <- n

        member this.Value
            with get () = this.value
            and set v   = this.value <- v

        static member Custom n v =
            {name = n; value = v}

    /// Represents parameter collections.
    type ParameterCollection(nameValues: NameValueCollection) =

        /// Creates a new parameter collection from s sequence of name and value pairs.
        new (nvs: seq<string * string>) =
            let nv = new NameValueCollection()
            nvs
            |> Seq.iter (fun (n,v) ->
                nv.Add(n,v)
            )
            ParameterCollection(nv)

        /// Get the entry with the specified key.
        // If the no entry is found the value none is returned.
        member this.Item (name:string) =
            if nameValues.[name] = null then
                None
            else
                Some nameValues.[name]

        /// Transforms the parameter list into a list of
        /// name and value pairs.
        member this.ToList () =
            seq {
                for k in nameValues.Keys do
                   yield (k, nameValues.[k])
            }
            |> Seq.toList

    /// Represents HTTP requests.
    type Request =
        {
            Method  : Method
            Uri     : System.Uri
            Headers : seq<Header>
            Post    : ParameterCollection
            Get     : ParameterCollection
            Cookies : HttpCookieCollection
            ServerVariables : ParameterCollection
            Body    : System.IO.Stream
            Files   : seq<System.Web.HttpPostedFile>
        }

    /// Represents the status of HTTP responses.
    /// TODO
    type Status =
        private
            {
                Message : string;
                Code    : int
            }
        static member Ok = {Message = "OK"; Code = 200}
        static member NotFound = {Message = "Not Found"; Code = 404}
        static member Unauthorized = {Message = "Unauthorized "; Code = 401}
        static member Forbidden = {Message = "Forbidden  "; Code = 403}
        static member InternalServerError = {Message = "Internal Error"; Code = 500}
        static member Custom (n: int) (s: option<string>) =
            let m =
                match s with
                | Some s    -> s
                | None      -> ""
            {Message = m; Code = n}
        override this.ToString() =
            this.Code.ToString() + " " + this.Message

    //// Represents HTTP responses.
    type Response =
        {
            Status      : Status
            Headers     : seq<Header>
            WriteBody   : System.IO.Stream -> unit
        }
