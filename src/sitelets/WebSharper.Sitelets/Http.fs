// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open WebSharper

/// Defines HTTP-related functionality.
module Http =
    open System
    open System.Collections.Generic
    open System.Collections.Specialized
    open System.IO
    open System.Threading.Tasks

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
    [<AllowNullLiteral>]
    [<JavaScript false>]
    type ParameterCollection =
        abstract Item: string -> option<string> 
        abstract ToList: unit -> list<string * string> 

    let EmptyParameters =
        { new ParameterCollection with
            member this.Item _ = None
            member this.ToList() = []
        }
    
    let ParametersFromNameValues (nv: NameValueCollection) =
        { new ParameterCollection with
            member this.Item(name:string) =
                Option.ofObj nv.[name]
            member this.ToList() =
                [
                    for k in nv do
                        yield (k, nv.[k])
                ]    
        }

    let ParametersFromMap (m: Map<string, string>) =
        { new ParameterCollection with
            member this.Item(name:string) =
                m |> Map.tryFind name
            member this.ToList() =
                Map.toList m
        }

    [<JavaScript false>]
    type IPostedFile =
        abstract Key : string
        abstract ContentLength : int
        abstract ContentType : string
        abstract FileName : string
        abstract InputStream : Stream
        abstract SaveAs : filename: string -> unit

    /// Represents HTTP requests.
    [<AbstractClass>]
    type Request() =
        abstract Method : Method 
        abstract Uri : System.Uri 
        abstract Headers : seq<Header> 
        abstract Post : ParameterCollection 
        abstract Get : ParameterCollection 
        abstract ServerVariables : ParameterCollection 
        abstract Body : Stream
        abstract Files : seq<IPostedFile>
        abstract Cookies : ParameterCollection
        abstract BodyText : Task<string>
        abstract IsBodyTextCompleted : bool
        
        member this.WithUri(uri) =
            match this with
            | :? RequestWithUri as req ->
                RequestWithUri(req.Original, uri) :> Request   
            | _ ->
                RequestWithUri(this, uri) :> Request   

        member this.BodyTextAsync =
            this.BodyText |> Async.AwaitTask

        static member Empty (uri) =
            { new Request() with
                override x.Method = Method.Get
                override x.Uri = Uri(uri, UriKind.Relative)
                override x.Headers = Seq.empty
                override x.Post = EmptyParameters
                override x.Get = EmptyParameters
                override x.ServerVariables = EmptyParameters
                override x.Body = Stream.Null
                override x.Files = Seq.empty
                override x.Cookies = EmptyParameters
                override x.BodyText = Task.FromResult ""
                override x.IsBodyTextCompleted = true
            }

    // optimized wrapper around Request, used for IRouter.Shift
    and private RequestWithUri(req, uri: System.Uri) =
        inherit Request()
        member this.Original = req
        override x.Method = req.Method
        override x.Uri = uri
        override x.Headers = req.Headers
        override x.Post = req.Post
        override x.Get = req.Get
        override x.ServerVariables = req.ServerVariables
        override x.Body = req.Body
        override x.Files = req.Files
        override x.Cookies = req.Cookies
        override x.BodyText = req.BodyText
        override x.IsBodyTextCompleted = req.IsBodyTextCompleted

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
        let notImplemented = def 501 "Not Implemented"
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

        /// 501 Not Implemented.
        static member NotImplemented = notImplemented

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

    type ResponseBody = 
        | EmptyBody
        | WriteBody of (Stream -> unit)
        | WriteBodyAsync of (Stream -> Task)
        | MvcBody of obj

    //// Represents HTTP responses.
    type Response =
        {
            Status : Status
            Headers : seq<Header>
            WriteBody : ResponseBody
        }
