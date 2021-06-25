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

namespace WebSharper.Web

open System
open System.IO
open System.Security.Cryptography
open System.Security.Principal
module R = WebSharper.Core.Remoting

module private RpcUtil =
    let [<Literal>] HttpContextKey = "HttpContext"
    let [<Literal>] CsrfTokenKey = "csrftoken"
    let [<Literal>] CsrfTokenHeader = "x-" + CsrfTokenKey

type CorsAndCsrfCheckResult =
    | Ok of headers : list<string * string>
    | Preflight of headers : list<string * string>
    | Error of httpStatusCode: int * httpStatusMessage: string * responseText: string

[<Sealed>]
type RpcHandler() =

    static let corsAndCsrfCheck (reqMethod: string) (reqUrl: Uri) (getCookie: string -> option<string>) (getHeader: string -> option<string>) (setInfiniteCookie: string -> string -> unit) =
        let checkCsrf() =
            match getCookie RpcUtil.CsrfTokenKey with
            | None ->
                setInfiniteCookie RpcHandler.CsrfTokenKey (RpcHandler.MakeCsrfCookie())
                false
            | Some c ->
                match getHeader RpcUtil.CsrfTokenHeader with
                | None -> false
                | Some h ->
                    // The CSRF token is a Base64 string,
                    // so potential double percent-decoding is not an issue.
                    System.Uri.UnescapeDataString h = System.Uri.UnescapeDataString c
        let isSameAuthority origin =
            match Uri.TryCreate(origin, System.UriKind.Absolute) with
            | true, origin -> origin.Authority = reqUrl.Authority
            | false, _ -> false
        let origin = getHeader "Origin"
        let explicitlyAcceptedOrigin =
            match origin with
            | Some origin when isSameAuthority origin || Remoting.allowedOrigins.Contains (origin.ToLowerInvariant()) -> Some origin
            | _ -> None
        let acceptedOrigin =
            if Remoting.allowedOrigins.Contains "*" then Some "*" else explicitlyAcceptedOrigin
        let headers =
            match acceptedOrigin with
            | Some origin ->
                (if origin = "*" then [] else ["Vary", "Origin"])
                @ [
                    "Access-Control-Allow-Origin", origin
                    "Access-Control-Allow-Credentials", "true"
                ]
            | _ -> []
        match reqMethod with
        | "OPTIONS" ->
            ("Access-Control-Allow-Headers", "x-websharper-rpc, content-type, x-csrftoken")
            :: headers
            |> Preflight
        | _ when Remoting.csrfProtect && not (explicitlyAcceptedOrigin.IsSome || checkCsrf()) ->
            Error (403, "Forbidden", "CSRF")
        | _ ->
            Ok headers

    static member CsrfTokenKey = RpcUtil.CsrfTokenKey

    static member MakeCsrfCookie() =
        use rng = new RNGCryptoServiceProvider()
        let bytes = Array.zeroCreate 32
        rng.GetBytes(bytes)
        Convert.ToBase64String(bytes)
            // Avoid issues with HTTP special characters; see https://tools.ietf.org/html/rfc4648#section-5
            .Replace('+', '-')
            .Replace('/', '_')

    static member CorsAndCsrfCheck reqMethod reqUrl getCookie getHeader setInfiniteCookie =
        corsAndCsrfCheck reqMethod reqUrl getCookie getHeader setInfiniteCookie
