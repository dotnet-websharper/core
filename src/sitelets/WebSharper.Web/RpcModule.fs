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
open System.Security.Principal
module R = WebSharper.Core.Remoting

type CorsAndCsrfCheckResult =
    | Ok of headers : list<string * string>
    | Preflight of headers : list<string * string>
    | Error of httpStatusCode: int * httpStatusMessage: string * responseText: string

/// Cross-site request forgery (CSRF) protection based on the Sec-Fetch-Site and Origin headers,
/// which are set by the browser and cannot be forged by scripts
/// (same approach as Go's net/http CrossOriginProtection).
[<Sealed>]
type CsrfCheck =

    /// Checks a request for cross-site request forgery.
    /// Returns the Origin header if it is trusted (accepted by isAllowedOrigin, or the same origin as the request),
    /// and whether the request is safe to execute: it is same-origin, from a trusted origin, or not from a browser.
    static member Check (reqMethod: string, reqUrl: Uri, getHeader: string -> option<string>, isAllowedOrigin: string -> bool) =
        let isSameAuthority origin =
            match Uri.TryCreate(origin, System.UriKind.Absolute) with
            | true, origin -> reqUrl.IsAbsoluteUri && origin.Authority = reqUrl.Authority
            | false, _ -> false
        let origin = getHeader "Origin"
        // Sent by browsers to HTTPS and localhost targets. Unlike comparing Origin to Host, it is scheme-aware
        // and reliable behind proxies rewriting Host or with Referrer-Policy: no-referrer, which makes Origin "null".
        let fetchSite = getHeader "Sec-Fetch-Site"
        let isSameOriginFetch =
            match fetchSite with
            | Some ("same-origin" | "none") -> true
            | _ -> false
        let explicitlyAcceptedOrigin =
            match origin with
            | Some origin when isAllowedOrigin origin -> Some origin
            // If the browser says it's cross-origin, a matching host is not enough: an http page calling https is not trusted.
            | Some origin when (fetchSite.IsNone || isSameOriginFetch) && isSameAuthority origin -> Some origin
            | _ -> None
        let isCsrfSafe =
            if isSameOriginFetch then true
            else
                match origin with
                | Some _ -> explicitlyAcceptedOrigin.IsSome
                // Browsers send Origin on all requests except GET and HEAD, so this is not a browser.
                // A cross-site GET (for example an image) to plain HTTP has neither header, so it is not trusted.
                | None -> fetchSite.IsNone && reqMethod <> "GET" && reqMethod <> "HEAD"
        explicitlyAcceptedOrigin, isCsrfSafe

[<Sealed>]
type RpcHandler() =

    static let corsAndCsrfCheck (reqMethod: string) (reqUrl: Uri) (getHeader: string -> option<string>) =
        let explicitlyAcceptedOrigin, isCsrfSafe =
            CsrfCheck.Check(reqMethod, reqUrl, getHeader, fun origin -> Remoting.allowedOrigins.Contains (origin.ToLowerInvariant()))
        let headers =
            match explicitlyAcceptedOrigin with
            | Some origin ->
                [
                    "Vary", "Origin"
                    "Access-Control-Allow-Origin", origin
                    "Access-Control-Allow-Credentials", "true"
                ]
            | None when Remoting.allowedOrigins.Contains "*" ->
                // Browsers reject credentials with "*", and it does not pass the CSRF check either
                [ "Access-Control-Allow-Origin", "*" ]
            | None -> []
        match reqMethod with
        | "OPTIONS" ->
            // Allow custom headers set by the client, for example an Authorization header in AjaxRemotingProvider.Headers.
            // x-csrftoken is no longer sent, but is still accepted for clients built with earlier WebSharper versions.
            let allowHeaders =
                match getHeader "Access-Control-Request-Headers" with
                | Some h when not (String.IsNullOrWhiteSpace h) -> h
                | _ -> "content-type, x-csrftoken"
            ("Access-Control-Allow-Headers", allowHeaders)
            :: headers
            |> Preflight
        | _ when Remoting.csrfProtect && not isCsrfSafe ->
            Error (403, "Forbidden", "CSRF")
        | _ ->
            Ok headers

    static member CorsAndCsrfCheck reqMethod reqUrl getHeader =
        corsAndCsrfCheck reqMethod reqUrl getHeader
