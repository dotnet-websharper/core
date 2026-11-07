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

module UrlHelpers =
    open System
    open System.Text.RegularExpressions

    module Internals =
        let matchToken pattern s : (string * string) option =
            let regexp = new Regex("^(" + pattern + ")(.*)")
            let results = regexp.Match s
            if results.Success then
                (results.Groups.[1].Value, results.Groups.[2].Value) |> Some
            else
                None

        let MatchToken s f pattern =
            s |> matchToken pattern |> Option.bind f

        let MatchSymbol s pattern =
            pattern |> MatchToken s (fun (_, rest) -> rest |> Some)

    let (|EOL|_|) s =
        @"$" |> Internals.MatchToken s
            (fun (n, rest) -> Some ())

    let (|SLASH|_|) s =
        @"/" |> Internals.MatchToken s
            (fun (n, rest) -> rest |> Some)

    let (|INT|_|) (s: string) =
        let res = ref 0
        if Int32.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|UINT|_|) (s: string) =
        let res = ref 0u
        if UInt32.TryParse(s, res) then
            !res |> Some
        else
            None
     
    let (|INT64|_|) (s: string) =
        let res = ref 0L
        if Int64.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|UINT64|_|) (s: string) =
        let res = ref 0UL
        if UInt64.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|INT16|_|) (s: string) =
        let res = ref (int16 0)
        if Int16.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|UINT16|_|) (s: string) =
        let res = ref (uint16 0)
        if UInt16.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|INT8|_|) (s: string) =
        let res = ref (int8 0)
        if SByte.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|UINT8|_|) (s: string) =
        let res = ref (uint8 0)
        if Byte.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|SBYTE|_|) (s: string) =
        let res = ref (sbyte 0)
        if SByte.TryParse(s, res) then
            !res |> Some
        else
            None
    
    let (|BYTE|_|) (s: string) =
        let res = ref (byte 0)
        if Byte.TryParse(s, res) then
            !res |> Some
        else
            None
            
    let (|FLOAT|_|) (s: string)=
        let res = ref 0.0
        if Double.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|GUID|_|) (s: string) =
        let res = ref Guid.Empty
        if Guid.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|BOOL|_|) (s: string) =
        let res = ref false
        if Boolean.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|ALPHA|_|) s =
        "[a-zA-Z]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|ALPHA_NUM|_|) s =
        "[a-zA-Z0-9]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|REGEX|_|) regexp input =
        regexp |> Internals.MatchToken input
            (fun (n, rest) -> Some rest)

    let (|SPLIT_BY|_|) (c: char) (uri: Uri) =
        uri.LocalPath.Split c
        |> Array.filter (fun s -> s <> "")
        |> List.ofArray
        |> Some

    let private allParams (req: Http.Request) =
        req.Get.ToList () @ req.Post.ToList ()

    let private ifMethod (m: Http.Method) (req: Http.Request) =
        if req.Method = m then Some (allParams req, req.Uri) else None

    let (|GET|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Get
        | Http.Method.Custom "GET" -> Some (req.Get.ToList (), req.Uri)
        | _ -> None

    let (|POST|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Post
        | Http.Method.Custom "POST" -> Some (req.Post.ToList (), req.Uri)
        | _ -> None

    let (|PUT|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Put
        | Http.Method.Custom "PUT" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|DELETE|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Delete
        | Http.Method.Custom "DELETE" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|OPTIONS|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Options
        | Http.Method.Custom "OPTIONS" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|TRACE|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Trace
        | Http.Method.Custom "TRACE" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|QUERY|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Query
        | Http.Method.Custom "QUERY" -> Some (req.Post.ToList (), req.Uri)
        | _ -> None

    let (|HEAD|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Head
        | Http.Method.Custom "HEAD" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|CONNECT|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Connect
        | Http.Method.Custom "CONNECT" -> Some (allParams req, req.Uri)
        | _ -> None

    let (|CUSTOM|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Custom ("GET" | "POST" | "PUT" | "DELETE" | "OPTIONS" | "TRACE" | "HEAD" | "CONNECT" | "QUERY") -> None
        | Http.Method.Custom _ -> Some (req.Method, (fun () -> allParams req), req.Uri)
        | _ -> None
