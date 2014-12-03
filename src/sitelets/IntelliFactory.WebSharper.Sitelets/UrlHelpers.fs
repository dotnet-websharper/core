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

    let (|FLOAT|_|) (s: string)=
        let res = ref 0.0
        if Double.TryParse(s, res) then
            !res |> Some
        else
            None

    let (|ALPHA|_|) s =
        "[a-zA-Z]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|ALPHA_NUM|_|) s =
        "[a-zA-Z0-9]+" |> Internals.MatchToken s (fun res -> res |> Some)

    let (|REGEX|_|) r s =
        r |> Internals.MatchToken s
            (fun (n, rest) -> Some rest)

    let (|SPLIT_BY|_|) (c: char) (uri: Uri) =
        uri.LocalPath.Split c
        |> Array.filter (fun s -> s <> "")
        |> List.ofArray
        |> Some

    let (|DELETE|GET|OPTIONS|POST|PUT|TRACE|SPECIAL|) (req: Http.Request) =
        let allParams () = req.Get.ToList () @ req.Post.ToList ()
        match req.Method with
        | Http.Method.Delete ->
            DELETE (allParams (), req.Uri)
        | Http.Method.Get
        | Http.Method.Custom "GET" ->
            GET (req.Get.ToList (), req.Uri)
        | Http.Method.Options ->
            OPTIONS (allParams (), req.Uri)
        | Http.Method.Post ->
            POST (req.Post.ToList (), req.Uri)
        | Http.Method.Put ->
            PUT (allParams (), req.Uri)
        | Http.Method.Trace ->
            TRACE (allParams (), req.Uri)
        // TODO: Revise.  Unfortunately, F# active patterns only allow up to 7 cases.
        | Http.Method.Head
        | Http.Method.Connect
        | Http.Method.Custom _ ->
            SPECIAL (req.Method, allParams, req.Uri)

