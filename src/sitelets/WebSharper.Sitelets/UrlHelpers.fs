// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
    open System.Globalization
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
        match Int32.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|UINT|_|) (s: string) =
        match UInt32.TryParse(s, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|INT64|_|) (s: string) =
        match Int64.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|UINT64|_|) (s: string) =
        match UInt64.TryParse(s, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|INT16|_|) (s: string) =
        match Int16.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|UINT16|_|) (s: string) =
        match UInt16.TryParse(s, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|INT8|_|) (s: string) =
        match SByte.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|UINT8|_|) (s: string) =
        match Byte.TryParse(s, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|SBYTE|_|) (s: string) =
        match SByte.TryParse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|BYTE|_|) (s: string) =
        match Byte.TryParse(s, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

    let (|FLOAT|_|) (s: string) =
        let style = NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint ||| NumberStyles.AllowExponent
        match Double.TryParse(s, style, CultureInfo.InvariantCulture) with
        | true, res -> Some res
        | _ -> None

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

    let private isMethod (m: Http.Method) (req: Http.Request) =
        match req.Method with
        | Http.Method.Custom name when name = m.ToString() -> Some ()
        | x when x = m -> Some ()
        | _ -> None

    let (|GET|_|) req = isMethod Http.Method.Get req

    let (|POST|_|) req = isMethod Http.Method.Post req

    let (|PUT|_|) req = isMethod Http.Method.Put req

    let (|DELETE|_|) req = isMethod Http.Method.Delete req

    let (|OPTIONS|_|) req = isMethod Http.Method.Options req

    let (|TRACE|_|) req = isMethod Http.Method.Trace req

    let (|QUERY|_|) req = isMethod Http.Method.Query req

    let (|HEAD|_|) req = isMethod Http.Method.Head req

    let (|CONNECT|_|) req = isMethod Http.Method.Connect req

    let (|CUSTOM|_|) (req: Http.Request) =
        match req.Method with
        | Http.Method.Custom ("GET" | "POST" | "PUT" | "DELETE" | "OPTIONS" | "TRACE" | "QUERY" | "HEAD" | "CONNECT") -> None
        | Http.Method.Custom name -> Some name
        | _ -> None

    let (|PATH|) (req: Http.Request) =
        let u = req.Uri
        let p =
            if u.IsAbsoluteUri then
                u.AbsolutePath
            else
                let s = u.OriginalString
                match s.IndexOfAny [| '?'; '#' |] with
                | -1 -> s
                | i -> s.Substring(0, i)
        p.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map Uri.UnescapeDataString
        |> List.ofSeq

    let (|PARAM|_|) (name: string) (req: Http.Request) =
        req.Get.Item name

    let (|FORM|_|) (name: string) (req: Http.Request) =
        req.Post.Item name
