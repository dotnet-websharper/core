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

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Web.UI
open IntelliFactory.WebSharper

type Location = Uri

[<AutoOpen>]
module RouterUtil =

    let dictSum (a: IDictionary<_,_>) (b: IDictionary<_,_>) =
        let d = Dictionary<_,_>(a.Count + b.Count)
        for kv in b do
            d.[kv.Key] <- kv.Value
        for kv in a do
            d.[kv.Key] <- kv.Value
        d :> IDictionary<_,_>

    let inline optFunSum f g x =
        match f x with
        | None -> g x
        | r -> r

    /// Creates an absolute or relative URI from a string.
    let makeUri uri =
        let mutable res = null
        if Uri.TryCreate(uri, UriKind.Relative, &res) then res else
            Uri(uri, UriKind.Absolute)

    let isAbsoluteUri uri =
        (makeUri uri).IsAbsoluteUri

    let path (uri: Uri) =
        if uri.IsAbsoluteUri
        then uri.AbsolutePath
        else Uri.UnescapeDataString(uri.OriginalString) |> joinWithSlash "/"

type Router<'Action when 'Action : equality> =
    {
        StaticRoutes : IDictionary<string,'Action>
        StaticLinks : IDictionary<'Action,string>
        DynamicRoute : Http.Request -> option<'Action>
        DynamicLink : 'Action -> option<Location>
    }

    member this.Link(action: 'Action) =
        match this.StaticLinks.TryGetValue(action) with
        | true, v -> Some (makeUri v)
        | _ -> this.DynamicLink(action)

    member this.Route(req: Http.Request) =
        match this.StaticRoutes.TryGetValue(path req.Uri) with
        | true, r -> Some r
        | _ -> this.DynamicRoute(req)

    static member ( <|> ) (r1: Router<'Action>, r2: Router<'Action>) =
        {
            StaticRoutes = dictSum r1.StaticRoutes r2.StaticRoutes
            StaticLinks = dictSum r1.StaticLinks r2.StaticLinks
            DynamicLink = optFunSum r1.DynamicLink r2.DynamicLink
            DynamicRoute = optFunSum r1.Route r2.Route
        }

module Router =
    module J = IntelliFactory.WebSharper.Core.Json

    let New (route: Http.Request -> option<'T>) (link: 'T -> option<Location>) =
        {
            StaticRoutes = Dictionary()
            StaticLinks = Dictionary()
            DynamicRoute = route
            DynamicLink = link
        }

    let Table (mapping: seq<'Action * string>) =
        let mapping =
            mapping
            |> Seq.map (fun (k, v) ->
                if isAbsoluteUri v then (k, v) else
                    (k, joinWithSlash "/" v))
        let sr = Dictionary()
        let sl = Dictionary()
        for (a, l) in mapping do
            if sl.ContainsKey a then
                String.Format("More than one location corresponds \
                    to the action: {0}", a)
                |> invalidArg "mapping"
            else
                sl.Add(a, l)
            if sr.ContainsKey l then
                String.Format("More than one action corresponds to \
                    the location: {0}", l)
                |> invalidArg "mapping"
            else
                sr.Add(l, a)
        for (a, l) in mapping do
            if sr.[sl.[a]] <> a then
                String.Format("Invalid bijection for the {0} action.", a)
                |> invalidArg "mapping"
            if sl.[sr.[l]] <> l then
                String.Format("Invalid bijection for the {0} location.", l)
                |> invalidArg "mapping"
        {
            StaticRoutes = sr
            StaticLinks = sl
            DynamicRoute = fun _ -> None
            DynamicLink = fun _ -> None
        }

    let Infer () : Router<'Action> =
        let fmt = ActionEncoding.GetFormat<'Action>()
        {
            StaticRoutes = Dictionary()
            StaticLinks  = Dictionary()
            DynamicRoute = fun req ->
                let uri = path req.Uri
                fmt.Read (uri.Substring 1, req)
                |> Option.bind (function
                    | ActionEncoding.Success x -> Some x
                    | _ -> None)
            DynamicLink = fun act ->
                match fmt.Show act with
                | Some x -> Some (Uri("/" + x, UriKind.Relative))
                | None -> None
        }

    let InferWithErrors () : Router<ActionEncoding.DecodeResult<'Action>> =
        let fmt = ActionEncoding.GetFormat<'Action>()
        {
            StaticRoutes = Dictionary()
            StaticLinks  = Dictionary()
            DynamicRoute = fun req ->
                let uri = path req.Uri
                fmt.Read (uri.Substring 1, req)
            DynamicLink = function
                | ActionEncoding.Success act
                | ActionEncoding.MissingQueryParameter (act, _)
                | ActionEncoding.InvalidJson act
                | ActionEncoding.InvalidMethod (act, _) ->
                    match fmt.Show act with
                    | Some x -> Some (Uri("/" + x, UriKind.Relative))
                    | None -> None
        }

    let Empty<'Action when 'Action : equality> : Router<'Action> =
        New (fun _ -> None) (fun _ -> None)

    let Sum (routers: seq<Router<'Action>>) : Router<'Action> =
        if Seq.isEmpty routers then Empty else
            Seq.reduce ( <|> ) routers

    let Map (encode: 'Action1 -> 'Action2)
            (decode: 'Action2 -> 'Action1)
            (router: Router<'Action1>) =
        {
            StaticRoutes =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticRoutes do
                    d.[k] <- encode v
                d
            StaticLinks =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticLinks do
                    d.[encode k] <- v
                d
            DynamicRoute = Option.map encode << router.DynamicRoute
            DynamicLink  = router.DynamicLink << decode
        }

    let Shift (prefix: string) (router: Router<'Action>) =
        let prefix = joinWithSlash "/" prefix
        let shift (loc: Location) =
            if loc.IsAbsoluteUri then loc else
                makeUri (joinWithSlash prefix (path loc))
        {
            StaticRoutes =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticRoutes do
                    d.[joinWithSlash prefix k] <- v
                d
            StaticLinks =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticLinks do
                    d.[k] <- joinWithSlash prefix v
                d
            DynamicRoute = fun req ->
                let builder = UriBuilder req.Uri
                if builder.Path.StartsWith prefix then
                    builder.Path <- builder.Path.Substring prefix.Length
                    router.DynamicRoute {req with Uri = builder.Uri}
                else
                    None
            DynamicLink =
                Option.map shift << router.DynamicLink
        }

    let FromPostParameter (name: string):  Router<string> =
        let route (req: Http.Request) =
            match req.Method, req.Post.[name] with
            | Http.Method.Post, (Some _ as r) -> r
            | _ -> None
        New route (fun _ -> None)

    let FromJsonParameter<'T when 'T : equality>(name: string) : Router<'T> =
        let decoder =
            (* TODO: Consider that Shared.Json relies on ASP.NET-specific
               initialization. Does this limit the utility of this method? *)
            IntelliFactory.WebSharper.Web.Shared.Json.GetDecoder<'T>()
        let route (req: Http.Request) =
            if req.Method = Http.Method.Post then
                req.Post.[name]
                |> Option.bind (fun json ->
                    if json = null then None else
                        Some (decoder.Decode (J.Parse json)))
            else None
        New route (fun _ -> None)

    let At (url: string) (r: Router<'T>): Router<'T> =
        New r.Route (fun _ -> makeUri url |> Some)

    let TryMap (encode: 'T1 -> option<'T2>)
               (decode: 'T2 -> option<'T1>)
               (router: Router<'T1>): Router<'T2> =
        {
            StaticRoutes =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticRoutes do
                    encode v
                    |> Option.iter (fun v2 ->
                        d.[k] <- v2)
                d
            StaticLinks =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticLinks do
                    encode k
                    |> Option.iter (fun k2 ->
                        d.[k2] <- v)
                d
            DynamicRoute = router.DynamicRoute >> Option.bind encode
            DynamicLink  = decode >> Option.bind router.DynamicLink
        }
