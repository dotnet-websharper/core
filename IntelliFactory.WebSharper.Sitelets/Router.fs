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

open System
open System.Collections.Generic
open System.Web.UI
open IntelliFactory.WebSharper

/// Represents addressable locations.
type Location = Uri

/// Provides a bijection between URL locations and abstract actions.
type Router<'Action when 'Action : equality> =
    internal {
        StaticRoutes    : IDictionary<string,'Action>
        StaticLinks     : IDictionary<'Action,Location>
        DynamicRoute    : Http.Request -> option<'Action>
        DynamicLink     : 'Action -> option<Location>
    }

    /// Tries to constructs a link to a given action. Fails with None
    /// if the action is not understood by the router.
    member this.Link(action: 'Action) =
        if this.StaticLinks.ContainsKey action then
            Some this.StaticLinks.[action]
        else
            this.DynamicLink action

    /// Tries to route a request to an action. Fails if the request
    /// is not understood by the router.
    member this.Route(req: Http.Request) =
        if this.StaticRoutes.ContainsKey req.Uri.LocalPath then
            Some this.StaticRoutes.[req.Uri.LocalPath]
        else
            this.DynamicRoute req

    /// Combines two routers. The combined router
    static member ( <|> ) (r1: Router<'Action>, r2: Router<'Action>) =
        {
            StaticRoutes =
                let d = Dictionary<_,_>()
                for kv in r1.StaticRoutes do
                    d.[kv.Key] <- kv.Value
                for kv in r2.StaticRoutes do
                    if not <| d.ContainsKey( kv.Key) then
                        d.[kv.Key] <- kv.Value
                d :> IDictionary<_,_>

            StaticLinks =
                let d = Dictionary<_,_>()
                for kv in r1.StaticLinks do
                    d.[kv.Key] <- kv.Value
                for kv in r2.StaticLinks do
                    if not <| d.ContainsKey( kv.Key) then
                        d.[kv.Key] <- kv.Value
                d :> IDictionary<_,_>

            DynamicLink = fun action ->
                match r1.Link action with
                | Some x    -> Some x
                | None      -> r2.Link action

            DynamicRoute = fun req ->
                match r1.Route req with
                | Some x    -> Some x
                | None      -> r2.Route req
        }

/// Provides combinators over the Router type.
module Router =
    module J = IntelliFactory.WebSharper.Core.Json

    /// Creates a absolute or relative URI form a string.
    let private MakeUri uri =
        match Uri.TryCreate(uri, UriKind.Absolute) with
        | true, uri -> uri
        | _         -> Uri(uri, UriKind.Relative)

    /// Constructs a custom new router with a given route and link functions.
    let New route link : Router<'Action> =
        {
            StaticRoutes    = Dictionary()
            StaticLinks     = Dictionary()
            DynamicRoute    = route
            DynamicLink     = link
        }

    /// Constructs a router from a finite table defining a
    /// bijection between locations and actions. Throws InvalidArgument
    /// exceptions if the table does not define a bijection.
    let Table (mapping: seq<'Action * string>) =
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
        let links = Dictionary()
        for KeyValue (k, v) in sl do
            links.[k] <-MakeUri v
        {
            StaticRoutes    = sr
            StaticLinks     = links
            DynamicRoute    = fun _ -> None
            DynamicLink     = fun _ -> None
        }

    /// Infers the router by analyzing an algebraic action data type.
    let Infer () : Router<'Action> =
        let fmt = UrlEncoding.GetFormat<'Action>()
        {
            StaticRoutes = Dictionary()
            StaticLinks  = Dictionary()
            DynamicRoute = fun req ->
                let uri =
                    if req.Uri.IsAbsoluteUri then
                        req.Uri.LocalPath + req.Uri.Fragment
                    else
                        Uri.UnescapeDataString req.Uri.OriginalString
                if uri.Length > 0 && uri.[0] = '/' then
                    fmt.Read (uri.Substring 1)
                else
                    None
            DynamicLink = fun act ->
                match fmt.Show act with
                | Some x ->
                    let uri = "/" + x
                    Some (Uri(uri, UriKind.Relative))
                | None -> None
        }

    /// Composes several routers. For both linking and routing,
    /// the leftmost matching router is selected. Two routers can be
    /// composed with the `<|>` combinator.
    let Sum (routers: seq<Router<'Action>>) : Router<'Action> =
        let zero = New (fun _ -> None) (fun _ -> None)
        Seq.fold ( <|> ) zero routers

    /// Maps over a router, changing its action type.
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

    /// Shifts the router's locations by adding a prefix.
    let Shift (prefix: string) (router: Router<'Action>) =
        let shift (loc: Location) =
            if loc.IsAbsoluteUri then loc else
                Uri (prefix + loc.OriginalString, UriKind.Relative)
        {
            StaticRoutes =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticRoutes do
                    d.[prefix + k] <- v
                d
            StaticLinks =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticLinks do
                    d.[k] <- shift v
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


    /// Creates a router with Link always failing,
    /// and Route picking a POST-ed parameter with a
    /// given key, for example PostParameter "id" routes
    /// request POST id=123 to action "123".
    let FromPostParameter (name: string):  Router<string> =
        let route (req: Http.Request) =
            if req.Method = Http.Method.Post && req.Post.[name].IsSome then
                req.Post.[name]
            else
                None
        New  route (fun _ -> None)

    /// Creates a router with Link always failing,
    /// and Route picking a POST-ed parameter with a
    /// given key, assumes that the corresponding value is
    // a JSON string, and tries to decode it into a value.
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

    /// Modifies the router to use a constant URL.
    let At (url: string) (r: Router<'T>): Router<'T> =
        New r.Route (fun _ -> MakeUri url |> Some)

    let Empty<'Action when 'Action : equality> : Router<'Action> =
        New (fun _ -> None) (fun _ -> None)

    /// Maps over the action type of the router.
    let TryMap (encode: 'T1 -> option<'T2>)
               (decode: 'T2 -> option<'T1>)
               (router: Router<'T1>): Router<'T2> =
        {
            StaticRoutes =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticRoutes do
                    encode v
                    |> Option.iter (fun v2 ->
                        d.[k] <- v2
                    )
                d
            StaticLinks =
                let d = Dictionary()
                for KeyValue (k, v) in router.StaticLinks do
                    encode k
                    |> Option.iter (fun k2 ->
                        d.[k2] <- v
                    )
                d

            DynamicRoute =
                router.DynamicRoute >> Option.bind encode

            DynamicLink  =
                decode >> Option.bind router.DynamicLink
        }
