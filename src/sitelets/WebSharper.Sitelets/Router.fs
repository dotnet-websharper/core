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

namespace WebSharper.Sitelets

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open System.Collections.Generic
open System.Text

#nowarn "64" // type parameter renaming warnings 

[<NamedUnionCases "result"; RequireQualifiedAccess>]
type ParseRequestResult<'T> =
    | [<CompiledName "success">]
      Success of endpoint: 'T
    | [<CompiledName "invalidMethod">]
      InvalidMethod of endpoint: 'T * ``method``: string
    | [<CompiledName "invalidJson">]
      InvalidJson of endpoint: 'T
    | [<CompiledName "missingQueryParameter">]
      MissingQueryParameter of endpoint: 'T * queryParam: string
    | [<CompiledName "missingFormData">]
      MissingFormData of endpoint: 'T * formFieldName: string

    member this.Value =
        match this with
        | Success a
        | InvalidMethod (a, _)
        | InvalidJson a
        | MissingQueryParameter (a, _)
        | MissingFormData (a, _) -> a

    [<System.Obsolete "Use Value instead">]
    member this.Action = this.Value

[<System.Obsolete "Use ParseRequestResult instead of ActionEncoding.DecodeResult">]
/// For back-compatibility only, use ParseRequestResult instead of ActionEncoding.DecodeResult
module ActionEncoding =

    type DecodeResult<'T> = ParseRequestResult<'T>

    let Success endpoint = ParseRequestResult.Success endpoint
    let InvalidMethod (endpoint, ``method``) = ParseRequestResult.InvalidMethod(endpoint, ``method``)
    let InvalidJson endpoint = ParseRequestResult.InvalidJson endpoint
    let MissingQueryParameter (endpoint, queryParam) = ParseRequestResult.MissingQueryParameter(endpoint, queryParam)
    let MissingFormData (endpoint, formFieldName) = ParseRequestResult.MissingFormData(endpoint, formFieldName)

type internal PathUtil =
    static member WriteQuery q =
        let sb = StringBuilder 128
        let mutable start = true
        q |> Map.toSeq |> Seq.iter (fun (k: string, v: string) ->
            if start then
                start <- false
            else 
                sb.Append('&') |> ignore                    
            sb.Append(k).Append('=').Append(v) |> ignore
        )
        sb.ToString()

    static member WriteLink s q =
        let sb = StringBuilder 128
        s |> List.iter (fun x ->
            if not (System.String.IsNullOrEmpty x) then
                sb.Append('/').Append(x) |> ignore
        )
        if Map.isEmpty q then () 
        else 
            let mutable start = true
            sb.Append('?') |> ignore                    
            q |> Map.toSeq |> Seq.iter (fun (k: string, v: string) ->
                if start then
                    start <- false
                else 
                    sb.Append('&') |> ignore                    
                sb.Append(k).Append('=').Append(v) |> ignore
            )
        sb.ToString()

[<Proxy(typeof<PathUtil>)>]
type internal PathUtilProxy =
    static member Concat xs = 
        let sb = System.Collections.Generic.Queue()
        let mutable start = true
        xs |> List.iter (fun x ->
            if not (System.String.IsNullOrEmpty x) then
                if start then
                    start <- false
                else 
                    sb.Enqueue("/") |> ignore                    
                sb.Enqueue(x) |> ignore
        )
        sb |> System.String.Concat

    static member WriteQuery q =
        q |> Map.toSeq |> Seq.map (fun (k, v) -> k + "=" + v) |> String.concat "&"

    static member WriteLink s q =
        let query = 
            if Map.isEmpty q then "" 
            else "?" + PathUtil.WriteQuery(q)
        "/" + PathUtilProxy.Concat s + query

[<JavaScript>]
type Route =
    {
        Segments : list<string>
        QueryArgs : Map<string, string>
        Method : option<string> 
        Body : option<string>
    }

    static member Empty =
        {
            Segments = []
            QueryArgs = Map.empty
            Method = None
            Body = None
        }
    
    static member Segment s =
        { Route.Empty with
            Segments = [ s ]
        }

    static member Segment s =
        { Route.Empty with
            Segments = s
        }

    static member Segment (s, m) =
        { Route.Empty with
            Segments = s
            Method = m
        }

    static member Combine (paths: seq<Route>) =
        let paths = Seq.toArray paths
        match paths.Length with
        | 1 -> paths.[0]
        | 0 -> Route.Empty
        | _ ->
        let mutable method = None
        let mutable body = None
        let segments = System.Collections.Generic.Queue()
        let mutable queryArgs = Map.empty
        let mutable i = 0
        let l = paths.Length
        while i < l do
            let p = paths.[i]
            match p.Method with
            | Some _ as m ->
                method <- m
            | _ -> ()
            match p.Body with
            | Some _ as b ->
                body <- b
            | _ -> ()
            queryArgs <- p.QueryArgs |> Map.foldBack Map.add queryArgs 
            p.Segments |> List.iter segments.Enqueue
            i <- i + 1
        {
            Segments = List.ofSeq segments
            QueryArgs = queryArgs
            Method = method
            Body = body
        }

    static member ParseQuery(q: string) =
        q.Split('&') |> Array.choose (fun kv ->
            match kv.Split('=') with
            | [| k; v |] -> Some (k, v)
            | _ -> 
                printfn "wrong format for query argument: %s" kv
                None
        ) |> Map.ofSeq
    
    static member WriteQuery(q) = PathUtil.WriteQuery q

    static member FromUrl(path: string, ?strict: bool) =
        let s, q = 
            match path.IndexOf '?' with
            | -1 -> path, Map.empty
            | i -> 
                path.Substring(0, i),
                path.Substring(i + 1) |> Route.ParseQuery
        let splitOptions =
            if strict = Some true then 
                System.StringSplitOptions.None
            else
                System.StringSplitOptions.RemoveEmptyEntries
        { Route.Empty with
            Segments = 
                s.Split([| '/' |], splitOptions) |> List.ofArray
            QueryArgs = q
        }

    [<JavaScript false>]
    static member FromRequest(r: System.Web.HttpRequestBase) =
        let u = r.Url
        let p = if u.IsAbsoluteUri then u.PathAndQuery else u.OriginalString
        { Route.FromUrl(p) with
            Method = Some r.HttpMethod
            Body =
                let i = r.InputStream 
                if not (isNull i) then 
                    // We need to copy the stream because else StreamReader would close it.
                    use m =
                        if i.CanSeek then
                            new System.IO.MemoryStream(int i.Length)
                        else
                            new System.IO.MemoryStream()
                    i.CopyTo m
                    if i.CanSeek then
                        i.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                    m.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                    use reader = new System.IO.StreamReader(m)
                    Some (reader.ReadToEnd())
                else None
        }

    [<JavaScript false>]
    static member FromWSRequest(r: Http.Request) =
        let u = r.Uri
        let p = if u.IsAbsoluteUri then u.PathAndQuery else u.OriginalString
        { Route.FromUrl(p) with
            Method = Some (r.Method.ToString())
            Body =
                let i = r.Body 
                if not (isNull i) then 
                    // We need to copy the stream because else StreamReader would close it.
                    use m =
                        if i.CanSeek then
                            new System.IO.MemoryStream(int i.Length)
                        else
                            new System.IO.MemoryStream()
                    i.CopyTo m
                    if i.CanSeek then
                        i.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                    m.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                    use reader = new System.IO.StreamReader(m)
                    Some (reader.ReadToEnd())
                else None
        }

    static member FromHash(path: string, ?strict: bool) =
        match path.IndexOf "#" with
        | -1 -> Route.Empty
        | i -> Route.FromUrl(path.Substring(i), ?strict = strict)

    member this.ToLink() = PathUtil.WriteLink this.Segments this.QueryArgs

[<JavaScript>]
module internal List =
    let rec startsWith s l =
        match s, l with
        | [], _ -> Some l
        | sh :: sr, lh :: lr when sh = lh -> startsWith sr lr
        | _ -> None

type IRouter<'T> =
    abstract Route : Http.Request -> option<'T>
    abstract Link : 'T -> option<System.Uri>

[<JavaScript>]
type Router =
    {
        Parse : Route -> Route seq
        Segment : seq<Route> 
    }
    
    static member FromString (name: string) =
        let parts = name.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries)
        if Array.isEmpty parts then 
            {
                Parse = fun path -> Seq.singleton path
                Segment = Seq.empty
            }
        else
            let parts = List.ofArray parts
            {
                Parse = fun path ->
                    match path.Segments |> List.startsWith parts with
                    | Some p -> 
                        Seq.singleton ({ path with Segments = p })
                    | _ -> Seq.empty
                Segment = 
                    Seq.singleton (Route.Segment parts)
            }

    static member (/) (before: Router, after: Router) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect after.Parse
            Segment = 
                Seq.append before.Segment after.Segment
        }

    [<Inline>]
    static member (/) (before: string, after: Router) = Router.FromString before / after

    [<Inline>]
    static member (/) (before: Router, after: string) = before / Router.FromString after

    static member (+) (a: Router, b: Router) =
        {
            Parse = fun path ->
                Seq.append (a.Parse path) (b.Parse path) 
            Segment = a.Segment
        }

    [<Inline>]
    static member Combine<'A, 'B when 'A: equality and 'B: equality>(a: Router<'A>, b: Router<'B>) : Router<'A * 'B> =
        a / b

and [<JavaScript>] Router<'T when 'T: equality> =
    {
        Parse : Route -> (Route * 'T) seq
        Write : 'T -> option<seq<Route>> 
    }
    
    static member (/) (before: Router<'T>, after: Router<'U>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect (fun (p, x) -> after.Parse p |> Seq.map (fun (p, y) -> (p, (x, y))))
            Write = fun (v1, v2) ->
                match before.Write v1, after.Write v2 with
                | Some p1, Some p2 -> Some (Seq.append p1 p2)
                | _ -> None
        }

    static member (/) (before: Router, after: Router<'T>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect after.Parse
            Write = fun v ->
                after.Write v |> Option.map (Seq.append before.Segment)
        }

    static member (/) (before: Router<'T>, after: Router) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect (fun (p, x) -> after.Parse p |> Seq.map (fun p -> (p, x)))
            Write = fun v ->
                before.Write v |> Option.map (fun x -> Seq.append x after.Segment)
        }

    [<Inline>]
    static member (/) (before: string, after: Router<'T>) = Router.FromString before / after

    [<Inline>]
    static member (/) (before: Router<'T>, after: string) = before / Router.FromString after

    static member (+) (a: Router<'T>, b: Router<'T>) =
        {
            Parse = fun path ->
                Seq.append (a.Parse path) (b.Parse path) 
            Write = fun value ->
                match a.Write value with
                | None -> b.Write value
                | p -> p
        }

    interface IRouter<'T> with
        [<JavaScript false>]
        member this.Route req = 
            let path = Route.FromWSRequest req
            this.Parse path
            |> Seq.tryPick (fun (path, value) -> if List.isEmpty path.Segments then Some value else None)
        [<JavaScript false>]
        member this.Link ep =
            this.Write ep |> Option.map (fun p -> System.Uri((Route.Combine p).ToLink(), System.UriKind.Relative))
        
[<JavaScript>]
module Router =
    [<Inline>]
    let Combine (a: Router<'A>) (b: Router<'B>) = a / b
    
    [<Inline>]
    let Shift (prefix: string) (router: Router<'A>) =
        prefix / router

    let Empty<'A when 'A: equality> : Router<'A> =
        {
            Parse = fun _ -> Seq.empty
            Write = fun _ -> None
        }

    /// Creates a fully customized router.
    let New (route: Http.Request -> option<'T>) (link: 'T -> option<System.Uri>) =
        { new IRouter<'T> with
            member this.Route req = route req
            member this.Link e = link e
        }

    /// Compatible with old UI.Next.RouteMap.Create.
    let Create (ser: 'T -> list<string>) (des: list<string> -> 'T) =
        {
            Parse = fun path ->
                Seq.singleton ({ path with Segments = [] }, des path.Segments)
            Write = fun value ->
                Some (Seq.singleton (Route.Segment(ser value)))
        }

    /// Compatible with old UI.Next.RouteMap.CreateWithQuery.
    let CreateWithQuery (ser: 'T -> list<string> * Map<string, string>) (des: list<string> * Map<string, string> -> 'T) =
        {
            Parse = fun path ->
                Seq.singleton ({ path with Segments = [];  }, des (path.Segments, path.QueryArgs))
            Write = fun value ->
                let s, q = ser value
                Some (Seq.singleton { Route.Empty with Segments = s; QueryArgs = q })
        }
    
    /// Parses/writes a single value from a query argument with the given key instead of url path.
    let Query key (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.empty
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (p, v) ->
                        { path with QueryArgs = newQa }, v
                    )
            Write = fun value ->
                item.Write value |> Option.map (fun p -> 
                    let p = Route.Combine p
                    match p.Segments with
                    | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                    | _ -> Seq.empty
                )
        }

    /// Parses/writes a single option value from an optional query argument with the given key instead of url path.
    let QueryOption key (item: Router<'A>) : Router<option<'A>> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.singleton (path, None)
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (_, v) ->
                        { path with QueryArgs = newQa }, Some v
                    )
            Write = fun value ->
                match value with
                | None -> Some Seq.empty
                | Some v ->
                    item.Write v |> Option.map (fun p -> 
                        let p = Route.Combine p
                        match p.Segments with
                        | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                        | _ -> Seq.empty
                    )
        }

    /// Parses/writes a single nullable value from an optional query argument with the given key instead of url path.
    let QueryNullable key (item: Router<'A>) : Router<System.Nullable<'A>> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.singleton (path, System.Nullable())
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Route.Empty with Segments = [ q ] }
                    |> Seq.map (fun (_, v) ->
                        { path with QueryArgs = newQa }, System.Nullable v
                    )
            Write = fun value ->
                if value.HasValue then
                    item.Write value.Value |> Option.map (fun p -> 
                        let p = Route.Combine p
                        match p.Segments with
                        | [ v ] -> Seq.singleton { Route.Empty with QueryArgs = Map.ofList [ key, v ] }
                        | _ -> Seq.empty
                    )
                else
                    Some Seq.empty
        }

    let Method (m: string) : Router =
        {
            Parse = fun path ->
                match path.Method with
                | Some pm when pm = m -> Seq.singleton path
                | _ -> Seq.empty
            Segment =
                Seq.singleton { Route.Empty with Method = Some m }
        }

    let Body (deserialize: string -> option<'A>) (serialize: 'A -> string) : Router<'A> =
        {
            Parse = fun path ->
                match path.Body |> Option.bind deserialize with
                | Some b -> Seq.singleton ({ path with Body = None}, b)
                | _ -> Seq.empty
            Write = fun value ->
                Some <| Seq.singleton { Route.Empty with Body = Some (serialize value) }
        }

    let FormData (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                match path.Body with
                | None -> item.Parse path
                | Some b ->
                    item.Parse { path with QueryArgs = path.QueryArgs |> Map.foldBack Map.add (Route.ParseQuery b); Body = None }
            Write = fun value ->
                item.Write value |> Option.map Route.Combine 
                |> Option.map (fun p -> Seq.singleton { p with QueryArgs = Map.empty; Body = Some (Route.WriteQuery p.QueryArgs) })  
        }
    
    let Parse (router: Router<'A>) path =
        router.Parse path
        |> Seq.tryPick (fun (path, value) -> if List.isEmpty path.Segments then Some value else None)

    let Write (router: Router<'A>) endpoint =
        router.Write endpoint |> Option.map Route.Combine 

    let TryLink (router: Router<'A>) endpoint =
        match Write router endpoint with
        | Some p -> Some (p.ToLink())
        | None -> None

    let Link (router: Router<'A>) endpoint =
        match Write router endpoint with
        | Some p -> p.ToLink()
        | None -> ""

    let Ajax (router: Router<'A>) endpoint =
        match Write router endpoint with
        | Some path ->
            let settings = AjaxSettings(DataType = DataType.Text)
            match path.Method with
            | Some m -> settings.Method <- As m
            | _ -> ()
            Async.FromContinuations (fun (ok, err, cc) ->
                settings.Success <- fun res _ _ -> ok (As<string> res) 
                settings.Error <- fun _ _ msg -> err (exn msg)
                // todo: cancellation
                JQuery.Ajax(path.ToLink(), settings) |> ignore
            )
        | _ -> 
            failwith "Failed to map endpoint to request" 

    let HashLink (router: Router<'A>)  endpoint =
        "#" + Link router endpoint
    
    /// Maps a router to a narrower router type. The decode function must return None if the
    /// value can't be mapped to a value of the target.
    let Slice (decode: 'T -> 'U option) (encode: 'U -> 'T) (router: Router<'T>) : Router<'U> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> decode v |> Option.map (fun v -> p, v)) 
            Write = fun value ->
                encode value |> router.Write
        }

    /// Maps a router to a wider router type. The encode function must return None if the
    /// value can't be mapped back to a value of the source.
    let Embed (decode: 'A -> 'B) (encode: 'B -> 'A option) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, decode v) 
            Write = fun value ->
                encode value |> Option.bind router.Write
        }

    /// Maps a router with a bijection.
    let Map (decode: 'A -> 'B) (encode: 'B -> 'A) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, decode v) 
            Write = fun value ->
                encode value |> router.Write
        }

    /// Combination of Slice and Embed, a mapping from a subset of source values to
    /// a subset of target values. Both encode and decode must return None if
    /// there is no mapping to a value of the other type.
    let TryMap (decode: 'A -> 'B option) (encode: 'B -> 'A option) router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> decode v |> Option.map (fun v -> p, v)) 
            Write = fun value ->
                encode value |> Option.bind router.Write
        }

    /// Filters a router, only parsing/writing values that pass the predicate check.
    let Filter predicate router =
        {
            Parse = fun path ->
                router.Parse path |> Seq.filter (snd >> predicate)
            Write = fun value ->
                if predicate value then router.Write value else None
        }

    [<Name "Box">]
    let private BoxImpl tryUnbox (router: Router<'A>): Router<obj> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, box v) 
            Write = fun value ->
                tryUnbox value |> Option.bind router.Write
        }

    [<Inline>]
    /// Converts to Router<obj>. When writing, a type check against type A is performed.
    let Box (router: Router<'A>): Router<obj> =
        BoxImpl (function :? 'A as v -> Some v | _ -> None) router

    [<Inline>]
    let Json<'T when 'T: equality> : Router<'T> =
        Body (fun s -> try Some (Json.Deserialize<'T> s) with _ -> None) Json.Serialize<'T>

    [<Name "Unbox">]
    let UnboxImpl<'A when 'A: equality> tryUnbox (router: Router<obj>) : Router<'A> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> match tryUnbox v with Some v -> Some (p, v) | _ -> None) 
            Write = fun value ->
                box value |> router.Write
        }

    [<Inline>]
    /// Converts from Router<obj>. When parsing, a type check against type A is performed.
    let Unbox<'A when 'A: equality> (router: Router<obj>) : Router<'A> =
        UnboxImpl (function :? 'A as v -> Some v | _ -> None) router

    [<Name "Cast">]
    let private CastImpl tryParseCast tryWriteCast (router: Router<'A>): Router<'B> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> match tryParseCast v with Some v -> Some (p, v) | _ -> None) 
            Write = fun value ->
                tryWriteCast value |> Option.bind router.Write
        }

    [<Inline>]
    /// Converts a Router<A> to Router<B>. When parsing and writing, type checks are performed.
    /// Upcasting do not change set of parsed routes, downcasting restricts it within the target type.
    let Cast (router: Router<'A>): Router<'B> =
        CastImpl (fun v -> match box v with :? 'B as v -> Some v | _ -> None) (fun v -> match box v with :? 'A as v -> Some v | _ -> None) router

    /// Maps a single-valued (non-generic) Router to a specific value.
    let MapTo value (router: Router) =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun p -> p, value) 
            Write = fun v ->
                if v = value then Some router.Segment else None
        }

    /// Parses/writes using any of the routers, attempts are made in the given order.
    let Sum (routers: seq<Router<_>>) =
        let routers = Array.ofSeq routers
        {
            Parse = fun path ->
                routers |> Seq.collect (fun r -> r.Parse path)
            Write = fun value ->
                routers |> Seq.tryPick (fun r -> r.Write value)
        }
    
    // todo: optimize
    let Table<'T when 'T : equality> (mapping: seq<'T * string>) : Router<'T> =
        mapping |> Seq.map (fun (v, s) -> Router.FromString s |> MapTo v) |> Sum 

    // todo: optimize
    let Single<'T when 'T : equality> (endpoint: 'T) (route: string) : Router<'T> =
        Router.FromString route |> MapTo endpoint

    let Delay<'T when 'T: equality> (getRouter: unit -> Router<'T>) : Router<'T> =
        let r = lazy getRouter()
        {
            Parse = fun path -> r.Value.Parse path
            Write = fun value -> r.Value.Write value
        }

    /// Creates a router for parsing/writing an Array of values.
    let Array (item: Router<'A>) : Router<'A[]> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Int32.TryParse h with
                    | true, l ->
                        let rec collect l path acc =
                            if l = 0 then Seq.singleton (path, Array.ofList (List.rev acc))
                            else item.Parse path |> Seq.collect(fun (p, a) -> collect (l - 1) p (a :: acc))
                        collect l { path with Segments = t } []
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                let parts = value |> Array.map item.Write
                if Array.forall Option.isSome parts then
                    Some (Seq.append (Seq.singleton (Route.Segment (string value.Length))) (parts |> Seq.collect Option.get))
                else None                      
        }

    /// Creates a router for parsing/writing a Nullable value.
    let Nullable (item: Router<'A>) : Router<System.Nullable<'A>> =
        {
            Parse = fun path ->
                match path.Segments with
                | "null" :: p -> 
                    Seq.singleton ({ path with Segments = p }, System.Nullable())
                | _ ->
                    item.Parse path |> Seq.map (fun (p, v) -> p, System.Nullable v)
            Write = fun value ->
                if value.HasValue then 
                    item.Write value.Value
                else 
                    Some (Seq.singleton (Route.Segment "null"))
        }

    /// Creates a router for parsing/writing an F# option of a value.
    let Option (item: Router<'A>) : Router<'A option> =
        {
            Parse = fun path ->
                match path.Segments with
                | "None" :: p -> 
                    Seq.singleton ({ path with Segments = p }, None)
                | "Some" :: _ ->
                    item.Parse path |> Seq.map (fun (p, v) -> p, Some v)
                | _ ->
                    Seq.empty
            Write = fun value ->
                match value with 
                | None -> Some (Seq.singleton (Route.Segment "None"))
                | Some v -> 
                    item.Write v |> Option.map (Seq.append (Seq.singleton (Route.Segment "None")))
        }

    module FArray = Collections.Array

    type IListArrayConverter =
        abstract OfArray: obj -> obj
        abstract ToArray: obj -> obj

    type ListArrayConverter<'T>() =
        interface IListArrayConverter with
            member this.OfArray a = List.ofArray (unbox<'T []> a) |> box
            member this.ToArray l = List.toArray (unbox<'T list> l) |> box

    /// Creates a router for parsing/writing an F# list of a value.
    let List (item: Router<'A>) : Router<'A list> =
        Array item |> Map List.ofArray FArray.ofList

type Router with
    [<Inline>]
    member this.MapTo(value: 'T) =
        Router.MapTo value this

    [<Inline>]
    static member Sum ([<System.ParamArray>] routers: Router<'T>[]) =
        Router.Sum routers

    [<Inline>]
    static member Empty<'T when 'T: equality>() =
        Router.Empty<'T>

    [<JavaScript false>]
    static member New(route: System.Func<Http.Request, 'T>, link: System.Func<'T, System.Uri>) =
        Router.New (route.Invoke >> Option.ofObj) (link.Invoke >> Option.ofObj)

    [<Inline>]
    static member Method(method:string) =
        Router.Method method

    [<Inline>]
    static member Body(des:System.Func<string, 'T>, ser: System.Func<'T, string>) =
        Router.Body (fun s -> des.Invoke s |> Option.ofObj) ser.Invoke 

    [<Inline>]
    static member Json<'T when 'T: equality>() =
        Router.Json<'T>

    [<Inline>]
    static member Table([<System.ParamArray>] mapping: ('T * string)[]) =
        Router.Table mapping

    [<Inline>]
    static member Single(endpoint, route) =
        Router.Single endpoint route

    [<Inline>]
    static member Delay(getRouter: System.Func<Router<'T>>) =
        Router.Delay getRouter.Invoke

type Router<'T when 'T: equality> with

    [<Inline>]
    member this.Query(key: string) =
        Router.Query key this

    [<Inline>]
    member this.Link(endpoint: 'T) =
        Router.Link this endpoint

    [<Inline>]
    member this.TryLink(endpoint: 'T, link: byref<string>) =
        match Router.TryLink this endpoint with
        | Some l ->
            link <- l
            true
        | _ -> false
               
    [<Inline>]
    member this.HashLink(endpoint: 'T) =
        Router.HashLink this endpoint

    [<Inline>]
    member this.Map(decode: System.Func<'T, 'U>, encode: System.Func<'U, 'T>) =
        Router.TryMap (decode.Invoke >> ofObjNoConstraint) (encode.Invoke >> ofObjNoConstraint) this

    [<Inline>]
    member this.Filter(predicate: System.Func<'T, bool>) =
        Router.Filter predicate.Invoke this

    [<Inline>]
    member this.Cast<'U when 'U: equality>() : Router<'U> =
        Router.Cast this

    [<Inline>]
    member this.FormData() =
        Router.FormData this

    [<Inline>]
    member this.Ajax(endpoint) =
        Router.Ajax this endpoint |> Async.StartAsTask

    [<Inline>]
    member this.Box() =
        Router.Box this

    [<Inline>]
    member this.Array() =
        Router.Array this

open System.Runtime.CompilerServices
    
[<Extension>]
type RouterExtensions =
    [<Inline>]
    static member QueryNullable(router, key) =
        Router.QueryNullable key router

    [<Inline>]
    static member Unbox<'T when 'T: equality>(router) =
        Router.Unbox<'T> router

    [<Inline>]
    static member Nullable(router) =
        Router.Nullable router

module IRouter =
    open System

    let Empty : IRouter<'T> =
        { new IRouter<'T> with
            member this.Route _ = None
            member this.Link _ = None
        }        

    let Add (r1: IRouter<'T>) (r2: IRouter<'T>) =
        { new IRouter<'T> with
            member this.Route req = match r1.Route req with Some _ as l -> l | _ -> r2.Route req
            member this.Link e = match r1.Link e with Some _ as l -> l | _ -> r2.Link e
        }        

    let Sum (routers: seq<IRouter<'T>>) : IRouter<'T> =
        let routers = Array.ofSeq routers
        if Seq.isEmpty routers then Empty else
            { new IRouter<'T> with
                member this.Route req = routers |> Array.tryPick (fun r -> r.Route req)
                member this.Link e = routers |> Array.tryPick (fun r -> r.Link e)
            }        
            
    let Map encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.map encode
            member this.Link e = decode e |> router.Link
        } 
        
    let TryMap encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.bind encode
            member this.Link e = decode e |> Option.bind router.Link
        } 

    let Embed encode decode (router: IRouter<'T>) : IRouter<'U> =
        { new IRouter<'U> with
            member this.Route req = router.Route req |> Option.map encode
            member this.Link e = decode e |> Option.bind router.Link
        } 

    let private makeUri uri =
        let mutable res = null
        if Uri.TryCreate(uri, UriKind.Relative, &res) then res else
            Uri(uri, UriKind.Absolute)
    
    let private path (uri: Uri) =
        if uri.IsAbsoluteUri
        then uri.AbsolutePath
        else Uri.UnescapeDataString(uri.OriginalString) |> joinWithSlash "/"
        
    let private trimFinalSlash (s: string) =
        match s.TrimEnd('/') with
        | "" -> "/"
        | s -> s
    
    let Shift prefix (router: IRouter<'T>) =
        let prefix = joinWithSlash "/" prefix
        let shift (loc: System.Uri) =
            if loc.IsAbsoluteUri then loc else
                makeUri (joinWithSlash prefix (path loc) |> trimFinalSlash)
        { new IRouter<'T> with
            member this.Route req =
                let builder = UriBuilder req.Uri
                if builder.Path.StartsWith prefix then
                    builder.Path <- builder.Path.Substring prefix.Length
                    router.Route {req with Uri = builder.Uri}
                else
                    None
            member this.Link e = router.Link e |> Option.map shift
        }     
        
    let Box (router: IRouter<'T>) : IRouter<obj> =
        { new IRouter<obj> with
            member this.Route req = router.Route req |> Option.map box
            member this.Link e = tryUnbox<'T> e |> Option.bind router.Link
        } 

    let Unbox (router: IRouter<obj>) : IRouter<'T> =
        { new IRouter<'T> with
            member this.Route req = router.Route req |> Option.bind tryUnbox<'T>
            member this.Link e = box e |> router.Link
        } 

[<JavaScript>]
module RouterOperators =
    let rRoot : Router =
        {
            Parse = fun path -> Seq.singleton path
            Segment = Seq.empty
        }
    
    [<Inline>]
    /// Parse/write a specific string.
    let r name : Router = Router.FromString name

    [<Inline "encodeURIComponent($x)">]
    let inline internal encodeURIComponent (x: string) =
        System.Uri.EscapeDataString(x) 

    [<Inline "decodeURIComponent($x)">]
    let inline internal decodeURIComponent (x: string) =
        System.Uri.UnescapeDataString(x) 

    /// Parse/write a string using URL encode/decode.
    let rString : Router<string> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    Seq.singleton ({ path with Segments = t }, decodeURIComponent h)
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (if isNull value then "null" else encodeURIComponent value)))
        }

    /// Parse/write a char.
    let rChar : Router<char> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t when h.Length = 1 -> 
                    Seq.singleton ({ path with Segments = t }, char (decodeURIComponent h))
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (encodeURIComponent (string value))))
        }

    [<Inline>]
    let inline rTryParse< ^T when ^T: (static member TryParse: string * byref< ^T> -> bool) and ^T: equality>() =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let mutable res = Unchecked.defaultof< ^T>
                    let ok = (^T: (static member TryParse: string * byref< ^T> -> bool) (h, &res))
                    if ok then 
                        Seq.singleton ({ path with Segments = t }, res)
                    else Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (string value)))
        }

    /// Parse/write a Guid.
    let rGuid = rTryParse<System.Guid>()
    /// Parse/write an int.
    let rInt = rTryParse<int>()
    /// Parse/write a double.
    let rDouble = rTryParse<double>()
    /// Parse/write a signed byte.
    let rSByte = rTryParse<sbyte>() 
    /// Parse/write a byte.
    let rByte = rTryParse<byte>() 
    /// Parse/write a 16-bit int.
    let rInt16 = rTryParse<int16>() 
    /// Parse/write a 16-bit unsigned int.
    let rUInt16 = rTryParse<uint16>() 
    /// Parse/write an unsigned int.
    let rUInt = rTryParse<uint32>() 
    /// Parse/write a 64-bit int.
    let rInt64 = rTryParse<int64>() 
    /// Parse/write a 64-bit unsigned int.
    let rUInt64 = rTryParse<uint64>() 
    /// Parse/write a single.
    let rSingle = rTryParse<single>() 

    /// Parse/write a bool.
    let rBool : Router<bool> =
        // we define rBool not with rTryParse so that fragments are capitalized
        // to be fully consistent on client+server
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Boolean.TryParse h with
                    | true, g ->
                        Seq.singleton ({ path with Segments = t }, g)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Route.Segment (if value then "True" else "False")))
        }

    /// Parses any remaining part of the URL as a string, no URL encode/decode is done.
    let rWildcard : Router<string> = 
        {
            Parse = fun path ->
                let s = path.Segments |> String.concat "/"
                Seq.singleton ({ path with Segments = [] }, s)
            Write = fun value ->
                Some (Seq.singleton (Route.Segment value))
        }
    
    let rWildcardArray (item: Router<'A>) : Router<'A[]> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let rec collect path acc =
                        match path.Segments with
                        | [] -> Seq.singleton (path, Array.ofList (List.rev acc))
                        | _ ->
                            item.Parse path |> Seq.collect(fun (p, a) -> collect p (a :: acc))
                    collect { path with Segments = t } []
                | _ -> Seq.singleton (path, [||])
            Write = fun value ->
                let parts = value |> Array.map item.Write
                if Array.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }

    let rWildcardList (item: Router<'A>) : Router<'A list> = 
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let rec collect path acc =
                        match path.Segments with
                        | [] -> Seq.singleton (path, List.rev acc)
                        | _ ->
                            item.Parse path |> Seq.collect(fun (p, a) -> collect p (a :: acc))
                    collect { path with Segments = t } []
                | _ -> Seq.singleton (path, [])
            Write = fun value ->
                let parts = value |> List.map item.Write
                if List.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }

    /// Parse/write a DateTime in `YYYY-MM-DD-HH.mm.ss` format.
    let rDateTime : Router<System.DateTime> =
        let pInt x =
            match System.Int32.TryParse x with
            | true, i -> Some i
            | _ -> None
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    if h.Length = 19 && h.[4] = '-' && h.[7] = '-' && h.[10] = '-' && h.[13] = '.' && h.[16] = '.' then
                        match pInt h.[0 .. 3], pInt h.[5 .. 6], pInt h.[8 .. 9], pInt h.[11 .. 12], pInt h.[14 .. 15], pInt h.[17 .. 18] with
                        | Some y, Some m, Some d, Some h, Some mi, Some s  ->
                            Seq.singleton ({ path with Segments = t }, System.DateTime(y, m, d, h, mi, s))
                        | _ -> Seq.empty
                    else Seq.empty
                | _ -> Seq.empty
            Write = fun d ->
                let pad2 (x: int) =
                    let s = string x
                    if s.Length = 1 then "0" + s else s
                let pad4 (x: int) =
                    let s = string x
                    match s.Length with
                    | 1 -> "000" + s
                    | 2 -> "00" + s
                    | 3 -> "0" + s
                    | _ -> s
                let s = 
                    pad4 d.Year + "-" + pad2 d.Month + "-" + pad2 d.Day
                    + "-" + pad2 d.Hour + "." + pad2 d.Minute + "." + pad2 d.Second
                Some (Seq.singleton (Route.Segment s))
        }
      
    let internal Tuple (readItems: obj -> obj[]) (createTuple: obj[] -> obj) (items: Router<obj>[]) =
        {
            Parse = fun path ->
                let rec collect elems path acc =
                    match elems with 
                    | [] -> Seq.singleton (path, createTuple (Array.ofList (List.rev acc)))
                    | h :: t -> h.Parse path |> Seq.collect(fun (p, a) -> collect t p (a :: acc))
                collect (List.ofArray items) path []
            Write = fun value ->
                let parts =
                    (readItems value, items) ||> Array.map2 (fun v r ->
                        r.Write v
                    )
                if Array.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }

    let internal JSTuple (items: Router<obj>[]) : Router<obj> =
        let readItems (value: obj) =
            Array.init items.Length (fun i ->
                (As<Array<obj>> value).[i]
            )
        Tuple readItems box items

    [<Inline>]
    let internal JSEmpty item = Router.Empty<obj>

    [<Inline>]
    let internal JSArray item = Router.Array item
    
    [<Inline>]
    let internal JSList item = Router.List item

    [<Inline>]
    let internal JSOption item = Router.Option item

    [<Inline>]
    let internal JSNullable item = Router.Nullable item

    [<Inline>]
    let internal JSQuery key item = Router.Query key item

    [<Inline>]
    let internal JSQueryOption key item = Router.QueryOption key item

    [<Inline>]
    let internal JSQueryNullable key item = Router.QueryNullable key item

    [<Inline>]
    let internal JSFormData item = Router.FormData item

    [<Inline>]
    let internal JSJson<'T when 'T: equality> = Router.Json<'T>

    [<Inline>]
    let internal JSBox item = Router.Box item

    [<Inline>]
    let internal JSDelayed getRouter = Router.Delay getRouter

    let internal Record (readFields: obj -> obj[]) (createRecord: obj[] -> obj) (fields: Router<obj>[]) =
        let fieldsList =  List.ofArray fields        
        {
            Parse = fun path ->
                let rec collect fields path acc =
                    match fields with 
                    | [] -> Seq.singleton (path, createRecord (Array.ofList (List.rev acc)))
                    | h :: t -> h.Parse path |> Seq.collect(fun (p, a) -> collect t p (a :: acc))
                collect fieldsList path []
            Write = fun value ->
                let parts =
                    (readFields value, fields) ||> Array.map2 (fun v r ->
                        r.Write v
                    )
                if Array.forall Option.isSome parts then
                    Some (parts |> Seq.collect Option.get)
                else None                      
        }
        
    let internal JSRecord (t: obj) (fields: (string * Router<obj>)[]) : Router<obj> =
        let readFields value =
            fields |> Array.map (fun (n, _) ->
                value?(n)
            )
        let createRecord fieldValues =
            let o = if isNull t then New [] else JS.New t
            (fields, fieldValues) ||> Array.iter2 (fun (n, _) v ->
                o?(n) <- v
            )
            o
        let fields = fields |> Array.map snd
        Record readFields createRecord fields
    
    let internal Union getTag readFields createCase (cases: (option<string> * string[] * Router<obj>[])[]) : Router<obj> =
        {
            Parse = fun path ->
                cases |> Seq.indexed |> Seq.collect (fun (i, (m, s, fields)) ->
                    let correctMethod =
                        match path.Method, m with
                        | Some pm, Some m -> pm = m
                        | _, Some _ -> false
                        | _ -> true
                    if correctMethod then
                        match path.Segments |> List.startsWith (List.ofArray s) with
                        | Some p -> 
                            match List.ofArray fields with
                            | [] -> Seq.singleton ({ path with Segments = p }, createCase i [||])
                            | fields -> 
                                let rec collect fields path acc =
                                    match fields with 
                                    | [] -> Seq.singleton (path, createCase i (Array.ofList (List.rev acc)))
                                    | h :: t -> h.Parse path |> Seq.collect(fun (p, a) -> collect t p (a :: acc))
                                collect fields { path with Segments = p } []
                        | None -> Seq.empty
                    else
                        Seq.empty
                )
            Write = fun value ->
                let tag = getTag value
                let method, path, fields = cases.[tag]
                match fields with
                | [||] -> Some (Seq.singleton (Route.Segment (List.ofArray path, method))) 
                | _ ->
                    let fieldParts =
                        (readFields tag value, fields) ||> Array.map2 (fun v f -> f.Write v)
                    if Array.forall Option.isSome fieldParts then
                        Some (Seq.append (Seq.singleton (Route.Segment (List.ofArray path, method))) (fieldParts |> Seq.collect Option.get))
                    else None                      
        }

    let internal JSUnion (t: obj) (cases: (option<obj> * option<string> * string[] * Router<obj>[])[]) : Router<obj> = 
        let getTag value = 
            let constIndex =
                cases |> Seq.tryFindIndex (
                    function
                    | Some c, _, _, _ -> value = c
                    | _ -> false
                )
            match constIndex with
            | Some i -> i
            | _ -> value?("$") 
        let readFields tag value =
            let _, _, _, fields = cases.[tag]
            Array.init fields.Length (fun i ->
                value?("$" + string i)
            )
        let createCase tag fieldValues =
            let o = if isNull t then New [] else JS.New t
            match cases.[tag] with
            | Some constant, _, _, _ -> constant
            | _ ->
                o?("$") <- tag
                fieldValues |> Seq.iteri (fun i v ->
                    o?("$" + string i) <- v
                )
                o
        let cases = cases |> Array.map (fun (_, m, p, r) -> m, p, r) 
        Union getTag readFields createCase cases

    let internal Class (readFields: obj -> obj[]) (createObject: obj[] -> obj) (partsAndFields: Choice<string, Router<obj>>[]) (subClasses: Router<obj>[]) =
        let partsAndFieldsList =  List.ofArray partsAndFields        
        let thisClass =
            {
                Parse = fun path ->
                    let rec collect fields path acc =
                        match fields with 
                        | [] -> Seq.singleton (path, createObject (Array.ofList (List.rev acc)))
                        | Choice1Of2 p :: t -> 
                            match path.Segments with
                            | pp :: pr when pp = p ->
                                collect t { path with Segments = pr } acc
                            | _ -> Seq.empty
                        | Choice2Of2 h :: t -> h.Parse path |> Seq.collect(fun (p, a) -> collect t p (a :: acc))
                    collect partsAndFieldsList path []
                Write = fun value ->
                    let fields = readFields value
                    let mutable index = -1
                    let parts =
                        partsAndFields |> Array.map (function
                            | Choice1Of2 p -> Some (Seq.singleton (Route.Segment(p)))
                            | Choice2Of2 r ->
                                index <- index + 1
                                r.Write(fields.[index])
                        )
                    if Array.forall Option.isSome parts then
                        parts |> Seq.collect Option.get |> Some
                    else None                      
            }
        if Array.isEmpty subClasses then
            thisClass
        else
            Router.Sum (Seq.append subClasses (Seq.singleton thisClass))

    let internal JSClass (ctor: unit -> obj) (partsAndFields: Choice<string, string * bool * Router<obj>>[]) (subClasses: Router<obj>[]) : Router<obj> =
        let fields =
            partsAndFields |> Seq.choose (fun p ->
                match p with
                | Choice1Of2 _ -> None
                | Choice2Of2 (fn, opt, _) -> Some (fn, opt)
            )
            |> Array.ofSeq
        let readFields value =
            fields |> Array.map (fun (fn, opt) ->
                if opt then
                    let v = value?(fn)
                    if v = JS.Undefined then box None else box (Some v)
                else
                    value?(fn)
            )
        let createObject fieldValues =
            let o = ctor()
            (fields, fieldValues) ||> Array.iter2 (fun (fn, opt) v ->
                if opt then
                    match As<option<obj>> v with
                    | None -> ()
                    | Some v ->
                        o?(fn) <- v
                else
                    o?(fn) <- v
            )
            o
        let partsAndFields =
            partsAndFields |> Array.map (fun p ->
                match p with
                | Choice1Of2 s -> Choice1Of2 s
                | Choice2Of2 (_, _, r) -> Choice2Of2 r
            )
        Class readFields createObject partsAndFields subClasses
