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
type Path =
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
        { Path.Empty with
            Segments = [ s ]
        }

    static member Segment s =
        { Path.Empty with
            Segments = s
        }

    static member Segment (s, m) =
        { Path.Empty with
            Segments = s
            Method = m
        }

    static member Combine (paths: seq<Path>) =
        let paths = Seq.toArray paths
        match paths.Length with
        | 1 -> paths.[0]
        | 0 -> Path.Empty
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

    static member FromUrl(path: string) =
        let s, q = 
            match path.IndexOf '?' with
            | -1 -> path, Map.empty
            | i -> 
                path.Substring(0, i),
                path.Substring(i + 1) |> Path.ParseQuery
        { Path.Empty with
            Segments = 
                s.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
            QueryArgs = q
        }

    [<JavaScript false>]
    static member FromRequest(r: System.Web.HttpRequestBase) =
        let u = r.Url
        let p = if u.IsAbsoluteUri then u.PathAndQuery else u.OriginalString
        { Path.FromUrl(p) with
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

    static member FromHash(path: string) =
        match path.IndexOf "#" with
        | -1 -> Path.Empty
        | i -> path.Substring(i) |> Path.FromUrl

    member this.ToLink() = PathUtil.WriteLink this.Segments this.QueryArgs

[<JavaScript>]
module internal List =
    let rec startsWith s l =
        match s, l with
        | [], _ -> Some l
        | sh :: sr, lh :: lr when sh = lh -> startsWith sr lr
        | _ -> None

type IRouter<'Action> =
    abstract Route : Http.Request -> option<'Action>
    abstract Link : 'Action -> option<Location>
    abstract Handles : 'Action -> bool

[<JavaScript>]
type Router =
    {
        Parse : Path -> Path seq
        Segment : seq<Path> 
    }
    
    static member Empty = 
        {
            Parse = fun path -> Seq.singleton path
            Segment = Seq.empty
        }

    static member FromString (name: string) =
        let parts = name.Split([| '/' |], System.StringSplitOptions.RemoveEmptyEntries)
        if Array.isEmpty parts then 
            Router.Empty 
        else
            let parts = List.ofArray parts
            {
                Parse = fun path ->
                    match path.Segments |> List.startsWith parts with
                    | Some p -> 
                        Seq.singleton ({ path with Segments = p })
                    | _ -> Seq.empty
                Segment = 
                    Seq.singleton (Path.Segment parts)
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
        Parse : Path -> (Path * 'T) seq
        Write : 'T -> option<seq<Path>> 
        //CanWrite : 'T -> bool 
    }
    
    static member (/) (before: Router<'T>, after: Router<'U>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect (fun (p, x) -> after.Parse p |> Seq.map (fun (p, y) -> (p, (x, y))))
            Write = fun (v1, v2) ->
                match before.Write v1, after.Write v2 with
                | Some p1, Some p2 -> Some (Seq.append p1 p2)
                | _ -> None
            //CanWrite = fun (v1, v2) -> before.CanWrite v1 && after.CanWrite v2
        }

    static member (/) (before: Router, after: Router<'T>) =
        {
            Parse = fun path ->
                before.Parse path |> Seq.collect after.Parse
            Write = fun v ->
                after.Write v |> Option.map (Seq.append before.Segment)
            //CanWrite = fun (v1, v2) -> before.CanWrite v1 && after.CanWrite v2
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

[<JavaScript>]
module Router =
    [<Inline>]
    let Shift (prefix: string) (router: Router<'A>) =
        prefix / router

    let Empty<'A when 'A: equality> : Router<'A> =
        {
            Parse = fun _ -> Seq.empty
            Write = fun _ -> None
        }

    /// For compatibility with old UI.Next.RouteMap.Create.
    let Create (ser: 'T -> list<string>) (des: list<string> -> 'T) =
        {
            Parse = fun path ->
                Seq.singleton ({ path with Segments = [] }, des path.Segments)
            Write = fun value ->
                Some (Seq.singleton (Path.Segment(ser value)))
        }
    
    /// Parses/writes a single value from a query argument with the given key instead of url path.
    let Query key (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Seq.empty
                | Some q -> 
                    let newQa = path.QueryArgs |> Map.remove key
                    item.Parse { Path.Empty with Segments = [ q ] }
                    |> Seq.map (fun (p, v) ->
                        { path with QueryArgs = newQa }, v
                    )
            Write = fun value ->
                item.Write value |> Option.map (fun p -> 
                    let p = Path.Combine p
                    match p.Segments with
                    | [ v ] -> Seq.singleton { Path.Empty with QueryArgs = Map.ofList [ key, v ] }
                    | _ -> Seq.empty
                )
        }

    type IOptionConverter =
        abstract Get: obj -> obj option
        abstract Some: obj -> obj

    type OptionConverter<'T>() =
        interface IOptionConverter with
            member this.Get (o: obj) = unbox<'T option> o |> Option.map box
            member this.Some (x: obj) = Some (unbox<'T> x) |> box

    let Method (m: string) : Router =
        {
            Parse = fun path ->
                match path.Method with
                | Some pm when pm = m -> Seq.singleton path
                | _ -> Seq.empty
            Segment =
                Seq.singleton { Path.Empty with Method = Some m }
        }

    let Body (deserialize: string -> option<'A>) (serialize: 'A -> string) : Router<'A> =
        {
            Parse = fun path ->
                match path.Body |> Option.bind deserialize with
                | Some b -> Seq.singleton ({ path with Body = None}, b)
                | _ -> Seq.empty
            Write = fun value ->
                Some <| Seq.singleton { Path.Empty with Body = Some (serialize value) }
        }

    let FormData (item: Router<'A>) : Router<'A> =
        {
            Parse = fun path ->
                match path.Body with
                | None -> item.Parse path
                | Some b ->
                    item.Parse { path with QueryArgs = path.QueryArgs |> Map.foldBack Map.add (Path.ParseQuery b); Body = None }
            Write = fun value ->
                item.Write value |> Option.map Path.Combine 
                |> Option.map (fun p -> Seq.singleton { p with QueryArgs = Map.empty; Body = Some (Path.WriteQuery p.QueryArgs) })  
        }
    
    let Parse path (router: Router<'A>) =
        router.Parse path
        |> Seq.tryPick (fun (path, value) -> if List.isEmpty path.Segments then Some value else None)

    let Write action (router: Router<'A>) =
        router.Write action |> Option.map Path.Combine 

    let TryLink action (router: Router<'A>) =
        match Write action router with
        | Some p -> Some (p.ToLink())
        | None -> None

    let Link action (router: Router<'A>) =
        match Write action router with
        | Some p -> p.ToLink()
        | None -> ""

    let Ajax action (router: Router<'A>) =
        match Write action router with
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
            failwith "Failed to map action to request" 

    let HashLink action (router: Router<'A>) =
        let h = (Link action router).Substring(1)
        if h = "" then "" else "#" + h
    
    let Slice (decode: 'T -> 'U option) (encode: 'U -> 'T) (router: Router<'T>) : Router<'U> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.choose (fun (p, v) -> decode v |> Option.map (fun v -> p, v)) 
            Write = fun value ->
                encode value |> router.Write
        }

    /// Maps a router to a wider router type. The enc function must return None, if the
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
    let Single<'T when 'T : equality> (action: 'T) (route: string) : Router<'T> =
        Router.FromString route |> MapTo action

    let Recursive<'T when 'T: equality> (createRouter: Router<'T> -> Router<'T>) : Router<'T> =
        let res = ref Unchecked.defaultof<Router<'T>>
        let r =
            {
                Parse = fun path -> res.Value.Parse path
                Write = fun value -> res.Value.Write value
            }
        createRouter r

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
                    Some (Seq.append (Seq.singleton (Path.Segment (string value.Length))) (parts |> Seq.collect Option.get))
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
                    Some (Seq.singleton (Path.Segment "null"))
                else item.Write value.Value
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
                | None -> Some (Seq.singleton (Path.Segment "None"))
                | Some v -> 
                    item.Write v |> Option.map (Seq.append (Seq.singleton (Path.Segment "None")))
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

type Router<'T when 'T: equality> with

    [<Inline>]
    member this.Link(action: 'T) =
        Router.Link action this

    [<Inline>]
    member this.TryLink(action: 'T) =
        Router.TryLink action this

    [<Inline>]
    member this.HashLink(action: 'T) =
        Router.HashLink action this

    [<Inline>]
    member this.Map(decode: System.Func<'T, 'U>, encode: System.Func<'U, 'T>) =
        Router.Map decode.Invoke encode.Invoke this

    [<Inline>]
    member this.Embed<'U when 'U: equality>(decode: System.Func<'T, 'U>, encode: System.Func<'U, option<'T>>) : Router<'U> =
        Router.Embed decode.Invoke encode.Invoke this

    [<Inline>]
    member this.Cast<'U when 'U: equality>() : Router<'U> =
        Router.Cast this

[<JavaScript>]
module RouterOperators =
    let rRoot : Router =
        {
            Parse = fun path ->
                match path.Segments with
                | [] -> Seq.singleton path
                | _ -> Seq.empty
            Segment = 
                Seq.empty
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
                Some (Seq.singleton (Path.Segment (if isNull value then "null" else encodeURIComponent value)))
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
                Some (Seq.singleton (Path.Segment (encodeURIComponent (string value))))
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
                Some (Seq.singleton (Path.Segment (string value)))
        }

    // TODO: fix translating trait call here
    ///// Parse/write a Guid.
    //let rGuid = rTryParse<System.Guid>()
    ///// Parse/write a bool.
    //let rBool = rTryParse<bool>()
    ///// Parse/write an int.
    //let rInt = rTryParse<int>()
    ///// Parse/write a double.
    //let rDouble = rTryParse<double>()

    //let rSByte = rTryParse<sbyte>() 
    //let rByte = rTryParse<byte>() 
    //let rInt16 = rTryParse<int16>() 
    //let rUInt16 = rTryParse<uint16>() 
    //let rUInt = rTryParse<uint32>() 
    //let rInt64 = rTryParse<int64>() 
    //let rUInt64 = rTryParse<uint64>() 
    //let rSingle = rTryParse<single>() 

    /// Parse/write a Guid.
    let rGuid : Router<System.Guid> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Guid.TryParse h with
                    | true, g ->
                        Seq.singleton ({ path with Segments = t }, g)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Path.Segment (string value)))
        }

    /// Parse/write a bool.
    let rBool : Router<bool> =
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
                Some (Seq.singleton (Path.Segment (if value then "True" else "False")))
        }

    /// Parse/write an int.
    let rInt : Router<int> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Int32.TryParse h with
                    | true, i ->
                        Seq.singleton ({ path with Segments = t }, i)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Path.Segment (string value)))
        }

    /// Parse/write a double.
    let rDouble : Router<float> =
        {
            Parse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Double.TryParse h with
                    | true, i ->
                        Seq.singleton ({ path with Segments = t }, i)
                    | _ -> Seq.empty
                | _ -> Seq.empty
            Write = fun value ->
                Some (Seq.singleton (Path.Segment (string value)))
        }

    /// Parses any remaining part of the URL as a string, no URL encode/decode is done.
    let rWildcard : Router<string> = 
        {
            Parse = fun path ->
                let s = path.Segments |> String.concat "/"
                Seq.singleton ({ path with Segments = [] }, s)
            Write = fun value ->
                Some (Seq.singleton (Path.Segment value))
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
                Some (Seq.singleton (Path.Segment s))
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
    let internal JSFormData key item = Router.Query key item |> Router.FormData

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
                | [||] -> Some (Seq.singleton (Path.Segment (List.ofArray path, method))) 
                | _ ->
                    let fieldParts =
                        (readFields tag value, fields) ||> Array.map2 (fun v f -> f.Write v)
                    if Array.forall Option.isSome fieldParts then
                        Some (Seq.append (Seq.singleton (Path.Segment (List.ofArray path, method))) (fieldParts |> Seq.collect Option.get))
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
                            | Choice1Of2 p -> Some (Seq.singleton (Path.Segment(p)))
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
