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
                if not (isNull r.InputStream) then 
                    use reader = new System.IO.StreamReader(r.InputStream)
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
                before.Parse path |> Seq.collect (fun p -> after.Parse p |> Seq.map (fun (p, y) -> (p, y)))
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

[<JavaScript false>]
type PathWriter =
    {
        mutable AddSlash : bool
        PathWriter : StringBuilder
        mutable QueryWriter : StringBuilder
    }

    static member New() =
        {
            AddSlash = true
            PathWriter = StringBuilder 128
            QueryWriter = null
        }

    member this.NextSegment() =
        if this.AddSlash then 
            this.PathWriter.Append('/')
        else 
            this.AddSlash <- true
            this.PathWriter

    member this.ToPath() =
        {
            Segments = [ this.PathWriter.ToString() ]
            QueryArgs = 
                let q = this.QueryWriter
                if isNull q then Map.empty else Path.ParseQuery (q.ToString())
            Method = None
            Body = None
        }

    member this.ToLink() =
        let p = this.PathWriter
        let q = this.QueryWriter
        if not (isNull q) then
            p.Append('?').Append(q.ToString()) |> ignore
        p.ToString()

[<JavaScript false>]
type InferredRouter =
    {
        IParse : Path -> (Path * obj) option
        IWrite : PathWriter * obj -> unit 
    }   

    member this.Link(action: 'T) =
        let w = PathWriter.New()
        this.IWrite(w, box action)
        w.ToLink()

    member this.Parse<'T>(path) =
        match this.IParse(path) with
        | Some (p, v) ->
            if List.isEmpty p.Segments then Some (unbox<'T> v) else None
        | None -> None

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

    [<JavaScript false>]
    let IEmpty : InferredRouter =
        {
            IParse = fun _ -> None
            IWrite = ignore
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

    [<JavaScript false>]
    let IQuery key (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                path.QueryArgs.TryFind key
                |> Option.bind (fun q ->
                    item.IParse { Path.Empty with Segments = [ q ] }
                    |> Option.map (fun (p, v) ->
                        let newQa = path.QueryArgs |> Map.remove key
                        { path with QueryArgs = newQa }, v
                    )
                )
            IWrite = fun (w, value) ->
                let q = 
                    match w.QueryWriter with
                    | null ->
                        let q = StringBuilder 128
                        w.QueryWriter <- q
                        q
                    | q -> q.Append('&')
                w.QueryWriter.Append(key).Append('=') |> ignore
                let qw = { w with PathWriter = q; AddSlash = false }
                item.IWrite (qw, value)
        }

    type IOptionConverter =
        abstract Get: obj -> obj option
        abstract Some: obj -> obj

    type OptionConverter<'T>() =
        interface IOptionConverter with
            member this.Get (o: obj) = unbox<'T option> o |> Option.map box
            member this.Some (x: obj) = Some (unbox<'T> x) |> box

    [<JavaScript false>]
    let IQueryOption (itemType: System.Type) key (item: InferredRouter) : InferredRouter =
        let converter = 
            System.Activator.CreateInstance(typedefof<OptionConverter<_>>.MakeGenericType(itemType))
            :?> IOptionConverter
        {
            IParse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Some (path, null)
                | Some q ->
                    item.IParse { Path.Empty with Segments = [ q ] }
                    |> Option.map (fun (p, v) ->
                        let newQa = path.QueryArgs |> Map.remove key
                        { path with QueryArgs = newQa }, converter.Some v
                    )
            IWrite = fun (w, value) ->
                match converter.Get value with
                | None -> ()
                | Some v ->
                let q = 
                    match w.QueryWriter with
                    | null ->
                        let q = StringBuilder 128
                        w.QueryWriter <- q
                        q
                    | q -> q.Append('&')
                w.QueryWriter.Append(key).Append('=') |> ignore
                let qw = { w with PathWriter = q; AddSlash = false }
                item.IWrite (qw, v)
        }

    [<JavaScript false>]
    let IQueryNullable key (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                match path.QueryArgs.TryFind key with
                | None -> Some (path, null)
                | Some q ->
                    item.IParse { Path.Empty with Segments = [ q ] }
                    |> Option.map (fun (p, v) ->
                        let newQa = path.QueryArgs |> Map.remove key
                        { path with QueryArgs = newQa }, v
                    )
            IWrite = fun (w, value) ->
                match value with
                | null -> ()
                | v ->
                let q = 
                    match w.QueryWriter with
                    | null ->
                        let q = StringBuilder 128
                        w.QueryWriter <- q
                        q
                    | q -> q.Append('&')
                w.QueryWriter.Append(key).Append('=') |> ignore
                let qw = { w with PathWriter = q; AddSlash = false }
                item.IWrite (qw, v)
        }

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

    [<JavaScript false>]
    let IBody (deserialize: string -> option<obj>) : InferredRouter =
        {
            IParse = fun path ->
                match path.Body |> Option.bind deserialize with
                | Some b -> Some ({ path with Body = None}, b)
                | _ -> None
            IWrite = ignore
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
    
    [<JavaScript false>]
    let IFormData (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                match path.Body with
                | None -> item.IParse path
                | Some b ->
                    item.IParse { path with QueryArgs = path.QueryArgs |> Map.foldBack Map.add (Path.ParseQuery b); Body = None }
            IWrite = ignore
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

    let Ajax path =
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

    [<Inline>]
    let AjaxJson<'T> path =
        async {
            let! res = Ajax path
            return Json.Deserialize<'T> res
        }

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

    let IMap (decode: obj -> obj) (encode: obj -> obj) router =
        {
            IParse = fun path ->
                router.IParse path |> Option.map (fun (p, v) -> p, decode v) 
            IWrite = fun (w, value) ->
                router.IWrite(w, encode value)
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

    [<JavaScript false>]
    let internal BoxUnsafe (router: Router<'A>): Router<obj> =
        {
            Parse = fun path ->
                router.Parse path |> Seq.map (fun (p, v) -> p, box v) 
            Write = fun value ->
                unbox value |> router.Write
        }

    [<JavaScript false>]
    let JsonDyn<'T when 'T: equality> : InferredRouter =
        IBody (fun s -> try Some (Json.Deserialize<'T> s |> box) with _ -> None)

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

    [<JavaScript false>]
    let internal UnboxUnsafe<'A when 'A: equality> (router: InferredRouter) : Router<'A> =
        {
            Parse = fun path ->
                match router.IParse path with
                | Some (p, v) -> Seq.singleton (p, unbox v)
                | _ -> Seq.empty
            Write = fun value ->
                let w = PathWriter.New()
                router.IWrite(w, box value)
                w.ToPath() |> Seq.singleton |> Some
        }

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
    
    let Table<'T when 'T : equality> (mapping: seq<'T * string>) : Router<'T> =
        mapping |> Seq.map (fun (v, s) -> Router.FromString s |> MapTo v) |> Sum 

    let Recursive<'T when 'T: equality> (createRouter: Router<'T> -> Router<'T>) : Router<'T> =
        let res = ref Unchecked.defaultof<Router<'T>>
        let r =
            {
                Parse = fun path -> res.Value.Parse path
                Write = fun value -> res.Value.Write value
            }
        createRouter r

    let Delayed<'T when 'T: equality> (getRouter: unit -> Router<'T>) : Router<'T> =
        {
            Parse = fun path -> getRouter().Parse path
            Write = fun value -> getRouter().Write value
        }

    let IDelayed (getRouter: unit -> InferredRouter) : InferredRouter =
        {
            IParse = fun path -> getRouter().IParse path
            IWrite = fun (w, value) -> getRouter().IWrite(w, value)
        }

    [<JavaScript false>]
    let internal ArrayDyn (itemType: System.Type) (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.Int32.TryParse h with
                    | true, l ->
                        let arr = System.Array.CreateInstance(itemType, l)
                        let rec collect i path =
                            if i = l then 
                                Some (path, box arr)
                            else 
                                match item.IParse path with 
                                | Some (p, a) -> 
                                    arr.SetValue(a, i)
                                    collect (i + 1) p
                                | None -> None
                        collect 0 { path with Segments = t }
                    | _ -> None
                | _ -> None
            IWrite = fun (w, value) ->
                let arr = value :?> System.Array
                let l = arr.Length 
                w.NextSegment().Append(arr.Length) |> ignore
                for i = 0 to l - 1 do
                    item.IWrite (w, arr.GetValue i) 
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

    [<JavaScript false>]
    let NullableDyn (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                match path.Segments with
                | "null" :: p -> 
                    Some ({ path with Segments = p }, null)
                | _ ->
                    item.IParse path
            IWrite = fun (w, value) ->
                if isNull value then 
                    w.NextSegment().Append("null") |> ignore
                else
                    item.IWrite(w, value)
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

    [<JavaScript false>]
    let ListDyn (itemType: System.Type) (item: InferredRouter) : InferredRouter = 
        let converter = 
            System.Activator.CreateInstance(typedefof<ListArrayConverter<_>>.MakeGenericType(itemType))
            :?> IListArrayConverter
        ArrayDyn itemType item |> IMap converter.OfArray converter.ToArray

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
                Some (Seq.singleton (Path.Segment (string value)))
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
    let internal JSDelayed getRouter = Router.Delayed getRouter

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
    
    let internal Union getTag readFields createCase (cases: (string[] * Router<obj>[])[]) : Router<obj> =
        {
            Parse = fun path ->
                cases |> Seq.indexed |> Seq.collect (fun (i, (s, fields)) ->
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
                )
            Write = fun value ->
                let tag = getTag value
                let path, fields = cases.[tag]
                match fields with
                | [||] -> Some (Seq.singleton (Path.Segment (List.ofArray path))) 
                | _ ->
                    let path, fields = cases.[tag]
                    let fieldParts =
                        (readFields tag value, fields) ||> Array.map2 (fun v f -> f.Write v)
                    if Array.forall Option.isSome fieldParts then
                        Some (Seq.append (Seq.singleton (Path.Segment (List.ofArray path))) (fieldParts |> Seq.collect Option.get))
                    else None                      
        }

    let internal JSUnion (t: obj) (cases: (option<obj> * string[] * Router<obj>[])[]) : Router<obj> = 
        let getTag value = 
            let constIndex =
                cases |> Seq.tryFindIndex (
                    function
                    | Some c, _, _ -> value = c
                    | _ -> false
                )
            match constIndex with
            | Some i -> i
            | _ -> value?("$") 
        let readFields tag value =
            let _, _, fields = cases.[tag]
            Array.init fields.Length (fun i ->
                value?("$" + string i)
            )
        let createCase tag fieldValues =
            let o = if isNull t then New [] else JS.New t
            match cases.[tag] with
            | Some constant, _, _ -> constant
            | _ ->
                o?("$") <- tag
                fieldValues |> Seq.iteri (fun i v ->
                    o?("$" + string i) <- v
                )
                o
        let cases = cases |> Array.map (fun (_, p, r) -> p, r) 
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

module internal ServerInferredOperators =
    open RouterOperators

    let internal iString : InferredRouter =
        {
            IParse = fun path ->
                match path.Segments with
                | h :: t -> 
                    Some ({ path with Segments = t }, decodeURIComponent h |> box)
                | _ -> None
            IWrite = fun (w, value) ->
                if isNull value then 
                    w.NextSegment().Append("null") |> ignore
                else
                    w.NextSegment().Append(encodeURIComponent (unbox value)) |> ignore
        }

    let internal iChar : InferredRouter =
        {
            IParse = fun path ->
                match path.Segments with
                | h :: t when h.Length = 1 -> 
                    Some ({ path with Segments = t }, char (decodeURIComponent h) |> box)
                | _ -> None
            IWrite = fun (w, value) ->
                w.NextSegment().Append(encodeURIComponent (string value)) |> ignore
        }

    let inline iTryParse< ^T when ^T: (static member TryParse: string * byref< ^T> -> bool) and ^T: equality>() =
        {
            IParse = fun path ->
                match path.Segments with
                | h :: t -> 
                    let mutable res = Unchecked.defaultof< ^T>
                    let ok = (^T: (static member TryParse: string * byref< ^T> -> bool) (h, &res))
                    if ok then 
                        Some ({ path with Segments = t }, box res)
                    else None
                | _ -> None
            IWrite = fun (w, value) ->
                w.NextSegment().Append(value) |> ignore
        }

    /// Parse/write a Guid.
    let iGuid = iTryParse<System.Guid>()
    /// Parse/write a bool.
    let iBool = iTryParse<bool>()
    /// Parse/write an int.
    let iInt = iTryParse<int>()
    /// Parse/write a double.
    let iDouble = iTryParse<double>()

    let iDateTime format =
        let format = defaultArg format "yyyy-MM-dd-HH.mm.ss"
        {
            IParse = fun path ->
                match path.Segments with
                | h :: t -> 
                    match System.DateTime.TryParseExact(h, format, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.RoundtripKind) with
                    | true, d ->
                        Some ({ path with Segments = t }, box d)
                    | _ -> None
                | _ -> None
            IWrite = fun (w, value) ->                
                w.NextSegment().Append((value:?> System.DateTime).ToString(format)) |> ignore
        }

    let iWildcardString = 
        {
            IParse = fun path ->
                let s = path.Segments |> String.concat "/"
                Some ({ path with Segments = [] }, box s)
            IWrite = fun (w, value) ->
                w.NextSegment().Append(value) |> ignore
        }

    let iWildcardArray (itemType: System.Type) (item: InferredRouter) = 
        {
            IParse = fun path ->
                let acc = ResizeArray()
                let rec loop p =
                    match p.Segments with
                    | [] -> 
                        let arr = System.Array.CreateInstance(itemType, acc.Count)
                        for i = 0 to acc.Count - 1 do
                            arr.SetValue(acc.[i], i)
                        Some (p, box arr)
                    | _ -> 
                        match item.IParse(p) with
                        | Some (np, o) ->
                            acc.Add(o)
                            loop np
                        | None -> None
                loop path
            IWrite = fun (w, value) ->
                let arr = value :?> System.Array
                let l = arr.Length 
                for i = 0 to l - 1 do
                    item.IWrite (w, arr.GetValue i) 
        }

    let iWildcardList (itemType: System.Type) (item: InferredRouter) : InferredRouter = 
        let converter = 
            System.Activator.CreateInstance(typedefof<Router.ListArrayConverter<_>>.MakeGenericType(itemType))
            :?> Router.IListArrayConverter
        iWildcardArray itemType item |> Router.IMap converter.OfArray converter.ToArray

    let internal ITuple (readItems: obj -> obj[]) (createTuple: obj[] -> obj) (items: InferredRouter[]) =
        let itemsList = List.ofArray items
        let l = items.Length
        {
            IParse = fun path ->
                let arr = Array.zeroCreate l 
                let rec collect i elems path =
                    match elems with 
                    | [] -> Some (path, createTuple arr)
                    | h :: t -> 
                        match h.IParse path with
                        | Some (p, a) -> 
                            arr.[i] <- a
                            collect (i + 1) t p
                        | _ -> None
                collect 0 itemsList path
            IWrite = fun (w, value) ->
                let values = readItems value
                for i = 0 to items.Length - 1 do
                    items.[i].IWrite (w, values.[i]) 
        }

    let internal IRecord (readFields: obj -> obj[]) (createRecord: obj[] -> obj) (fields: InferredRouter[]) =
        let fieldsList =  List.ofArray fields        
        let l = fields.Length
        {
            IParse = fun path ->
                let arr = Array.zeroCreate l
                let rec collect i fields path =
                    match fields with 
                    | [] -> Some (path, createRecord arr)
                    | h :: t -> 
                        match h.IParse path with
                        | Some (p, a) -> 
                            arr.[i] <- a
                            collect (i + 1) t p
                        | None -> None
                collect 0 fieldsList path
            IWrite = fun (w, value) ->
                (readFields value, fields) ||> Array.iter2 (fun v r ->
                    r.IWrite(w, v)
                )
        }

    let internal IUnion getTag (caseReaders: _[]) (caseCtors: _[]) (cases: (option<string> * string[] * InferredRouter[])[]) : InferredRouter =
        let lookupCases =
            cases |> Seq.mapi (fun i (m, s, fields) -> 
                let fieldList = List.ofArray fields
                let l = fields.Length
                let parseFields p path =
                    let arr = Array.zeroCreate l
                    let rec collect j f path =
                        match f with 
                        | [] -> 
                            Some (path, caseCtors.[i] arr)
                        | h :: t -> 
                            match h.IParse path with
                            | Some (p, a) -> 
                                arr.[j] <- a
                                collect (j + 1) t p
                            | None -> None
                    collect 0 fieldList { path with Segments = p }
                let s = List.ofArray s
                m,
                match s with
                | [] -> 
                    "",
                    match fieldList with
                    | [] ->
                        let c = caseCtors.[i] [||]
                        -1,
                        fun p path -> Some (path, c)
                    | _ ->
                        fieldList.Length - 1, parseFields
                | [ h ] ->
                    h, 
                    match fieldList with
                    | [] ->
                        let c = caseCtors.[i] [||]
                        0,
                        fun p path -> Some ({ path with Segments = p }, c)
                    | _ ->
                        fieldList.Length, parseFields
                | h :: t ->
                    h, 
                    match fieldList with
                    | [] ->
                        let c = caseCtors.[i] [||]
                        t.Length,
                        fun p path -> Some ({ path with Segments = p }, c)
                    | _ ->
                        t.Length + fieldList.Length,
                        fun p path ->
                            match p |> List.startsWith t with
                            | Some p -> parseFields p path
                            | None -> None
            ) 
            // group by method
            |> Seq.groupBy fst |> Seq.map (fun (m, mcases) ->
                m,
                mcases |> Seq.map snd |> Seq.groupBy fst
                |> Seq.map (fun (h, hcases) ->
                    h, 
                    match hcases |> Seq.map snd |> List.ofSeq with
                    | [ _, parse ] -> parse
                    | parsers ->
                        // this is just an approximation, start with longer parsers
                        let parsers = parsers |> Seq.sortByDescending fst |> Seq.map snd |> Array.ofSeq 
                        fun p path ->
                            parsers |> Array.tryPick (fun parse -> parse p path)                        
                )
                |> dict 
            )
            |> dict
        let writeCases =
            cases |> Array.map (fun (_, s, fields) -> 
                String.concat "/" s, fields
            )
        {
            IParse = 
                match lookupCases.TryGetValue(None) with
                | true, lookup when lookupCases.Count = 1 -> 
                    // no union case specifies a method
                    fun path ->
                        match path.Segments with
                        | [] -> 
                            match lookup.TryGetValue("") with
                            | true, parse -> parse [] path
                            | _ -> None
                        | h :: t ->
                            match lookup.TryGetValue(h) with
                            | true, parse -> parse t path
                            | _ -> None
                | _ ->
                    // some union case specifies a method
                    let ignoreMethodLookup =
                        match lookupCases.TryGetValue(None) with
                        | true, lookup -> lookup
                        | _ -> dict []
                    fun path ->
                        let explicit =
                            match lookupCases.TryGetValue(path.Method) with
                            | true, lookup -> 
                                match path.Segments with
                                | [] -> 
                                    match lookup.TryGetValue("") with
                                    | true, parse -> parse [] path
                                    | _ -> None
                                | h :: t ->
                                    match lookup.TryGetValue(h) with
                                    | true, parse -> parse t path
                                    | _ -> None
                            | _ -> None
                        if Option.isSome explicit then explicit else
                        // not found with explicit method, fall back to cases ignoring method
                        match path.Segments with
                        | [] -> 
                            match ignoreMethodLookup.TryGetValue("") with
                            | true, parse -> parse [] path
                            | _ -> None
                        | h :: t ->
                            match ignoreMethodLookup.TryGetValue(h) with
                            | true, parse -> parse t path
                            | _ -> None
            IWrite = fun (w, value) ->
                let tag = getTag value
                let path, fields = writeCases.[tag]
                if path <> "" then
                    w.NextSegment().Append(path) |> ignore
                match fields with
                | [||] -> ()
                | _ ->
                    let values = caseReaders.[tag] value : _[]
                    for i = 0 to fields.Length - 1 do
                        fields.[i].IWrite (w, values.[i]) 
        }

    let internal IClass (readFields: obj -> obj[]) (createObject: obj[] -> obj) (partsAndFields: Choice<string, InferredRouter>[]) (subClasses: (System.Type * InferredRouter)[]) =
        let partsAndFieldsList =  List.ofArray partsAndFields        
        let thisClass =
            {
                IParse = fun path ->
                    let rec collect fields path acc =
                        match fields with 
                        | [] -> Some (path, createObject (Array.ofList (List.rev acc)))
                        | Choice1Of2 p :: t -> 
                            match path.Segments with
                            | pp :: pr when pp = p ->
                                collect t { path with Segments = pr } acc
                            | _ -> None
                        | Choice2Of2 h :: t -> h.IParse path |> Option.bind (fun (p, a) -> collect t p (a :: acc))
                    collect partsAndFieldsList path []
                IWrite = fun (w, value) ->
                    let fields = readFields value
                    let mutable index = -1
                    partsAndFields |> Array.iter (function
                        | Choice1Of2 p -> w.PathWriter.Append(p) |> ignore
                        | Choice2Of2 r ->
                            index <- index + 1
                            r.IWrite(w, fields.[index])
                    )
            }
        if Array.isEmpty subClasses then
            thisClass
        else
            { thisClass with
                IWrite = fun (w, value) ->
                    let t = value.GetType()
                    let sub = subClasses |> Array.tryPick (fun (st, sr) -> if st = t then Some sr else None)
                    match sub with 
                    | Some s -> s.IWrite (w, value)
                    | _ -> thisClass.IWrite (w, value)
            }
