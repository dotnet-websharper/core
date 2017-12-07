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

module internal ServerInferredOperators =

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

    open RouterOperators

    let IEmpty : InferredRouter =
        {
            IParse = fun _ -> None
            IWrite = ignore
        }

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

    let IMap (decode: obj -> obj) (encode: obj -> obj) router =
        {
            IParse = fun path ->
                router.IParse path |> Option.map (fun (p, v) -> p, decode v) 
            IWrite = fun (w, value) ->
                router.IWrite(w, encode value)
        }

    let iWildcardList (itemType: System.Type) (item: InferredRouter) : InferredRouter = 
        let converter = 
            System.Activator.CreateInstance(typedefof<Router.ListArrayConverter<_>>.MakeGenericType(itemType))
            :?> Router.IListArrayConverter
        iWildcardArray itemType item |> IMap converter.OfArray converter.ToArray

    let INullable (item: InferredRouter) : InferredRouter =
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

    let IQueryOption (itemType: System.Type) key (item: InferredRouter) : InferredRouter =
        let converter = 
            System.Activator.CreateInstance(typedefof<Router.OptionConverter<_>>.MakeGenericType(itemType))
            :?> Router.IOptionConverter
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

    let IUnbox<'A when 'A: equality> (router: InferredRouter) : Router<'A> =
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

    let IBody (deserialize: string -> option<obj>) : InferredRouter =
        {
            IParse = fun path ->
                match path.Body |> Option.bind deserialize with
                | Some b -> Some ({ path with Body = None}, b)
                | _ -> None
            IWrite = ignore
        }

    let IJson<'T when 'T: equality> : InferredRouter =
        IBody (fun s -> try Some (Json.Deserialize<'T> s |> box) with _ -> None)

    let IFormData (item: InferredRouter) : InferredRouter =
        {
            IParse = fun path ->
                match path.Body with
                | None -> item.IParse path
                | Some b ->
                    item.IParse { path with QueryArgs = path.QueryArgs |> Map.foldBack Map.add (Path.ParseQuery b); Body = None }
            IWrite = ignore
        }

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

    let IDelayed (getRouter: unit -> InferredRouter) : InferredRouter =
        let r = lazy getRouter()
        {
            IParse = fun path -> r.Value.IParse path
            IWrite = fun (w, value) -> r.Value.IWrite(w, value)
        }

    let internal IArray (itemType: System.Type) (item: InferredRouter) : InferredRouter =
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

    let IList (itemType: System.Type) (item: InferredRouter) : InferredRouter = 
        let converter = 
            System.Activator.CreateInstance(typedefof<Router.ListArrayConverter<_>>.MakeGenericType(itemType))
            :?> Router.IListArrayConverter
        IArray itemType item |> IMap converter.OfArray converter.ToArray

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
            (cases, caseReaders) ||> Array.map2 (fun (_, s, fields) reader -> 
                String.concat "/" s, fields, reader
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
                let path, fields, reader = writeCases.[tag]
                if path <> "" then
                    w.NextSegment().Append(path) |> ignore
                match fields with
                | [||] -> ()
                | _ ->
                    let values = reader value : _[]
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
