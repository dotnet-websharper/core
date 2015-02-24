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

module WebSharper.Sitelets.ActionEncoding

open System
open System.Collections.Generic
open WebSharper.Core

type DecodeResult<'Action> =
    | Success of 'Action
    | InvalidMethod of 'Action * ``method``: string
    | InvalidJson of 'Action
    | MissingQueryParameter of 'Action * queryParam: string

    member this.Action =
        match this with
        | Success a
        | InvalidMethod (a, _)
        | InvalidJson a
        | MissingQueryParameter (a, _) -> a

type DecodeResult =

    static member map (f: 'a -> 'b) (x: option<DecodeResult<'a>>) : option<DecodeResult<'b>> =
        x |> Option.map (function
            | Success x -> Success (f x)
            | InvalidMethod (x, m) -> InvalidMethod (f x, m)
            | InvalidJson x -> InvalidJson (f x)
            | MissingQueryParameter (x, p) -> MissingQueryParameter (f x, p))

    static member unbox (x: DecodeResult<obj>) : DecodeResult<'b> =
        match x with
        | Success x -> Success (unbox x)
        | InvalidMethod (x, m) -> InvalidMethod (unbox x, m)
        | InvalidJson x -> InvalidJson (unbox x)
        | MissingQueryParameter (x, p) -> MissingQueryParameter (unbox x, p)

let (>>=) (x: option<DecodeResult<'a>>) (f: 'a -> option<DecodeResult<'b>>) : option<DecodeResult<'b>> =
    match x with
    | None -> None
    | Some (Success x) -> f x
    | Some (InvalidMethod (x, m)) ->
        f x |> Option.map (function
            | InvalidMethod (y, m) -> InvalidMethod (y, m)
            | r -> InvalidMethod (r.Action, m))
    | Some (InvalidJson x) ->
        f x |> Option.map (function
            | Success y -> InvalidJson y
            | r -> r)
    | Some (MissingQueryParameter (x, p)) ->
        f x |> Option.map (function
            | Success y -> MissingQueryParameter (y, p)
            | r -> r)

let isUnreserved isLast c =
    match c with
    | '-' | '_' -> true
    | '.' -> not isLast
    | c when c >= 'A' && c <= 'Z' -> true
    | c when c >= 'a' && c <= 'z' -> true
    | c when c >= '0' && c <= '9' -> true
    | _ -> false

let writeEscaped (w: System.Text.StringBuilder) isLast c =
    let k = int c
    if isUnreserved isLast c then w.Append c
    elif k < 256 then w.AppendFormat("~{0:x2}", k)
    else w.AppendFormat("~u{0:x4}", k)
    |> ignore

exception NoFormatError of System.Type with
    override this.ToString() =
        System.String.Format(
            "Could not derive URL formatting for: {0}", this.Data0)

let inline ( ++ ) (a: int) (b: int) = (a <<< 4) + b

[<Literal>]
let EOF = -1

[<Literal>]
let ERROR = -2

let readEscaped (r: System.IO.TextReader) =
    let hex x =
        match x with
        | x when x >= int '0' && x <= int '9' -> x - int '0'
        | x when x >= int 'a' && x <= int 'f' -> x - int 'a' + 10
        | x when x >= int 'A' && x <= int 'F' -> x - int 'A' + 10
        | _ -> ERROR
    match r.Read() with
    | x when x = int '~' ->
        match r.Read() with
        | x when x = int 'u' ->
            let a = r.Read()
            let b = r.Read()
            let c = r.Read()
            let d = r.Read()
            if a >= 0 && b >= 0 && c >= 0 && d >= 0 then
                hex a ++ hex b ++ hex c ++ hex d
            else ERROR
        | x ->
            let y = r.Read()
            if x >= 0 && y >= 0 then
                hex x ++ hex y
            else ERROR
    | x ->
        x

type System.Text.StringBuilder with

    member this.Add(c: char) =
        this.Append c |> ignore

    member this.Add(s: string) =
        this.Append s |> ignore

    member this.Flush() =
        let s = this.ToString()
        this.Remove(0, this.Length) |> ignore
        s

type Reflection.UnionCaseInfo with
    member this.CustomizedName =
        let aT = typeof<CompiledNameAttribute>
        match this.GetCustomAttributes aT with
        | [| :? CompiledNameAttribute as attr |] -> attr.CompiledName
        | _ -> this.Name

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic
    ||| System.Reflection.BindingFlags.Static
    ||| System.Reflection.BindingFlags.Instance

type ISequenceProcessor =
    abstract member ToSequence : obj -> seq<obj>
    abstract member FromSequence : seq<obj> -> obj

type ListProcessor<'T>() =
    interface ISequenceProcessor with
        member this.ToSequence (x: obj) = Seq.map box (x :?> list<'T>)
        member this.FromSequence (s: seq<obj>) = box [for x in s -> x :?> 'T]

type S =
    {
        Write : System.Text.StringBuilder -> List<string * string> -> obj -> bool
        WritesToUrlPath : unit -> bool
    }
    static member Make(writesToPath, write) =
        {
            Write = write
            WritesToUrlPath = writesToPath
        }
    static member Make(writesToPath, write) =
        {
            Write = write
            WritesToUrlPath = fun () -> writesToPath
        }

/// Dict<type, (encode * decode)>
let primitiveValueHandlers =
    let opt f s =
        match f s with
        | true, x -> Some (box x)
        | false, _ -> None
    let d = Dictionary()
    d.Add(typeof<bool>,
        (opt System.Boolean.TryParse, string))
    d.Add(typeof<int>,
        (opt System.Int32.TryParse, string))
    d.Add(typeof<float>,
        (opt System.Double.TryParse, string))
    d.Add(typeof<string>,
        ((fun s ->
            let buf = System.Text.StringBuilder()
            use i = new System.IO.StringReader(s)
            let rec loop () =
                match readEscaped i with
                | ERROR -> None
                | EOF -> Some (box (buf.Flush()))
                | x -> buf.Add (char x); loop ()
            loop ()),
         (fun (x: obj) ->
            match x :?> string with
            | null -> ""
            | s ->
                let b = System.Text.StringBuilder()
                s |> Seq.iteri (fun i c ->
                    writeEscaped b (i + 1 = s.Length) c)
                b.Flush())))
    d.Add(typeof<DateTime>,
        ((let rT = System.Globalization.DateTimeStyles.RoundtripKind
          opt <| fun x -> DateTime.TryParse(x, null, rT)),
         (fun (x: obj) -> (x :?> DateTime).ToString "o")))
    d

let isPrimitive t =
    primitiveValueHandlers.ContainsKey t

let isPrimitiveOption (t: System.Type) =
    t.IsGenericType &&
    t.GetGenericTypeDefinition() = typedefof<option<_>> &&
    isPrimitive (t.GetGenericArguments().[0])

type Field =
    | RecordField of System.Reflection.PropertyInfo
    | UnionCaseField of Reflection.UnionCaseInfo * System.Reflection.PropertyInfo

    member private this.Property =
        match this with
        | RecordField pi
        | UnionCaseField (_, pi) -> pi

    member this.Type = this.Property.PropertyType

    member this.DeclaringType = this.Property.DeclaringType

    member this.Name =
        let aT = typeof<WebSharper.Core.Attributes.NameAttribute>
        let p = this.Property
        let customName =
            p.GetCustomAttributesData()
            |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = aT then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        defaultArg customName p.Name

let isQueryParam = function
    | RecordField f ->
        f.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<QueryAttribute> &&
            cad.ConstructorArguments.Count = 0)
    | UnionCaseField (uci, f) as ff ->
        uci.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<QueryAttribute> &&
            cad.ConstructorArguments.Count = 1 &&
            cad.ConstructorArguments.[0].Value
            :?> System.Collections.ObjectModel.ReadOnlyCollection<
                    System.Reflection.CustomAttributeTypedArgument>
            |> Seq.exists (fun a -> a.Value :?> string = ff.Name))

let getUnionCaseJsonArgumentName (c: Reflection.UnionCaseInfo) =
    c.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<JsonAttribute> then
            Some (cad.ConstructorArguments.[0].Value :?> string)
        else None
    )

let isJson = function
    | RecordField f ->
        f.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<JsonAttribute> &&
            cad.ConstructorArguments.Count = 0)
    | UnionCaseField (uci, f) as ff ->
        uci.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<JsonAttribute> &&
            cad.ConstructorArguments.Count = 1 &&
            cad.ConstructorArguments.[0].Value :?> string = ff.Name)

let (|Enum|Array|List|Tuple|Record|Union|Other|) (t: System.Type) =
    if t.IsEnum then
        Enum (System.Enum.GetUnderlyingType(t))
    elif t.IsArray then
        Array (t.GetElementType())
    elif t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>> then
        List (t.GetGenericArguments().[0])
    elif Reflection.FSharpType.IsTuple t then
        let e = Reflection.FSharpType.GetTupleElements t
        let r = Reflection.FSharpValue.PreComputeTupleReader t
        let c = Reflection.FSharpValue.PreComputeTupleConstructor t
        Tuple (e, (r : obj -> obj[]), c)
    elif Reflection.FSharpType.IsRecord t then
        let fs = Reflection.FSharpType.GetRecordFields t
        let r = Reflection.FSharpValue.PreComputeRecordReader(t, flags)
        let c = Reflection.FSharpValue.PreComputeRecordConstructor(t, flags)
        Record (fs, (r : obj -> obj[]), c)
    elif Reflection.FSharpType.IsUnion t then
        let cs = Reflection.FSharpType.GetUnionCases(t, flags)
        let tR = Reflection.FSharpValue.PreComputeUnionTagReader(t, flags)
        let uR x = Reflection.FSharpValue.PreComputeUnionReader(x, flags)
        let uC x = Reflection.FSharpValue.PreComputeUnionConstructor(x, flags)
        Union (cs, (tR : obj -> int), (uR : _ -> obj -> obj[]), uC)
    else Other t

let writePrimitiveType (t: System.Type) =
    match primitiveValueHandlers.TryGetValue t with
    | false, _ -> raise (NoFormatError t)
    | true, (_, w) -> w

/// ot is known to be option<X>
let getSome (ot: System.Type) =
    let uci = Reflection.FSharpType.GetUnionCases(ot).[1]
    let r = Reflection.FSharpValue.PreComputeUnionReader(uci)
    fun x -> (r x).[0]

let writeQueryParam (f: Field) : S option =
    if isQueryParam f then
        let ft = f.Type
        if isPrimitive ft then
            let wp = writePrimitiveType ft
            S.Make(false, fun w (q: List<string * string>) x ->
                q.Add((f.Name, wp x))
                true)
            |> Some
        elif isPrimitiveOption ft then
            let wp = writePrimitiveType (ft.GetGenericArguments().[0])
            let getSome = getSome ft
            S.Make(false, fun w (q: List<string * string>) x ->
                match x with
                | null -> ()
                | x -> q.Add((f.Name, wp (getSome x)))
                true)
            |> Some
        else None
    else None

let writeField (getS: System.Type -> S) (f: Field) : S =
    if isJson f then
        S.Make(false, fun _ _ _ -> true)
    else
        match writeQueryParam f with
        | Some w -> w
        | None -> getS f.Type

let getS (getS: System.Type -> S) (t: System.Type) : S =
    let writeTuple r (prefixSlash: bool) (ss: S []) : S =
        let writesToPath() =
            ss |> Array.exists (fun s -> s.WritesToUrlPath())
        let ss =
            ss |> Array.mapi (fun i s ->
                let prefixSlash =
                    if i = 0 then
                        fun () -> prefixSlash && s.WritesToUrlPath()
                    else
                        s.WritesToUrlPath
                prefixSlash, s)
        S.Make(writesToPath, fun w q x ->
            let xs : _ [] = r x
            (ss, xs)
            ||> Array.forall2 (fun (slash, s) x ->
                if slash() then w.Add '/'
                s.Write w q x))
    match t with
    | Enum uT ->
        let s = getS uT
        S.Make(true, fun w q x ->
            let x' = System.Convert.ChangeType(x, uT)
            s.Write w q x')
    | Array eT ->
        let eS = getS eT
        if t.GetArrayRank() = 1 then
            S.Make(true, fun w q x ->
                let s = x :?> System.Array
                w.Add (string s.Length)
                Seq.cast s
                |> Seq.forall (fun x ->
                    w.Add '/'
                    eS.Write w q x))
        else
            raise (NoFormatError t)
    | List eT ->
        let eS = getS eT
        S.Make(true, fun w q x ->
            let s : seq<obj> = Seq.cast (x :?> _)
            w.Add (string (Seq.length s))
            s
            |> Seq.forall (fun x ->
                w.Add '/'
                eS.Write w q x))
    | Tuple (e, r, _) ->
        writeTuple r false (Array.map getS e)
    | Record (fs, r, _) ->
        writeTuple r false [| for f in fs -> writeField getS (RecordField f) |]
    | Union (cs, tR, uR, _) ->
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<DecodeResult<_>> then
            let s = getS (t.GetGenericArguments().[0])
            let getAction =
                t.GetProperty("Action",
                    System.Reflection.BindingFlags.NonPublic ||| 
                    System.Reflection.BindingFlags.Instance
                ).GetGetMethod(true)
            S.Make(s.WritesToUrlPath, fun w q x ->
                s.Write w q (getAction.Invoke(x, [||])))
        else
        let ss =
            [|
                for c in cs ->
                    writeTuple (uR c) true [|
                        for f in c.GetFields() ->
                            writeField getS (UnionCaseField(c, f))
                    |]
            |]
        let ns = [| for c in cs -> c.CustomizedName |]
        S.Make(true, fun w q x ->
            let t = tR x
            w.Add ns.[t]
            ss.[t].Write w q x)
    | Other t ->
        let wt = writePrimitiveType t
        S.Make(true, fun w q x -> w.Add(wt x); true)

type Parameters =
    {
        Request : Http.Request
        Read : unit -> option<string>
    }

type D =
    {
        ReadsJson : unit -> bool
        QueryParams : unit -> Set<string>
        Decode : Parameters -> option<DecodeResult<obj>>
    }
    static member Make(json, query, decode) =
        {
            ReadsJson = json
            QueryParams = query
            Decode = decode
        }
    static member Make(json, query, decode) =
        {
            ReadsJson = fun () -> json
            QueryParams = fun () -> query
            Decode = decode
        }

let getUnionCaseMethods (c: Reflection.UnionCaseInfo) =
    let s =
        c.GetCustomAttributesData()
        |> Seq.collect (fun cad ->
            if cad.Constructor.DeclaringType = typeof<MethodAttribute> then
                cad.ConstructorArguments.[0].Value
                :?> System.Collections.ObjectModel.ReadOnlyCollection<
                        System.Reflection.CustomAttributeTypedArgument>
                |> Seq.map (fun a -> Some (a.Value :?> string))
            else Seq.empty)
    if Seq.isEmpty s then Seq.singleton None else s

let parsePrimitiveType (t: System.Type) =
    match primitiveValueHandlers.TryGetValue t with
    | false, _ -> raise (NoFormatError t)
    | true, (p, _) -> p

let JsonProvider = Json.Provider.Create()

/// "ot" is known to be option<X>
let mkSome (ot: System.Type) =
    let someUci = Reflection.FSharpType.GetUnionCases(ot).[1]
    let ctor = Reflection.FSharpValue.PreComputeUnionConstructor(someUci)
    fun x -> ctor [| x |]

let getQueryParamParser (f: Field) =
    if isQueryParam f then
        let ft = f.Type
        let fn = f.Name
        if isPrimitive ft then
            let parse = parsePrimitiveType ft
            let defaultValue = JsonProvider.BuildDefaultValue ft
            D.Make(false, Set.singleton fn, fun p ->
                match p.Request.Get.[fn] with
                | None -> Some (MissingQueryParameter(defaultValue, fn))
                | Some v -> parse v |> Option.map Success
            )
            |> Some
        elif ft.IsGenericType &&
                ft.GetGenericTypeDefinition() = typedefof<option<_>> &&
                isPrimitive (ft.GetGenericArguments().[0]) then
            let parse = parsePrimitiveType (ft.GetGenericArguments().[0])
            let some = mkSome ft
            D.Make(false, Set.singleton fn, fun p ->
                match p.Request.Get.[fn] with
                | None -> Some (Success null)
                | Some v -> parse v |> Option.map (some >> Success)
            )
            |> Some
        else raise (NoFormatError f.DeclaringType)
    else None

let getJsonParser (f: Field) =
    if isJson f then
        let t = f.Type
        let decoder =
            try JsonProvider.GetDecoder t
            with Json.NoDecoderException _ -> raise (NoFormatError t)
        let defaultValue = JsonProvider.BuildDefaultValue t
        D.Make(true, Set.empty, fun p ->
            try
                // We need to copy the stream because else StreamReader would close it.
                use m = new System.IO.MemoryStream(int p.Request.Body.Length)
                p.Request.Body.CopyTo m
                p.Request.Body.Seek(0L, IO.SeekOrigin.Begin) |> ignore
                m.Seek(0L, IO.SeekOrigin.Begin) |> ignore
                use tr = new System.IO.StreamReader(m)
                Success (decoder.Decode (Json.Read tr))
            with
                | Json.ReadException
                | Json.DecoderException -> InvalidJson defaultValue
            |> Some)
        |> Some
    else None

let parseField getD (f: Field) =
    match getJsonParser f with
    | Some p -> p
    | None ->
        match getQueryParamParser f with
        | Some p -> p
        | None -> getD f.Type

let getD (getD: System.Type -> D) (t: System.Type) : D =
    let tryParse parse : D =
        D.Make(false, Set.empty, fun p ->
            match p.Read () with
            | Some s ->
                match parse s with
                | true, x -> Some (Success (x :> obj))
                | _ -> None
            | None -> None)
    let parseInt = tryParse System.Int32.TryParse
    let parseTuple t mk (ds: D[]) : D =
        let k = Array.length ds
        let json() =
            match ds |> Seq.filter (fun d -> d.ReadsJson()) |> Seq.length with
            | 0 -> false
            | 1 -> true
            | _ -> raise (NoFormatError t)
        let queryParams() =
            let qps = ds |> Array.map (fun d -> d.QueryParams())
            let noOverlap =
                qps |> Array.forall (fun q1 ->
                    qps |> Array.forall (fun q2 ->
                        Set.isEmpty (Set.intersect q1 q2)))
            if noOverlap then
                Set.unionMany qps
            else raise (NoFormatError t)
        D.Make(json, queryParams, fun p ->
            let xs : obj [] = Array.create k null
            let rec loop x =
                match x with
                | i when i = k -> Some (Success (mk xs))
                | i ->
                    ds.[i].Decode p
                    >>= fun x ->
                        xs.[i] <- x
                        loop (i + 1)
            loop 0)
    let parseArray eT (eD: D) : D =
        if eD.ReadsJson() then raise (NoFormatError eT)
        D.Make(false, Set.empty, fun p ->
            parseInt.Decode p
            >>= function
            | (:? int as k) ->
                let data = System.Array.CreateInstance(eT, k)
                let rec loop x =
                    match x with
                    | i when i = k -> Some (Success (box data))
                    | i ->
                        eD.Decode p
                        >>= fun obj ->
                            data.SetValue(obj, i)
                            loop (i + 1)
                if k >= 0 then loop 0 else None
            | _ -> None)
    match t with
    | Enum uT ->
        let toObj =
            typeof<System.Enum>
                .GetMethod("ToObject", [|typeof<System.Type>; uT|])
        let f = getD uT
        D.Make(false, Set.empty, fun p ->
            f.Decode p |> DecodeResult.map (fun x -> toObj.Invoke(null, [|t; x|])))
    | Array eT ->
        if t.GetArrayRank() > 1 then
            raise (NoFormatError t)
        parseArray eT (getD eT)
    | List eT ->
        let eD = getD eT
        let sP =
            typedefof<ListProcessor<_>>.MakeGenericType(eT)
            |> System.Activator.CreateInstance :?> ISequenceProcessor
        let f = parseArray eT eD
        D.Make(false, Set.empty, fun p ->
            f.Decode p
            |> DecodeResult.map (fun x -> sP.FromSequence (x :?> _)))
    | Tuple (e, _, c) ->
        parseTuple t c (Array.map getD e)
    | Record (fs, _, c) ->
        parseTuple t c [| for f in fs -> parseField getD (RecordField f) |]
    | Union (cs, _, _, uC) ->
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<DecodeResult<_>> then
            let ti = t.GetGenericArguments().[0]
            let d = getD ti
            let mkSuccess =
                let unbox =
                    typeof<DecodeResult>.GetMethod("unbox",
                        System.Reflection.BindingFlags.NonPublic |||
                        System.Reflection.BindingFlags.Static
                    ).MakeGenericMethod(ti)
                fun x -> DecodeResult<obj>.Success(unbox.Invoke(null, [| x |]))

            D.Make(d.ReadsJson, d.QueryParams, fun p ->
                d.Decode p |> Option.map mkSuccess)
        else
        let d = Dictionary<string, Map<option<Http.Method>, int>>()
        let ds =
            cs |> Array.mapi (fun i c ->
                let allowedMeths = getUnionCaseMethods c
                let existing =
                    match d.TryGetValue c.CustomizedName with
                    | true, m -> m
                    | false, _ -> Map.empty
                d.[c.CustomizedName] <-
                    (existing, allowedMeths)
                    ||> Seq.fold (fun map m -> Map.add (Option.map Http.Method.OfString m) i map)
                parseTuple t (uC c)
                    [| for f in c.GetFields() -> parseField getD (UnionCaseField(c, f)) |]
            )
        let json() = ds |> Array.exists (fun d -> d.ReadsJson())
        D.Make(json, (fun () -> Set.empty), fun p ->
            p.Read () |> Option.bind (fun name ->
                match d.TryGetValue name with
                | true, d ->
                    // Try to find a union case for the exact method searched
                    match d.TryFind (Some p.Request.Method) with
                    | Some k -> ds.[k].Decode p
                    | None ->
                        // Try to find None, ie. a non-method-specific union case
                        match d.TryFind None with
                        | Some k -> ds.[k].Decode p
                        | None ->
                            // This action doesn't parse, but try to find another action
                            // with the same name to pass to InvalidMethod
                            d |> Map.tryPick (fun _ k -> ds.[k].Decode p)
                            |> Option.map (function
                                | Success a
                                | MissingQueryParameter (a, _)
                                | InvalidJson a -> InvalidMethod (a, p.Request.Method.ToString())
                                | InvalidMethod (a, m) -> InvalidMethod (a, m))
                | false, _ -> None))
    | Other t ->
        let parse = parsePrimitiveType t
        D.Make(false, Set.empty, fun p ->
            match p.Read () with
            | Some s -> parse s |> Option.map Success
            | None -> None)

let memoFix delay f =
    let cache = System.Collections.Generic.Dictionary()
    let rec g x =
        if cache.ContainsKey x then
            match cache.[x] with
            | None -> delay (fun () -> cache.[x].Value)
            | Some x -> x
        else
            cache.[x] <- None
            let r = f g x
            cache.[x] <- Some r
            r
    g

type Format<'T> =
    {
        read : string -> Http.Request -> option<DecodeResult<obj>>
        show : obj -> option<string>
    }

    member this.Read(x, req) =
        this.read x req >>= function
            | (:? 'T as r) -> Some (Success r)
            | _ -> None

    member this.Show(x: 'T) =
        this.show (x :> obj)

[<Sealed>]
type Factory() =
    let delay f = fun x -> f () x
    let delayD d =
        {
            Decode = fun x -> d().Decode x
            ReadsJson = fun () -> d().ReadsJson()
            QueryParams = fun () -> d().QueryParams()
        }
    let delayS s =
        {
            Write = fun x -> s().Write x
            WritesToUrlPath = fun () -> s().WritesToUrlPath()
        }
    let getS : System.Type -> S = memoFix delayS getS
    let getD : System.Type -> D = memoFix delayD getD

    member this.GetFormatFor (t: System.Type) : Format<obj> =
        let d = getD t
        let s = getS t
        {
            read = fun input req ->
                let sb = System.Text.StringBuilder 128
                if input = null then None else
                    let parts = input.Split '/'
                    let e = (parts :> seq<string>).GetEnumerator()
                    let n () = if e.MoveNext() then Some e.Current else None
                    d.Decode { Request = req; Read = n }
            show = fun value ->
                let sb = System.Text.StringBuilder 128
                let q = List()
                if s.Write sb q value then
                    q |> Seq.iteri (fun i (k, v) ->
                        sb.Add (if i = 0 then '?' else '&')
                        sb.Add k
                        sb.Add '='
                        sb.Add v)
                    Some (sb.Flush())
                else None
        }

    member this.GetFormat<'T>() : Format<'T> =
        let fmt = this.GetFormatFor typeof<'T>
        {
            show = fmt.show
            read = fmt.read
        }

    static member Create() = Factory()

let GetFormat<'T> () =
    Factory.Create().GetFormat<'T>()

let GetFormatFor t =
    Factory.Create().GetFormatFor t
