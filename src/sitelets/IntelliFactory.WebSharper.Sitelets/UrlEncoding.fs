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

module IntelliFactory.WebSharper.Sitelets.UrlEncoding

open System.Collections.Generic
open IntelliFactory.WebSharper.Core

type DecodeResult<'Action> =
    | Success of 'Action
    | InvalidMethod of 'Action * ``method``: string
    | InvalidJson of 'Action

module DecodeResult =

    let map (f: 'a -> 'b) (x: option<DecodeResult<'a>>) : option<DecodeResult<'b>> =
        x |> Option.map (function
            | Success x -> Success (f x)
            | InvalidMethod (x, m) -> InvalidMethod (f x, m)
            | InvalidJson x -> InvalidJson (f x))

let (>>=) (x: option<DecodeResult<'a>>) (f: 'a -> option<DecodeResult<'b>>) : option<DecodeResult<'b>> =
    match x with
    | None -> None
    | Some (Success x) -> f x
    | Some (InvalidMethod (x, m)) ->
        f x |> Option.map (function
            | Success y | InvalidJson y -> InvalidMethod (y, m)
            | InvalidMethod (y, m) -> InvalidMethod (y, m))
    | Some (InvalidJson x) ->
        f x |> Option.map (function
            | InvalidMethod (y, m) -> InvalidMethod (y, m)
            | Success y | InvalidJson y -> InvalidJson y)

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

type S = System.Text.StringBuilder -> obj -> bool

let getUnionCaseJsonArgumentName (c: Reflection.UnionCaseInfo) =
    c.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<JsonAttribute> then
            Some (cad.ConstructorArguments.[0].Value :?> string)
        else None
    )

let getS (getS: System.Type -> S) (t: System.Type) : S =
    let writeTuple r (ss: S []) : S =
        fun w x ->
            let xs : _ [] = r x
            if xs.Length >= 1 then
                ss.[0] w xs.[0] &&
                seq { 1 .. xs.Length - 1 }
                |> Seq.forall (fun  i ->
                    w.Add '/'
                    ss.[i] w xs.[i])
            else
                true
    if t = typeof<bool> || t = typeof<int> || t = typeof<float> then
        fun w x -> w.Add (string x); true
    elif t = typeof<string> then
        fun w x ->
            if x <> null then
                let s = string x
                s
                |> Seq.iteri (fun i c ->
                    writeEscaped w (i + 1 = s.Length) c)
                true
            else false
    elif t = typeof<System.DateTime> then
        fun w x -> w.Add((x :?> System.DateTime).ToString "o"); true
    elif t.IsEnum then
        let uT = System.Enum.GetUnderlyingType(t)
        fun w x ->
            let s = System.Convert.ChangeType(x, uT)
            getS uT w s
    elif t.IsArray then
        let eT = t.GetElementType()
        let eS = getS eT
        if t.GetArrayRank() = 1 then
            fun w x ->
                let s = x :?> System.Array
                w.Add (string s.Length)
                Seq.cast s
                |> Seq.forall (fun x ->
                    w.Add '/'
                    eS w x)
        else
            raise (NoFormatError t)
    elif t.IsGenericType &&
         t.GetGenericTypeDefinition() = typedefof<list<_>> then
        let eT = t.GetGenericArguments().[0]
        let eS = getS eT
        fun w x ->
            let s : seq<obj> = Seq.cast (x :?> _)
            w.Add (string (Seq.length s))
            s
            |> Seq.forall (fun x ->
                w.Add '/'
                eS w x)
    elif Reflection.FSharpType.IsTuple t then
        let e = Reflection.FSharpType.GetTupleElements t
        let r = Reflection.FSharpValue.PreComputeTupleReader t
        writeTuple r (Array.map getS e)
    elif Reflection.FSharpType.IsRecord(t, flags) then
        let r  = Reflection.FSharpValue.PreComputeRecordReader(t, flags)
        let fs = Reflection.FSharpType.GetRecordFields t
        writeTuple r [| for f in fs -> getS f.PropertyType |]
    elif Reflection.FSharpType.IsUnion(t, flags) then
        let tR = Reflection.FSharpValue.PreComputeUnionTagReader(t, flags)
        let cs = Reflection.FSharpType.GetUnionCases(t, flags)
        let uR x = Reflection.FSharpValue.PreComputeUnionReader(x, flags)
        let ss =
            [|
                for c in cs ->
                    let json = getUnionCaseJsonArgumentName c
                    let jsonI =
                        c.GetFields()
                        |> Array.tryFindIndex (fun f -> json.IsSome && f.Name = json.Value)
                    match jsonI with
                    | None ->
                        writeTuple (uR c) [| for f in c.GetFields() -> getS f.PropertyType |]
                    | Some jsonI ->
                        writeTuple
                            (uR c >> fun a ->
                                Array.init (a.Length - 1)
                                    (fun j -> if j < jsonI then a.[j] else a.[j+1]))
                            (c.GetFields()
                                |> Array.mapi (fun i f ->
                                    if i = jsonI then None else Some (getS f.PropertyType))
                                |> Array.choose id)
            |]
        let ns = [| for c in cs -> c.CustomizedName |]
        let es = [| for c in cs -> c.GetFields().Length > 0 |]
        fun w x ->
            let t = tR x
            w.Add ns.[t]
            if es.[t] then
                w.Add '/'
                ss.[t] w x
            else
                true
    else
        raise (NoFormatError t)

type Parameters =
    {
        Request : Http.Request
        Read : unit -> option<string>
    }

type D =
    {
        ReadsJson : unit -> bool
        Decode : Parameters -> option<DecodeResult<obj>>
    }
    static member Make json decode =
        {
            ReadsJson = fun () -> json
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

let JsonProvider = Json.Provider.Create()

let parseJson (t: System.Type) =
    let decoder =
        try JsonProvider.GetDecoder t
        with Json.NoDecoderException _ -> raise (NoFormatError t)
    let defaultValue = lazy (JsonProvider.BuildDefaultValue t)
    D.Make true <| fun p ->
        try
            use tr = new System.IO.StreamReader(p.Request.Body)
            Success (decoder.Decode (Json.Read tr))
        with
            | Json.ReadException
            | Json.DecoderException -> InvalidJson defaultValue.Value
        |> Some

let getD (getD: System.Type -> D) (t: System.Type) : D =
    let tryParse parse : D =
        D.Make false <| fun p ->
            match p.Read () with
            | Some s ->
                match parse s with
                | true, x -> Some (Success (x :> obj))
                | _ -> None
            | None -> None
    let parseInt = tryParse System.Int32.TryParse
    let parseTuple t mk (ds: D[]) : D =
        let k = Array.length ds
        let json =
            match ds |> Seq.filter (fun d -> d.ReadsJson()) |> Seq.length with
            | 0 -> false
            | 1 -> true
            | _ -> raise (NoFormatError t)
        D.Make json <| fun p ->
            let xs : obj [] = Array.create k null
            let rec loop x =
                match x with
                | i when i = k -> Some (Success (mk xs))
                | i ->
                    ds.[i].Decode p
                    >>= fun x ->
                        xs.[i] <- x
                        loop (i + 1)
            loop 0
    let parseArray eT (eD: D) : D =
        if eD.ReadsJson() then raise (NoFormatError eT)
        D.Make false <| fun p ->
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
            | _ -> None
    if t = typeof<bool> then
        tryParse System.Boolean.TryParse
    elif t = typeof<int> then
        parseInt
    elif t = typeof<float> then
        tryParse System.Double.TryParse
    elif t = typeof<string> then
        let buf = System.Text.StringBuilder()
        D.Make false <| fun p ->
            let s = p.Read ()
            match s with
            | Some s ->
                use i = new System.IO.StringReader(s)
                let rec loop () =
                    match readEscaped i with
                    | ERROR -> None
                    | EOF -> Some (Success (box (buf.Flush())))
                    | x -> buf.Add (char x); loop ()
                loop ()
            | None -> None
    elif t = typeof<System.DateTime> then
        let rT = System.Globalization.DateTimeStyles.RoundtripKind
        tryParse <| fun x -> System.DateTime.TryParse(x, null, rT)
    elif t.IsEnum then
        let uT = System.Enum.GetUnderlyingType(t)
        let toObj =
            typeof<System.Enum>
                .GetMethod("ToObject", [|typeof<System.Type>; uT|])
        let f = getD uT
        D.Make false <| fun p ->
            f.Decode p |> DecodeResult.map (fun x -> toObj.Invoke(null, [|t; x|]))
    elif t.IsArray then
        if t.GetArrayRank() > 1 then
            raise (NoFormatError t)
        let eT = t.GetElementType()
        parseArray eT (getD eT)
    elif t.IsGenericType &&
         t.GetGenericTypeDefinition() = typedefof<list<_>> then
        let eT = t.GetGenericArguments().[0]
        let eD = getD eT
        let sP =
            typedefof<ListProcessor<_>>.MakeGenericType(eT)
            |> System.Activator.CreateInstance :?> ISequenceProcessor
        let f = parseArray eT eD
        D.Make false <| fun p ->
            f.Decode p
            |> DecodeResult.map (fun x -> sP.FromSequence (x :?> _))
    elif Reflection.FSharpType.IsTuple t then
        let e = Reflection.FSharpType.GetTupleElements t
        let c = Reflection.FSharpValue.PreComputeTupleConstructor t
        parseTuple t c (Array.map getD e)
    elif Reflection.FSharpType.IsRecord(t, flags) then
        let c = Reflection.FSharpValue.PreComputeRecordConstructor(t, flags)
        let fs = Reflection.FSharpType.GetRecordFields t
        parseTuple t c [| for f in fs -> getD f.PropertyType |]
    elif Reflection.FSharpType.IsUnion(t, flags) then
        let cs = Reflection.FSharpType.GetUnionCases(t, flags)
        let uC x = Reflection.FSharpValue.PreComputeUnionConstructor(x, flags)
        let d = Dictionary<string, Map<option<Http.Method>, int>>()
        let ds =
            cs |> Array.mapi (fun i c ->
                let allowedMeths = getUnionCaseMethods c
                let jsonArg = getUnionCaseJsonArgumentName c
                let existing =
                    match d.TryGetValue c.CustomizedName with
                    | true, m -> m
                    | false, _ -> Map.empty
                d.[c.CustomizedName] <-
                    (existing, allowedMeths)
                    ||> Seq.fold (fun map m -> Map.add (Option.map Http.Method.OfString m) i map)
                parseTuple t (uC c) [|
                    for f in c.GetFields() ->
                        if jsonArg.IsSome && f.Name = jsonArg.Value then
                            parseJson f.PropertyType
                        else
                            getD f.PropertyType
                |]
            )
        let json = ds |> Array.exists (fun d -> d.ReadsJson())
        D.Make json <| fun p ->
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
                                | InvalidJson a -> InvalidMethod (a, p.Request.Method.ToString())
                                | InvalidMethod (a, m) -> InvalidMethod (a, m))
                | false, _ -> None)
    else
        raise (NoFormatError t)

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
        }
    let getS : System.Type -> S = memoFix delay getS
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
                if s sb value then Some (sb.Flush()) else None
        }

    member this.GetFormat<'T>() : Format<'T> =
        let fmt = this.GetFormatFor typeof<'T>
        {
            show = fmt.show
            read = fmt.read
        }

    static member Create() = Factory()

let GetFormat<'T> ()=
    Factory.Create().GetFormat<'T>()

let GetFormatFor t =
    Factory.Create().GetFormatFor t
