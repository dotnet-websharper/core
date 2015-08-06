// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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
module A = WebSharper.Core.Attributes

[<A.NamedUnionCases "result">]
type DecodeResult<'Action> =
    | [<CompiledName "success">]
      Success of action: 'Action
    | [<CompiledName "invalidMethod">]
      InvalidMethod of action: 'Action * ``method``: string
    | [<CompiledName "invalidJson">]
      InvalidJson of action: 'Action
    | [<CompiledName "missingQueryParameter">]
      MissingQueryParameter of action: 'Action * queryParam: string
    | [<CompiledName "missingFormData">]
      MissingFormData of action: 'Action * formFieldName: string

    member this.Action =
        match this with
        | Success a
        | InvalidMethod (a, _)
        | InvalidJson a
        | MissingQueryParameter (a, _)
        | MissingFormData (a, _) -> a

type ReadParameters =
    {
        Request : Http.Request
        Fragments : string[]
        Pos : int
    }

    member this.Read() =
        if this.Pos < this.Fragments.Length then
            Some (this.Fragments.[this.Pos], { this with Pos = this.Pos + 1 })
        else None

    member this.IsAtEnd =
        { this.Pos .. this.Fragments.Length - 1 }
        |> Seq.forall (fun i -> this.Fragments.[i] = "")

    static member Make req frags =
        {
            Request = req
            Fragments = frags
            Pos = 0
        }

type DecodeResult =

    static member unbox (x: DecodeResult<obj>) : DecodeResult<'b> =
        match x with
        | Success x -> Success (unbox x)
        | InvalidMethod (x, m) -> InvalidMethod (unbox x, m)
        | InvalidJson x -> InvalidJson (unbox x)
        | MissingQueryParameter (x, p) -> MissingQueryParameter (unbox x, p)
        | MissingFormData (x, p) -> MissingFormData (unbox x, p)

type ReadResult<'a> = seq<DecodeResult<'a> * ReadParameters>

type ReadResult =

    static member map (f: 'a -> 'b) (x: ReadResult<'a>) : ReadResult<'b> =
        x |> Seq.map (function
            | Success x, p -> Success (f x), p
            | InvalidMethod (x, m), p -> InvalidMethod (f x, m), p
            | InvalidJson x, p -> InvalidJson (f x), p
            | MissingQueryParameter (x, pa), p -> MissingQueryParameter (f x, pa), p
            | MissingFormData (x, pa), p -> MissingFormData (f x, pa), p)

let (>>=) (x: ReadResult<'a>) (f: ReadParameters -> 'a -> ReadResult<'b>) : ReadResult<'b> =
    x |> Seq.collect (function
        | Success x, p -> f p x
        | InvalidMethod (x, m), p ->
            f p x |> Seq.map (function
                | InvalidMethod (y, m), p -> InvalidMethod (y, m), p
                | r, p -> InvalidMethod (r.Action, m), p)
        | InvalidJson x, p ->
            f p x |> Seq.map (function
                | Success y, p -> InvalidJson y, p
                | r -> r)
        | MissingQueryParameter (x, pa), p ->
            f p x |> Seq.map (function
                | Success y, p -> MissingQueryParameter (y, pa), p
                | r -> r)
        | MissingFormData (x, pa), p ->
            f p x |> Seq.map (function
                | Success y, p -> MissingFormData (y, pa), p
                | r -> r))

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
        let s =
            let aT = typeof<CompiledNameAttribute>
            match this.GetCustomAttributes aT with
            | [| :? CompiledNameAttribute as attr |] -> attr.CompiledName
            | _ -> this.Name
        s.[s.IndexOf('/') + 1 ..]

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

type ArrayProcessor<'T>() =
    interface ISequenceProcessor with
        member this.ToSequence (x: obj) = Seq.map box (x :?> 'T[])
        member this.FromSequence (s: seq<obj>) = box [|for x in s -> x :?> 'T|]

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

let opt f s =
    match f s with
    | true, x -> Some (box x)
    | false, _ -> None

let defaultDateFormat = "yyyy-MM-dd-HH.mm.ss"

let parseDateTime (fmt: string option) =
    let fmt = defaultArg fmt defaultDateFormat
    let rT = System.Globalization.DateTimeStyles.RoundtripKind
    opt <| fun x -> DateTime.TryParseExact(x, fmt, System.Globalization.CultureInfo.InvariantCulture, rT)

let writeDateTime (fmt: string option) =
    let fmt = defaultArg fmt defaultDateFormat
    fun (x: obj) -> (x :?> DateTime).ToString(fmt)

/// Dict<type, (encode * decode)>
let primitiveValueHandlers =
    let d = Dictionary()
    let add (parse: string -> bool * 'T) =
        d.Add(typeof<'T>, (opt parse, string))
    add System.Boolean.TryParse
    add System.SByte.TryParse
    add System.Byte.TryParse
    add System.Int16.TryParse
    add System.UInt16.TryParse
    add System.Int32.TryParse
    add System.UInt32.TryParse
    add System.Int64.TryParse
    add System.UInt64.TryParse
    add System.Single.TryParse
    add System.Double.TryParse
    add System.Decimal.TryParse
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
    d.Add(typeof<DateTime>, (parseDateTime None, writeDateTime None))
    d

let isPrimitive t =
    primitiveValueHandlers.ContainsKey t

let isPrimitiveOption (t: System.Type) =
    t.IsGenericType &&
    t.GetGenericTypeDefinition() = typedefof<option<_>> &&
    isPrimitive (t.GetGenericArguments().[0])

type Field =
    | RecordField of System.Reflection.PropertyInfo * isLast: bool
    | UnionCaseField of Reflection.UnionCaseInfo * System.Reflection.PropertyInfo * isLast: bool

    member private this.Property =
        match this with
        | RecordField (pi, _)
        | UnionCaseField (_, pi, _) -> pi

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

let getDateTimeFormat = function
    | RecordField (f, _) ->
        if f.PropertyType = typeof<System.DateTime> || f.PropertyType = typeof<option<System.DateTime>> then
            f.GetCustomAttributesData() |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = typeof<WebSharper.Core.Attributes.DateTimeFormatAttribute> &&
                   cad.ConstructorArguments.Count = 1 then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        else None
    | UnionCaseField (uci, pi, _) ->
        if pi.PropertyType = typeof<System.DateTime> || pi.PropertyType = typeof<option<System.DateTime>> then
            uci.GetCustomAttributesData() |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = typeof<WebSharper.Core.Attributes.DateTimeFormatAttribute> &&
                   cad.ConstructorArguments.Count = 2 &&
                   cad.ConstructorArguments.[0].Value :?> string = pi.Name then
                    Some (cad.ConstructorArguments.[1].Value :?> string)
                else None)
        else None

let isFormData = function
    | RecordField (f, _) ->
        f.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<FormDataAttribute> &&
            cad.ConstructorArguments.Count = 0)
    | UnionCaseField (uci, f, _) as ff ->
        uci.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<FormDataAttribute> &&
            cad.ConstructorArguments.Count = 1 &&
            cad.ConstructorArguments.[0].Value
            :?> System.Collections.ObjectModel.ReadOnlyCollection<
                    System.Reflection.CustomAttributeTypedArgument>
            |> Seq.exists (fun a -> a.Value :?> string = ff.Name))

let isQueryParam = function
    | RecordField (f, _) ->
        f.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<QueryAttribute> &&
            cad.ConstructorArguments.Count = 0)
    | UnionCaseField (uci, f, _) as ff ->
        uci.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<QueryAttribute> &&
            cad.ConstructorArguments.Count = 1 &&
            cad.ConstructorArguments.[0].Value
            :?> System.Collections.ObjectModel.ReadOnlyCollection<
                    System.Reflection.CustomAttributeTypedArgument>
            |> Seq.exists (fun a -> a.Value :?> string = ff.Name))

let isJson = function
    | RecordField (f, _) ->
        f.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<JsonAttribute> &&
            cad.ConstructorArguments.Count = 0)
    | UnionCaseField (uci, f, _) as ff ->
        uci.GetCustomAttributesData() |> Seq.exists (fun cad ->
            cad.Constructor.DeclaringType = typeof<JsonAttribute> &&
            cad.ConstructorArguments.Count = 1 &&
            cad.ConstructorArguments.[0].Value :?> string = ff.Name)

[<RequireQualifiedAccess>]
type Wildcard =
    | Seq of elementType: System.Type * ISequenceProcessor
    | String
    | None

let tryGetWildcardType f =
    let mkSP (sP: System.Type) (eT: System.Type) =
        sP.MakeGenericType(eT)
        |> System.Activator.CreateInstance :?> ISequenceProcessor
    let tryGetWildcardType (f: Reflection.PropertyInfo) =
        let t = f.PropertyType
        if t = typeof<string> then
            Wildcard.String
        elif t.IsArray then
            let eT = t.GetElementType()
            Wildcard.Seq (eT, mkSP typedefof<ArrayProcessor<_>> eT)
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
            let eT = t.GetGenericArguments().[0]
            Wildcard.Seq (eT, mkSP typedefof<ListProcessor<_>> eT)
        else Wildcard.None
    match f with
    | RecordField (f, true)
        when (f.DeclaringType.GetCustomAttributes(typeof<WildcardAttribute>, false) |> Array.isEmpty |> not) ->
        tryGetWildcardType f
    | UnionCaseField (uci, f, true)
        when (uci.GetCustomAttributes(typeof<WildcardAttribute>) |> Array.isEmpty |> not) ->
        tryGetWildcardType f
    | _ -> Wildcard.None

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

let tryWritePrimitiveField (f: Field) acceptOption =
    let ft = f.Type
    let writePrimitiveField t =
        if t = typeof<DateTime> then
            writeDateTime (getDateTimeFormat f)
        else writePrimitiveType t
    if isPrimitive ft then
        Some (writePrimitiveField ft >> Some)
    elif acceptOption && isPrimitiveOption ft then
        let w = getSome ft >> writePrimitiveField (ft.GetGenericArguments().[0])
        Some <| function
            | null -> None
            | x -> Some (w x)
    else None

let writeQueryParam (f: Field) : S option =
    if isQueryParam f then
        let ft = f.Type
        tryWritePrimitiveField f true
        |> Option.map (fun wp ->
            S.Make(false, fun w q x ->
                wp x |> Option.iter (fun x -> q.Add((f.Name, x)))
                true))
    else None

let writeWildcardSeq (eT: System.Type) (eS: S) (sP: ISequenceProcessor) : S =
    S.Make(true, fun w q x ->
        sP.ToSequence x
        |> Seq.cast
        |> Seq.forall (fun x ->
            w.Add '/'
            eS.Write w q x))

let writeWildcardString (eS: S) =
    S.Make(true, fun w q x ->
        (x :?> string).Split '/'
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.forall (fun (i, x) ->
            if i <> 0 then w.Add '/'
            eS.Write w q (box x)))

let writeField (getS: System.Type -> S) (f: Field) : S =
    if isJson f || isFormData f then
        S.Make(false, fun _ _ _ -> true)
    else
        match writeQueryParam f with
        | Some w -> w
        | None ->
            match tryGetWildcardType f with
            | Wildcard.Seq (eT, sP) -> writeWildcardSeq eT (getS eT) sP
            | Wildcard.String -> writeWildcardString (getS typeof<string>)
            | Wildcard.None ->
                match tryWritePrimitiveField f false with
                | Some wp ->
                    S.Make(true, fun w _ x ->
                        match wp x with
                        | Some t -> w.Add t; true
                        | None -> false)
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
        writeTuple r false
            (fs |> Array.mapi (fun i f ->
                writeField getS (RecordField(f, i = fs.Length - 1))))
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
                    let fs = c.GetFields()
                    writeTuple (uR c) true (fs |> Array.mapi (fun i f ->
                        writeField getS (UnionCaseField(c, f, i = fs.Length - 1))))
            |]
        let ns = [| for c in cs -> c.CustomizedName |]
        S.Make(true, fun w q x ->
            let t = tR x
            w.Add ns.[t]
            ss.[t].Write w q x)
    | Other t ->
        let wt = writePrimitiveType t
        S.Make(true, fun w q x -> w.Add(wt x); true)

type D =
    {
        ReadsJson : unit -> bool
        QueryParams : unit -> Set<string>
        Decode : ReadParameters -> ReadResult<obj>
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

let Just (p: ReadParameters) (x: DecodeResult<obj>) = Seq.singleton (x, p)
let Nothing = Seq.empty<DecodeResult<obj> * ReadParameters>
let ofOption (p: ReadParameters) (x: option<DecodeResult<obj>>) =
    match x with
    | None -> Nothing
    | Some x -> Just p x

let getUnionCaseMethods (c: Reflection.UnionCaseInfo) =
    let s =
        c.GetCustomAttributesData()
        |> Seq.collect (fun cad ->
            if cad.Constructor.DeclaringType = typeof<MethodAttribute> then
                cad.ConstructorArguments.[0].Value
                :?> System.Collections.ObjectModel.ReadOnlyCollection<
                        System.Reflection.CustomAttributeTypedArgument>
                |> Seq.map (fun a -> Some (a.Value :?> string))
            elif cad.Constructor.DeclaringType = typeof<CompiledNameAttribute> then
                let s = cad.ConstructorArguments.[0].Value :?> string
                match s.IndexOf '/' with
                | -1 -> Seq.empty
                | i ->
                    s.[..i - 1].Split([|',';' '|],
                        StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map Some
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

let tryParsePrimitiveField (f: Field) acceptOption =
    let ft = f.Type
    let parsePrimitiveField t =
        if t = typeof<DateTime> then
            parseDateTime (getDateTimeFormat f)
        else
            parsePrimitiveType t
    if isPrimitive ft then
        let parse = parsePrimitiveField ft
        let defaultValue = JsonProvider.BuildDefaultValue ft
        Some <| function
            | None -> None
            | Some v -> parse v |> Option.map Success
    elif acceptOption && isPrimitiveOption ft then
        let parse = parsePrimitiveField (ft.GetGenericArguments().[0])
        let some = mkSome ft
        Some <| function
            | None -> Some (Success null)
            | Some v -> parse v |> Option.map (some >> Success)
    else None

let getQueryParamParser (f: Field) =
    if isQueryParam f then
        let ft = f.Type
        let fn = f.Name
        let defaultValue = JsonProvider.BuildDefaultValue ft
        match tryParsePrimitiveField f true with
        | None -> raise (NoFormatError f.DeclaringType)
        | Some parse ->
            D.Make(false, Set.singleton fn, fun p ->
                match parse p.Request.Get.[fn] with
                | Some (Success _ as x) -> x
                | _ -> MissingQueryParameter(defaultValue, fn)
                |> Just p)
            |> Some
    else None

let getFormDataParser (f: Field) =
    if isFormData f then
        let ft = f.Type
        let fn = f.Name
        let defaultValue = JsonProvider.BuildDefaultValue ft
        match tryParsePrimitiveField f true with
        | None -> raise (NoFormatError f.DeclaringType)
        | Some parse ->
            D.Make(false, Set.singleton fn, fun p ->
                match parse p.Request.Post.[fn] with
                | Some (Success _ as x) -> x
                | _ -> MissingFormData(defaultValue, fn)
                |> Just p)
            |> Some
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
                use m =
                    if p.Request.Body.CanSeek then
                        new System.IO.MemoryStream(int p.Request.Body.Length)
                    else
                        new System.IO.MemoryStream()
                p.Request.Body.CopyTo m
                if p.Request.Body.CanSeek then
                    p.Request.Body.Seek(0L, IO.SeekOrigin.Begin) |> ignore
                m.Seek(0L, IO.SeekOrigin.Begin) |> ignore
                use tr = new System.IO.StreamReader(m)
                Success (decoder.Decode (Json.Read tr))
            with
                | Json.ReadException
                | Json.DecoderException -> InvalidJson defaultValue
            |> Just p)
        |> Some
    else None

type Arr = System.Collections.ArrayList

let parseRemainingStrings (eD: D) k (p: ReadParameters) =
    let rec loop (p: ReadParameters) (a: Arr) =
        if p.IsAtEnd then
            Just p (Success (box (k (a.ToArray()))))
        else
            eD.Decode p >>= fun p fragment ->
                let a = a.Clone() :?> Arr
                a.Add(fragment) |> ignore
                loop p a
    loop p (new Arr())

let parseWildcardSeq (eT: System.Type) (eD: D) (sP: ISequenceProcessor) : D =
    if eD.ReadsJson() then raise (NoFormatError eT)
    D.Make(false, Set.empty, parseRemainingStrings eD sP.FromSequence)

let parseWildcardString (eD: D) : D =
    D.Make(false, Set.empty, parseRemainingStrings eD (Seq.cast >> String.concat "/"))

let parseField getD (f: Field) =
    match getJsonParser f with
    | Some p -> p
    | None ->
        match getQueryParamParser f with
        | Some p -> p
        | None ->
            match getFormDataParser f with
            | Some p -> p
            | None ->
                match tryGetWildcardType f with
                | Wildcard.Seq (eT, sP) -> parseWildcardSeq eT (getD eT) sP
                | Wildcard.String -> parseWildcardString (getD typeof<string>)
                | Wildcard.None ->
                    match tryParsePrimitiveField f false with
                    | Some parse ->
                        D.Make(false, Set.empty, fun (p: ReadParameters) ->
                            match p.Read() with
                            | None -> parse None |> ofOption p
                            | Some (x, p) -> parse (Some x) |> ofOption p)
                    | None -> getD f.Type

let getD (getD: System.Type -> D) (t: System.Type) : D =
    let tryParse parse : D =
        D.Make(false, Set.empty, fun (p: ReadParameters) ->
            match p.Read () with
            | Some (s, p) ->
                match parse s with
                | true, x -> Just p (Success (x :> obj))
                | _ -> Nothing
            | None -> Nothing)
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
            let rec loop p i =
                if i = k then
                    Just p (Success (mk xs))
                else
                    ds.[i].Decode p
                    >>= fun p x ->
                        xs.[i] <- x
                        loop p (i + 1)
            loop p 0)
    let parseArray eT (eD: D) : D =
        if eD.ReadsJson() then raise (NoFormatError eT)
        D.Make(false, Set.empty, fun p ->
            parseInt.Decode p
            >>= fun p -> function
            | (:? int as k) ->
                let data = System.Array.CreateInstance(eT, k)
                let rec loop p x =
                    match x with
                    | i when i = k -> Just p (Success (box data))
                    | i ->
                        eD.Decode p
                        >>= fun p obj ->
                            data.SetValue(obj, i)
                            loop p (i + 1)
                if k >= 0 then loop p 0 else Nothing
            | _ -> Nothing)
    match t with
    | Enum uT ->
        let toObj =
            typeof<System.Enum>
                .GetMethod("ToObject", [|typeof<System.Type>; uT|])
        let f = getD uT
        D.Make(false, Set.empty, fun p ->
            f.Decode p |> ReadResult.map (fun x -> toObj.Invoke(null, [|t; x|])))
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
            |> ReadResult.map (fun x -> sP.FromSequence (x :?> _)))
    | Tuple (e, _, c) ->
        parseTuple t c (Array.map getD e)
    | Record (fs, _, c) ->
        parseTuple t c
            (fs |> Array.mapi (fun i f ->
                parseField getD (RecordField (f, i = fs.Length - 1))))
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
                d.Decode p |> Seq.map (fun (x, p) -> mkSuccess x, p))
        else
        let d = Dictionary<string, Map<option<Http.Method>, list<int>>>()
        let ds =
            cs |> Array.mapi (fun i c ->
                let allowedMeths = getUnionCaseMethods c
                let existing =
                    match d.TryGetValue c.CustomizedName with
                    | true, m -> m
                    | false, _ -> Map.empty
                d.[c.CustomizedName] <-
                    (existing, allowedMeths)
                    ||> Seq.fold (fun map m ->
                        let k = (Option.map Http.Method.OfString m)
                        Map.add k (defaultArg (map.TryFind k) [] @ [i]) map)
                let fields = c.GetFields()
                parseTuple t (uC c)
                    (fields |> Array.mapi (fun i f ->
                        parseField getD (UnionCaseField(c, f, i = fields.Length - 1))))
            )
        let json() = ds |> Array.exists (fun d -> d.ReadsJson())
        D.Make(json, (fun () -> Set.empty), fun (p: ReadParameters) ->
            let tn = t.FullName
            let tryDecode p ks : ReadResult<obj> =
                ks |> Seq.collect (fun k ->
                    ds.[k].Decode p)
            if p.IsAtEnd then
                Some ("", p)
            else p.Read ()
            |> Option.map (fun (name, p) ->
                match d.TryGetValue name with
                | false, _ -> Nothing
                | true, m ->
                    // Try to find a union case for the exact method searched
                    match m.TryFind (Some p.Request.Method) with
                    | Some ks -> tryDecode p ks
                    | None ->
                        // Try to find None, ie. a non-method-specific union case
                        match m.TryFind None with
                        | Some ks -> tryDecode p ks
                        | None ->
                            // This action doesn't parse, but try to find another action
                            // with the same name to pass to InvalidMethod
                            m |> Map.tryPick (fun _ -> function
                                | [] -> None
                                | (k :: _) -> Some (ds.[k].Decode p))
                            |> Option.map (Seq.map (function
                                | Success a, p
                                | MissingQueryParameter (a, _), p
                                | MissingFormData (a, _), p
                                | InvalidJson a, p -> InvalidMethod (a, p.Request.Method.ToString()), p
                                | InvalidMethod (a, m), p -> InvalidMethod (a, m), p))
                            |> function None -> Nothing | Some s -> s)
            |> function None -> Nothing | Some s -> s)
    | Other t ->
        let parse = parsePrimitiveType t
        D.Make(false, Set.empty, fun (p: ReadParameters) ->
            match p.Read () with
            | Some (s, p) ->
                match parse s with
                | None -> Nothing
                | Some s -> Just p (Success s)
            | None -> Nothing)

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

    member this.Read(x, req) : option<DecodeResult<'T>> =
        this.read x req
        |> Option.map DecodeResult.unbox

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
                    input.Split '/'
                    |> ReadParameters.Make req
                    |> d.Decode
                    |> Seq.tryPick (fun (x, p) ->
                        if p.IsAtEnd then Some x else None)
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
