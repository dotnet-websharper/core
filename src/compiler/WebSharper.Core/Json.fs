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

module WebSharper.Core.Json

open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module A = WebSharper.Core.Attributes
module P = WebSharper.Core.JavaScript.Packager
module R = WebSharper.Core.Reflection
module Re = WebSharper.Core.Resources

[<Literal>]
let TYPES = "$TYPES"

[<Literal>]
let DATA = "$DATA"

[<Literal>]
let TYPE = "$T"

[<Literal>]
let VALUE = "$V"

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

type Value =
    | Null
    | True
    | False
    | Number of string
    | String of string
    | Array  of list<Value>
    | Object of list<string * Value>

exception ReadException
exception WriteException

let readNumber (w: System.Text.StringBuilder) (tr: System.IO.TextReader) =
    let c (x: char) = w.Append x |> ignore
    let read () = tr.Read()
    let peek () = tr.Peek()
    let skip () = tr.Read() |> ignore
    let readDigits () =
        let rec loop () =
            match peek () with
            | n when n >= 48 && n <= 57 ->
                skip (); c (char n); loop ()
            | _ -> ()
        loop ()
    match peek () with
    | 45 -> skip (); c '-'
    | _ -> ()
    match read () with
    | n when n >= 49 && n <= 57 ->
        c (char n); readDigits ()
    | 48 -> c '0'
    | _ -> raise ReadException
    match peek () with
    | 46 -> skip (); c '.'; readDigits ()
    | _ -> ()
    match peek () with
    | 101 | 69 ->
        skip (); c 'E'
        match peek () with
        | 45 -> skip (); c '-'
        | 43 -> skip (); c '+'
        | _ -> ()
        readDigits ()
    | _ ->
        ()
    let text = w.ToString()
    w.Remove(0, w.Length) |> ignore
    Number text

let readString (w: System.Text.StringBuilder) (tr: System.IO.TextReader) =
    let c (x: char) = w.Append x |> ignore
    let read () = tr.Read()
    let peek () = tr.Peek()
    let skip () = tr.Read() |> ignore
    match read () with
    | 34 ->
        let rec loop () =
            match read() with
            | 34 -> ()
            | -1 -> raise ReadException
            | 92 ->
                match read () with
                | 34 -> c '"'
                | 92 -> c '\\'
                | 47 -> c '/'
                | 98 -> c '\b'
                | 102 -> c '\f'
                | 110 -> c '\n'
                | 114 -> c '\r'
                | 116 -> c '\t'
                | 117 ->
                    let hex () =
                        match read () with
                        | n when n >= 97 && n <= 102 ->
                            n - 97 + 10
                        | n when n >= 65 && n <= 70 ->
                            n - 65 + 10
                        | n when n >= 48 && n <= 57 ->
                            n - 48
                        | _ ->
                            raise ReadException
                    let inline ( * ) a b = (a <<< 4) + b
                    c (char (hex () * hex () * hex () * hex ()))
                | _ ->
                    raise ReadException
                loop ()
            | x ->
                let x = char x
                c x
                loop ()
        loop ()
        let text = w.ToString()
        w.Remove(0, w.Length) |> ignore
        text
    | _ ->
        raise ReadException

let readSpace (tr: System.IO.TextReader) =
    let rec loop () =
        match tr.Peek() with
        | n when System.Char.IsWhiteSpace (char n) ->
            tr.Read() |> ignore
            loop ()
        | _ ->
            ()
    loop ()

let rec readJson (w: System.Text.StringBuilder) (tr: System.IO.TextReader) =
    let c (x: char) = w.Append x |> ignore
    let read () = tr.Read()
    let peek () = tr.Peek()
    let skip () = tr.Read() |> ignore
    readSpace tr
    match peek () with
    | 110 ->
        if read () = 110
           && read () = 117
           && read () = 108
           && read () = 108
        then
            Null
        else
            raise ReadException
    | 116 ->
        if read() = 116
           && read() = 114
           && read() = 117
           && read() = 101
        then
            True
        else
            raise ReadException
    | 102 ->
        if read() = 102
           && read() = 97
           && read() = 108
           && read() = 115
           && read() = 101
        then
            False
        else
            raise ReadException
    | 34 ->
        String (readString w tr)
    | 45 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 ->
        readNumber w tr
    | 123 ->
        skip ()
        readSpace tr
        match peek () with
        | 125 ->
            skip ()
            Object []
        | _ ->
            let readPair () =
                let n = readString w tr
                readSpace tr
                if not (read() = 58) then
                    raise ReadException
                readSpace tr
                let j = readJson w tr
                readSpace tr
                (n, j)
            let p = readPair ()
            let rec loop acc =
                match read () with
                | 125 -> List.rev acc
                | 44 ->
                    readSpace tr
                    loop (readPair () :: acc)
                | _ -> raise ReadException
            Object (loop [p])
    | 91 ->
        skip ()
        readSpace tr
        match peek () with
        | 93 ->
            skip ()
            Array []
        | _ ->
            let j = readJson w tr
            readSpace tr
            let rec loop acc =
                readSpace tr
                match read () with
                | 44 ->
                    let j = readJson w tr
                    readSpace tr
                    loop (j :: acc)
                | 93 -> List.rev acc
                | _ -> raise ReadException
            Array (loop [j])
    | _ ->
        raise ReadException

let numberPattern =
    let pat = @"^([-]?(0|[1-9]\d*))([.]\d+)?([eE][-+]?\d+)?$"
    System.Text.RegularExpressions.Regex pat

let rec Write (writer: System.IO.TextWriter) (value: Value) =
    let c (x: char) = writer.Write x
    let s (x: string) = writer.Write x
    let wJ x = Write writer x
    let wA x =
        match x with
        | [] -> s "[]"
        | x :: xs ->
            c '['
            wJ x
            for x in xs do
                c ','
                wJ x
            c ']'
    let wN (x: string) =
        if x <> null && numberPattern.IsMatch x then
            s x
        else
            raise WriteException
    let wS (x: string) =
        if x = null then
            raise WriteException
        c '"'
        for i in 0 .. x.Length - 1 do
            match x.[i] with
            | '"' -> s "\\\""
            | '/' -> s "\\/"
            | '\\' -> s "\\\\"
            | '\b' -> s "\\b"
            | '\n' -> s "\\n"
            | '\r' -> s "\\r"
            | '\t' -> s "\\t"
            | '\012' -> s "\\f"
            | x ->
                if System.Char.IsControl x then
                    writer.Write("\\u{0:x4}", int x)
                else
                    c x
        c '"'
    let wO x =
        match x with
        | [] -> s "{}"
        | x :: xs ->
            let pair (n, x) =
                wS n
                c ':'
                wJ x
            c '{'
            pair x
            for x in xs do
                c ','
                pair x
            c '}'
    match value with
    | Null -> s "null"
    | True -> s "true"
    | False -> s "false"
    | Number x -> wN x
    | String x -> wS x
    | Array x -> wA x
    | Object x -> wO x

let Read (tr: System.IO.TextReader) : Value =
    let w = new System.Text.StringBuilder()
    readJson w tr

exception DecoderException
exception EncoderException

exception NoDecoderException of System.Type with
    override this.Message =
        "No JSON decoder for " + string this.Data0

exception NoEncoderException of System.Type with
    override this.Message =
        "No JSON encoder for " + string this.Data0

type Encoded =
    | EncodedNull
    | EncodedTrue
    | EncodedFalse
    | EncodedNumber of string
    | EncodedString of string
    | EncodedArray of list<Encoded>
    | EncodedObject of list<string * Encoded>
    | EncodedInstance of P.Address * list<string * Encoded>

    static member Lift json =
        let enc (x, y) = (x, Encoded.Lift y)
        match json with
        | Null -> EncodedNull
        | True -> EncodedTrue
        | False -> EncodedFalse
        | Number x -> EncodedNumber x
        | String x -> EncodedString x
        | Array x -> EncodedArray (List.map Encoded.Lift x)
        | Object x -> EncodedObject (List.map enc x)

    static member Array x = EncodedArray x
    static member Object x = EncodedObject x

[<Sealed>]
type Decoder(dec: Value -> obj) =
    member this.Decode x = dec x

[<Sealed>]
type Decoder<'T>(dec: Value -> 'T) =
    member this.Decode x = dec x

[<Sealed>]
type Encoder(enc: obj -> Encoded) =
    member this.Encode (x: obj) = enc x

[<Sealed>]
type Encoder<'T>(enc: 'T -> Encoded) =
    member this.Encode x = enc x

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue

type FormatSettings =
    {
        /// Tag the given encoded value with its type.
        AddTag : System.Type -> Encoded -> Encoded
        /// Get the JSON-encoded name of the given F# record or class field.
        GetEncodedFieldName : System.Type -> string -> string
        /// Find the union case tag of the given JSON object
        /// (represented as a fieldname -> value function)
        GetUnionTag : System.Type -> (string -> option<Value>) -> option<int>
        /// Get the JSON-encoded union tag name and value.
        EncodeUnionTag : System.Type -> int -> option<string * Encoded>
        /// Get the JSON-encoded name of the given F# union case field.
        GetEncodedUnionFieldName : System.Reflection.PropertyInfo -> int -> string
        /// If true then:
        /// * Represent fields whose type is a union marked UseNullAsTrueValue as absent field.
        /// * Always represent options as if they were marked [<OptionalField>].
        /// * Flatten collections:
        ///     * list<'T>, Set<'T> -> array
        ///     * Map<string, _>, Dictionary<string, _> -> flat object
        /// * Inline single record argument of a union into the union object itself.
        ConciseRepresentation : bool
        /// Pack an encoded value to JSON.
        Pack : Encoded -> Value
        EncodeDateTime : TAttrs -> System.DateTime -> Encoded
        DecodeDateTime : TAttrs -> Value -> option<System.DateTime>
    }

and TAttrs =
    {
        OptionalField : bool
        NullableUnion : bool
        DateTimeFormat : string option
        NamedUnionCases : string option option
        Type : System.Type
    }

    static member inline GetName(mi) =
        let customName =
            (^T : (member GetCustomAttributesData : unit -> IList<CustomAttributeData>) (mi))
            |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = typeof<A.NameAttribute> ||
                    cad.Constructor.DeclaringType = typeof<CompiledNameAttribute> then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        defaultArg customName (^T : (member Name : string) (mi))

    static member Get(i: FormatSettings, t: System.Type, ?mi: #System.Reflection.MemberInfo, ?uci: Reflection.UnionCaseInfo) =
        let tcad = t.GetCustomAttributesData()
        let mcad =
            match mi with
            | Some mi -> mi.GetCustomAttributesData()
            | None -> [||] :> _
        let ucad =
            match uci with
            | Some uci -> uci.GetCustomAttributesData()
            | None -> [||] :> _
        let isOptionalField =
            t.IsGenericType &&
            t.GetGenericTypeDefinition() = typedefof<option<_>> &&
            (i.ConciseRepresentation ||
                (mcad |> Seq.exists (fun t ->
                    t.Constructor.DeclaringType = typeof<A.OptionalFieldAttribute>)))
        let isNullableUnion =
            i.ConciseRepresentation &&
            FST.IsUnion t &&
            mcad |> Seq.exists (fun cad ->
                cad.Constructor.DeclaringType = typeof<CompilationRepresentationAttribute> &&
                let flags = cad.ConstructorArguments.[0].Value :?> CompilationRepresentationFlags
                flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0)
        let dateTimeFormat =
            mcad |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = typeof<A.DateTimeFormatAttribute> &&
                   cad.ConstructorArguments.Count = 1 then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        let dateTimeFormat =
            match dateTimeFormat with
            | Some f -> Some f
            | None ->
                ucad |> Seq.tryPick (fun cad ->
                    if cad.Constructor.DeclaringType = typeof<A.DateTimeFormatAttribute> &&
                       cad.ConstructorArguments.Count = 2 &&
                       mi.IsSome &&
                       cad.ConstructorArguments.[0].Value :?> string = mi.Value.Name then
                        Some (cad.ConstructorArguments.[1].Value :?> string)
                    else None)
        {
            OptionalField = isOptionalField
            NullableUnion = isNullableUnion
            DateTimeFormat = dateTimeFormat
            NamedUnionCases = None
            Type = t
        }

type Serializer =
    {
        Decode : option<Value -> obj>
        Encode : option<obj -> Encoded>
    }

let simple enc dec =
    {
        Encode = Some (fun x -> enc x)
        Decode = Some (fun x -> dec x)
    }

let numeric<'T> dec =
    let enc (x: obj) =
        match x with
        | null -> EncodedNull
        | :? 'T as x -> EncodedNumber (string (x :> obj))
        | _ -> raise EncoderException
    let dec = function
        | Null -> box (Unchecked.defaultof<'T>)
        | Number x ->
            match dec x with
            | true, (x: 'T) -> box x
            | _ -> raise DecoderException
        | _ -> raise DecoderException
    simple enc dec

let addNumeric<'T> (dec: string -> bool * 'T) (d: Dictionary<_,_>) =
    d.[typeof<'T>] <- numeric dec

let add<'T> (e: 'T -> Encoded) (d: Value -> 'T) (dict: Dictionary<_,_>) =
    let enc (x: obj) =
        match x with
        | null -> EncodedNull
        | :? 'T as x -> e x
        | _ -> raise EncoderException
    let dec = function
        | Null -> box (Unchecked.defaultof<'T>)
        | x -> box (d x)
    dict.[typeof<'T>] <- simple enc dec

let tryParseSingle x= 
    System.Single.TryParse(x, 
        System.Globalization.NumberStyles.Float, 
        System.Globalization.NumberFormatInfo.InvariantInfo)

let tryParseDouble x = 
    System.Double.TryParse(x, 
        System.Globalization.NumberStyles.Float, 
        System.Globalization.NumberFormatInfo.InvariantInfo)

let tryParseDecimal x = 
    System.Decimal.TryParse(x, 
        System.Globalization.NumberStyles.Float, 
        System.Globalization.NumberFormatInfo.InvariantInfo)

let serializers =
    let d = Dictionary()
    addNumeric System.Byte.TryParse d
    addNumeric System.SByte.TryParse d
    addNumeric System.Int16.TryParse d
    addNumeric System.Int32.TryParse d
    addNumeric System.Int64.TryParse d
    addNumeric System.UInt16.TryParse d
    addNumeric System.UInt32.TryParse d
    addNumeric System.UInt64.TryParse d
    addNumeric tryParseSingle d
    addNumeric tryParseDouble d
    addNumeric tryParseDecimal d
    let encBool = function
        | true -> EncodedTrue
        | false -> EncodedFalse
    let decBool = function
        | True -> true
        | False -> false
        | _ -> raise DecoderException
    add encBool decBool d
    let encChar (c: char) =
        EncodedNumber (string (int c))
    let decChar = function
        | Number x ->
            match System.Int32.TryParse x with
            | true, i when i >= 0 ->char i
            | _ -> raise DecoderException
        | _ -> raise DecoderException
    add encChar decChar d
    let decString = function
        | String x -> x
        | _ -> raise DecoderException
    add EncodedString decString d
    let encTimeSpan (t: System.TimeSpan) =
        EncodedNumber (string t.TotalMilliseconds)
    let decTimeSpan = function
        | Number x ->
            match tryParseDouble x with
            | true, x -> System.TimeSpan.FromMilliseconds x
            | _ -> raise DecoderException
        | _ -> raise DecoderException
    add encTimeSpan decTimeSpan d
    let encGuid (g: System.Guid) =
        EncodedString (string g)
    let decGuid = function
        | String g -> 
            match System.Guid.TryParse g with
            | true, g -> g
            | _ -> raise DecoderException
        | _ -> raise DecoderException  
    add encGuid decGuid d   
    d

let tupleEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let e = Array.map (fun t -> dE (TAttrs.Get(i, t))) (FST.GetTupleElements ta.Type)
    let r = FSV.PreComputeTupleReader ta.Type
    fun (x: obj) ->
        match x with
        | null ->
            raise EncoderException
        | o when o.GetType() = ta.Type ->
            EncodedArray (Array.toList (Array.map2 (fun e x -> e x) e (r o)))
        | _ ->
            raise EncoderException

let tupleDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let e = Array.map (fun t -> dD (TAttrs.Get(i, t))) (FST.GetTupleElements ta.Type)
    let c = FSV.PreComputeTupleConstructor ta.Type
    fun (x: Value) ->
        match x with
        | Array xs ->
            let xs = List.toArray xs
            if xs.Length = e.Length then
                c (Array.map2 (fun e x -> e x) e xs)
            else
                raise DecoderException
        | _ ->
            raise DecoderException

let arrayEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let e = dE (TAttrs.Get(i, ta.Type.GetElementType()))
    fun (x: obj) ->
        match x with
        | null ->
            EncodedNull
        | o when o.GetType() = ta.Type ->
            let o = o :?> System.Array
            Seq.cast o
            |> Seq.map e
            |> Seq.toList
            |> EncodedArray
        | _ ->
            raise EncoderException

let arrayDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let eT = ta.Type.GetElementType()
    let e = dD (TAttrs.Get(i, eT))
    fun (x: Value) ->
        match x with
        | Null ->
            null
        | Array xs ->
            let data =
                Seq.map e xs
                |> Seq.toArray
            let k = data.Length
            let r = System.Array.CreateInstance(eT, k)
            System.Array.Copy(data, r, k)
            r :> obj
        | _ ->
            raise DecoderException

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let table ts =
    let d = Dictionary()
    for (k, v) in ts do
        d.[k] <- v
    fun x ->
        match d.TryGetValue x with
        | true, x -> Some x
        | _ -> None

/// Get the MethodInfo corresponding to a let-bound function or value,
/// parameterized with the given types.
let genLetMethod (e: Expr, ts: System.Type[]) =
    match e with
    | Lambda(_, Lambda(_, Call(None, m, [_;_]))) // function
    | Call(None, m, []) -> // value
        FastInvoke.Compile(m.GetGenericMethodDefinition().MakeGenericMethod(ts))
    | _ -> failwithf "Json.genLetMethod: invalid expr passed: %A" e

let callGeneric (func: Expr<'f -> 'i -> 'o>) (dD: TAttrs -> 'f) ta (targ: System.Type) : 'i -> 'o =
    let m = genLetMethod(func, [|targ|])
    let dI = dD { ta with Type = targ }
    fun x -> unbox<'o> (m.Invoke2(dI, x))

let unmakeOption<'T> (dV: obj -> Encoded) (x: obj) =
    x |> unbox<option<'T>> |> Option.map (box >> dV)

let encodeOptionalField dE ta : obj -> option<Encoded> =
    if ta.OptionalField then
        ta.Type.GetGenericArguments().[0]
        |> callGeneric <@ unmakeOption @> dE ta
    elif ta.NullableUnion then
        let enc = dE ta
        function null -> None | x -> Some (enc x)
    else
        let enc = dE ta
        fun x -> Some (enc x)

let makeOption<'T> (dV: Value -> obj) (v: option<Value>) =
    match v with
    | Some (Null as v) ->
        // Decode null as None, only if 'T itself doesn't support encoding as Null
        try Some (dV v |> unbox<'T>) with _ -> None
    | v ->
        v |> Option.map (dV >> unbox<'T>)
    |> box

let decodeOptionalField dD ta : option<Value> -> obj =
    if ta.OptionalField then
        ta.Type.GetGenericArguments().[0]
        |> callGeneric <@ makeOption @> dD ta
    elif ta.NullableUnion then
        let dec = dD ta
        function Some v -> dec v | None -> null
    else
        let dec = dD ta
        function Some v -> dec v | None -> raise DecoderException

let isInlinableRecordCase (uci: Reflection.UnionCaseInfo) =
    let fields = uci.GetFields()
    fields.Length = 1 &&
    fields.[0].Name = "Item" &&
    FST.IsRecord fields.[0].PropertyType

/// Some (Some x) if tagged [<NameUnionCases x>];
/// Some None if tagged [<NamedUnionCases>];
/// None if not tagged.
let getDiscriminatorName (t: System.Type) =
    t.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<A.NamedUnionCasesAttribute> then
            if cad.ConstructorArguments.Count = 1 then
                Some (Some (cad.ConstructorArguments.[0].Value :?> string))
            else Some None
        else None)

let inferredCasesTable t =
    let cases =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let fields = c.GetFields()
            let fields =
                if isInlinableRecordCase c then
                    FST.GetRecordFields fields.[0].PropertyType
                else fields
            let fields =
                fields
                |> Array.filter (fun f ->
                    let t = f.PropertyType
                    not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>))
                |> Array.map (fun f -> f.Name)
            c.Tag, Set fields
        )
        |> Map.ofArray
    let findDistinguishingCase (cases: Map<int, Set<string>>) =
        cases
        |> Map.tryPick (fun t fs ->
            let allOtherFields =
                cases
                |> Seq.choose (fun (KeyValue(t', fs)) ->
                    if t = t' then None else Some fs)
                |> Set.unionMany
            let uniqueCases = fs - allOtherFields
            if Set.isEmpty uniqueCases then
                None
            else Some (Seq.head uniqueCases, t)
        )
    let rec buildTable acc cases =
        if Map.isEmpty cases then acc else
        match findDistinguishingCase cases with
        | None -> raise (NoDecoderException t)
        | Some (name, tag) ->
            buildTable
                <| (name, tag) :: acc
                <| Map.remove tag cases
    buildTable [] cases

module Internal =

    let inline GetName x = TAttrs.GetName x

    type UnionDiscriminator =
        | NoField of (string * int) list
        | StandardField
        | NamedField of string

    type UnionCaseArgFlag =
        | DateTimeFormat of string

    [<RequireQualifiedAccess>]
    type UnionCaseConstantEncoding =
        | Bool of bool
        | Int of int
        | Float of float
        | String of string

    type UnionCaseEncoding =
        | Normal of name: string * args: (string * System.Type * UnionCaseArgFlag[])[]
        | InlineRecord of name: string * record: System.Type
        | Constant of value: UnionCaseConstantEncoding

    let getUnionCaseConstantEncoding (uci: Reflection.UnionCaseInfo) =
        uci.GetCustomAttributesData()
        |> Seq.tryPick (fun cad ->
            if cad.Constructor.DeclaringType = typeof<A.ConstantAttribute> then
                let arg = cad.ConstructorArguments.[0]
                if arg.ArgumentType = typeof<int> then
                    UnionCaseConstantEncoding.Int (unbox arg.Value)
                elif arg.ArgumentType = typeof<float> then
                    UnionCaseConstantEncoding.Float (unbox arg.Value)
                elif arg.ArgumentType = typeof<bool> then
                    UnionCaseConstantEncoding.Bool (unbox arg.Value)
                elif arg.ArgumentType = typeof<string> then
                    UnionCaseConstantEncoding.String (unbox arg.Value)
                else failwith "Invalid ConstantAttribute."
                |> Some
            else None)

    let GetUnionEncoding (t: System.Type) =
        let discr =
            match getDiscriminatorName t with
            | None -> StandardField
            | Some None -> NoField (inferredCasesTable t)
            | Some (Some n) -> NamedField n
        let cases =
            FST.GetUnionCases t
            |> Array.mapi (fun i uci ->
                let name =
                    match discr with
                    | StandardField -> "$" + string i
                    | _ -> GetName uci
                if isInlinableRecordCase uci then
                    InlineRecord(name = name, record = uci.GetFields().[0].PropertyType)
                else
                    match getUnionCaseConstantEncoding uci with
                    | Some e -> Constant e
                    | None ->
                        let dateTimeFormats =
                            uci.GetCustomAttributesData()
                            |> Array.ofSeq
                            |> Array.choose (fun cad ->
                                if cad.Constructor.DeclaringType = typeof<A.DateTimeFormatAttribute> &&
                                    cad.ConstructorArguments.Count = 2 then
                                    let args = cad.ConstructorArguments
                                    Some (args.[0].Value :?> string, args.[1].Value :?> string)
                                else None)
                        let args = uci.GetFields() |> Array.map (fun f ->
                            let flags =
                                dateTimeFormats
                                |> Seq.tryPick (fun (k, v) ->
                                    if k = f.Name then Some (DateTimeFormat v) else None)
                                |> Option.toArray
                            GetName f, f.PropertyType, flags)
                        Normal(name = name, args = args))
        discr, cases

open Internal

let unmakeList<'T> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [
        for v in unbox<list<'T>> x ->
            dV (box v)
    ]

let unionEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        t.GetGenericArguments().[0]
        |> callGeneric <@ unmakeList @> dE ta
    else
    let tR = FSV.PreComputeUnionTagReader(t, flags)
    let cs =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            match getUnionCaseConstantEncoding c with
            | Some (UnionCaseConstantEncoding.Int i) -> Choice1Of2 (EncodedNumber (string i))
            | Some (UnionCaseConstantEncoding.Float f) -> Choice1Of2 (EncodedNumber (string f))
            | Some (UnionCaseConstantEncoding.Bool b) -> Choice1Of2 (if b then EncodedTrue else EncodedFalse)
            | Some (UnionCaseConstantEncoding.String s) -> Choice1Of2 (EncodedString s)
            | None ->
                let r = FSV.PreComputeUnionReader(c, flags)
                let fields = c.GetFields()
                let r, fields =
                    if i.ConciseRepresentation && isInlinableRecordCase c then
                        let rt = fields.[0].PropertyType
                        let rr = FSV.PreComputeRecordReader(rt, flags)
                        let r x = rr (r x).[0]
                        r, FST.GetRecordFields(rt, flags)
                    else r, fields
                let fs = fields |> Array.mapi (fun k f ->
                    let ta = TAttrs.Get(i, f.PropertyType, f, c)
                    i.GetEncodedUnionFieldName f k, encodeOptionalField dE ta)
                Choice2Of2 (r, fs))
    let encodeTag = i.EncodeUnionTag t
    let addTag = i.AddTag t
    fun (x: obj) ->
        match x with
        | null ->
            EncodedObject (Option.toList (encodeTag 0))
            |> addTag
        | o when t.IsAssignableFrom(o.GetType()) ->
            let tag = tR o
            match cs.[tag] with
            | Choice1Of2 constant -> constant
            | Choice2Of2 (r, fs) ->
                let data =
                    [
                        for f, d in Array.map2 (fun (f, e) x -> (f, e x)) fs (r o) do
                            if d.IsSome then yield f, d.Value
                    ]
                let data =
                    match encodeTag tag with
                    | Some kv -> kv :: data
                    | None -> data
                EncodedObject data
                |> addTag
        | x ->
            raise EncoderException

let makeList<'T> (dV: Value -> obj) = function
    | Array vs -> vs |> List.map (unbox<'T> << dV) |> box
    | _ -> raise DecoderException

let unionDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        t.GetGenericArguments().[0]
        |> callGeneric <@ makeList @> dD ta
    else
    let cases = FST.GetUnionCases(t, flags)
    let cs =
        cases
        |> Array.map (fun c ->
            let mk = FSV.PreComputeUnionConstructor(c, flags)
            let fields = c.GetFields()
            let mk, fields =
                if i.ConciseRepresentation && isInlinableRecordCase c then
                    let rt = fields.[0].PropertyType
                    let mkR = FSV.PreComputeRecordConstructor(rt, flags)
                    let mk x = mk [|mkR x|]
                    mk, FST.GetRecordFields(rt, flags)
                else mk, fields
            let fs =
                fields
                |> Array.mapi (fun k f ->
                    let ta = TAttrs.Get(i, f.PropertyType, f, c)
                    i.GetEncodedUnionFieldName f k, decodeOptionalField dD ta)
            (mk, fs))
    let consts =
        let c =
            cases
            |> Array.choose (fun c ->
                let mk() = FSV.PreComputeUnionConstructor(c, flags) [||]
                getUnionCaseConstantEncoding c
                |> Option.map (function
                    | UnionCaseConstantEncoding.Int i -> (Number (string i), mk())
                    | UnionCaseConstantEncoding.Float f -> (Number (string f), mk())
                    | UnionCaseConstantEncoding.Bool b -> ((if b then True else False), mk())
                    | UnionCaseConstantEncoding.String s -> (String s, mk())
                )
            )
        let consts = Dictionary()
        for k, v in c do consts.Add(k, v)
        consts
    let k = cs.Length
    let getTag = i.GetUnionTag t
    fun (x: Value) ->
        match x with
        | Object fields ->
            let get = table fields
            let tag =
                match getTag get with
                | Some tag -> tag
                | None -> raise DecoderException
            let (mk, fs) = cs.[tag]
            fs
            |> Array.map (fun (f, e) -> e (get f))
            |> mk
        | v ->
            match consts.TryGetValue v with
            | true, x -> x
            | false, _ -> raise DecoderException

let recordEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let mt = R.TypeDefinition.FromType t
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f ->
            let r = FSV.PreComputeRecordFieldReader f
            let ta = TAttrs.Get(i, f.PropertyType, f)
            (i.GetEncodedFieldName t f.Name, r, encodeOptionalField dE ta))
    fun (x: obj) ->
        match x with
        | null ->
            raise EncoderException
        | o when o.GetType() = t ->
            fs
            |> Array.choose (fun (n, r, enc) ->
                enc (r o) |> Option.map (fun e -> (n, e)))
            |> Array.toList
            |> EncodedObject
            |> i.AddTag t
        | _ ->
            raise EncoderException

let recordDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let mt = R.TypeDefinition.FromType t
    let mk = FSV.PreComputeRecordConstructor(t, flags)
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f ->
            let ta = TAttrs.Get(i, f.PropertyType, f)
            (i.GetEncodedFieldName t f.Name, decodeOptionalField dD ta))
    fun (x: Value) ->
        match x with
        | Object fields ->
            let get = table fields
            fs
            |> Array.map (fun (n, dec) -> dec (get n))
            |> mk
        | _ ->
            raise DecoderException

let fieldFlags =
    System.Reflection.BindingFlags.Instance
    ||| System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

exception NoEncodingException of System.Type with
    override this.Message =
        "No JSON encoding for " + string this.Data0

type FS = System.Runtime.Serialization.FormatterServices

let getObjectFields (t: System.Type) =
    t.GetFields fieldFlags
    |> Seq.filter (fun f ->
        let nS =
            f.Attributes &&&
            System.Reflection.FieldAttributes.NotSerialized
        f.DeclaringType.IsSerializable && int nS = 0)
    |> Seq.toArray

let unmakeDictionary<'T> (dE: obj -> Encoded) (x: obj) =
    EncodedObject [
        for KeyValue(k, v) in unbox<Dictionary<string, 'T>> x ->
            k, dE (box v)
    ]

let objectEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if t = typeof<System.DateTime> then
        fun (x: obj) ->
            match x with
            | :? System.DateTime as t -> i.EncodeDateTime ta t
            | _ -> raise EncoderException
    elif i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<Dictionary<_,_>> &&
        t.GetGenericArguments().[0] = typeof<string>
    then
        t.GetGenericArguments().[1]
        |> callGeneric <@ unmakeDictionary @> dE ta
    elif not t.IsSerializable then
        raise (NoEncodingException t)
    else
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let es = fs |> Array.map (fun f ->
        let ta = TAttrs.Get(i, f.FieldType, f)
        (i.GetEncodedFieldName f.DeclaringType f.Name,
         encodeOptionalField dE ta))
    fun (x: obj) ->
        match x with
        | null ->
            EncodedNull
        | o when t.IsAssignableFrom(o.GetType()) ->
            let data = FS.GetObjectData(o, ms)
            (data, es)
            ||> Array.map2 (fun x (name, enc) ->
                enc x |> Option.map (fun e -> (name, e)))
            |> Array.choose id
            |> Array.toList
            |> EncodedObject
            |> i.AddTag t
        | _ ->
            raise EncoderException

let makeDictionary<'T> (dD: Value -> obj) = function
    | Object vs ->
        let d = Dictionary<string, 'T>()
        for k, v in vs do d.Add(k, unbox<'T>(dD v))
        box d
    | _ -> raise DecoderException

let objectDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if t = typeof<System.DateTime> then
        fun (x: Value) ->
            match i.DecodeDateTime ta x with
            | Some d -> box d
            | None -> raise DecoderException
    elif i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<Dictionary<_,_>> &&
        t.GetGenericArguments().[0] = typeof<string>
    then
        t.GetGenericArguments().[1]
        |> callGeneric <@ makeDictionary @> dD ta
    elif not t.IsSerializable then
        raise (NoEncodingException t)
    else
    match t.GetConstructor [||] with
    | null -> raise (NoEncodingException t)
    | _ -> ()
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let ds = fs |> Array.map (fun f ->
        let ta = TAttrs.Get(i, f.FieldType, f)
        (i.GetEncodedFieldName f.DeclaringType f.Name,
         decodeOptionalField dD ta))
    fun (x: Value) ->
        match x with
        | Null -> null
        | Object fields ->
            let get = table fields
            let obj = System.Activator.CreateInstance t
            let data =
                ds
                |> Seq.map (fun (n, dec) ->
                   dec (get n))
                |> Seq.toArray
            FS.PopulateObjectMembers(obj, ms, data)
        | _ ->
            raise DecoderException

let btree node left right height count = 
    EncodedObject [
        "Node", node  
        "Left", left
        "Right", right  
        "Height", EncodedNumber height
        "Count", EncodedNumber count  
    ]

let unmakeMap<'T> (dV: obj -> Encoded)  (x: obj) =
    EncodedObject [
        for KeyValue(k, v) in unbox<Map<string, 'T>> x ->
            k, dV (box v)
    ]

let mapEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise EncoderException
    if i.ConciseRepresentation && tg.[0] = typeof<string> then
        callGeneric <@ unmakeMap @> dE ta tg.[1]
    else
    let treeF = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let pair key value =
        EncodedObject [
            "Key", key
            "Value", value
        ]   
    let dK = dE (TAttrs.Get(i, tg.[0]))
    let dV = dE (TAttrs.Get(i, tg.[1]))
    let tR = FSV.PreComputeUnionTagReader(treeF.FieldType, flags)
    let uR =
        FST.GetUnionCases(treeF.FieldType, flags)
        |> Array.map (fun c -> FSV.PreComputeUnionReader(c, flags))
    fun (x: obj) ->
        let rec encNode v = 
            match v with
            | null -> EncodedNull, 0
            | _ ->
            match tR v with
            | 0 -> EncodedNull, 0
            | 1 ->
                let u = uR.[1] v
                btree (pair (dK u.[0]) (dV u.[1])) EncodedNull EncodedNull "1" "1", 1
            | 2 ->
                let u = uR.[2] v
                let l, lc = encNode u.[2]
                let r, rc = encNode u.[3]
                let c = 1 + lc + rc
                btree (pair (dK u.[0]) (dV u.[1])) l r (string u.[4]) (string c), c 
            | _ -> raise EncoderException     
        let tr = fst (encNode (treeF.GetValue x))
        EncodedObject [ "tree", tr ] |> i.AddTag t

let makeMap<'T> (dV: Value -> obj) = function
    | Object vs ->
        Map.ofList<string, 'T>(
            vs |> List.map (fun (k, v) -> k, unbox<'T> (dV v))
        )
        |> box
    | _ -> raise DecoderException

let mapDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise DecoderException
    if i.ConciseRepresentation && tg.[0] = typeof<string> then
        callGeneric <@ makeMap @> dD ta tg.[1]
    else
    let dK = dD (TAttrs.Get(i, tg.[0]))
    let dV = dD (TAttrs.Get(i, tg.[1]))
    let tt = typedefof<System.Tuple<_,_>>.MakeGenericType(tg.[0], tg.[1])
    let cT = FSV.PreComputeTupleConstructor(tt)
    fun (x: Value) ->
        let rec walk fields =
            seq {
                for f in fields do
                    match f with
                    | "Node", Object [ "Key", k; "Value", v ] -> 
                        yield cT [| dK k; dV v |]
                    | ("Left" | "Right"), Object st -> 
                        yield! walk st
                    | _ -> ()
            }
        match x with
        | Null -> System.Activator.CreateInstance(t)
        | Object [ "tree", Object tr ] ->
            let els = walk tr |> Array.ofSeq
            let tEls = System.Array.CreateInstance(tt, els.Length)
            System.Array.Copy(els, tEls, els.Length)
            System.Activator.CreateInstance(t, tEls)
        | _ -> raise DecoderException

let unmakeSet<'T when 'T : comparison> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [for v in unbox<Set<'T>> x -> dV v]

let setEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    if i.ConciseRepresentation then
        callGeneric <@ unmakeSet @> dE ta tg.[0]
    else
    let dI = dE (TAttrs.Get(i, tg.[0]))
    let treeF = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let tR = FSV.PreComputeUnionTagReader(treeF.FieldType, flags)
    let uR =
        FST.GetUnionCases(treeF.FieldType, flags)
        |> Array.map (fun c -> FSV.PreComputeUnionReader(c, flags))
    fun (x: obj) ->
        let rec encNode v = 
            match v with
            | null -> EncodedNull, 0
            | _ ->
            match tR v with
            | 0 -> EncodedNull, 0
            | 1 ->
                let u = uR.[1] v
                let l, lc = encNode u.[1]
                let r, rc = encNode u.[2]
                let c = 1 + lc + rc
                btree (dI u.[0]) l r (string u.[3]) (string c), c
            | 2 ->
                let u = uR.[2] v
                btree (dI u.[0]) EncodedNull EncodedNull "1" "1", 1
            | _ -> raise EncoderException     
        let tr = fst (encNode (treeF.GetValue x))
        EncodedObject [ "tree", tr ] |> i.AddTag t

let makeSet<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        Set.ofList<'T>(vs |> List.map (unbox<'T> << dV))
        |> box
    | _ -> raise DecoderException

let makeSet'<'T when 'T : comparison> (dV: Value -> obj) (xs: seq<obj>) =
    Set.ofSeq (Seq.cast<'T> xs)
    |> box

let setDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    if i.ConciseRepresentation then
        callGeneric <@ makeSet @> dD ta tg.[0]
    else
    let dI = dD (TAttrs.Get(i, tg.[0]))
    let mk = callGeneric <@ makeSet' @> dD ta tg.[0]
    let rec walk fields =
        seq {
            for f in fields do
                match f with
                | "Node", n -> 
                    yield dI n
                | ("Left" | "Right"), Object st -> 
                    yield! walk st
                | _ -> ()
        }
    function
        | Null -> mk Seq.empty
        | Object [ "tree", Object tr ] -> mk (walk tr)
        | _ -> raise DecoderException

let enumEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let uT = System.Enum.GetUnderlyingType t
    let uE = dE { ta with Type = uT }
    fun (x: obj) ->
        uE (System.Convert.ChangeType(x, uT)) : Encoded

let enumDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let uT = System.Enum.GetUnderlyingType t
    let uD = dD { ta with Type = uT }
    fun (x: Value) ->
        let y : obj = uD x
        System.Enum.ToObject(t, y)

let getEncoding scalar array tuple union record enu map set obj wrap (fo: FormatSettings)
                (cache: Dictionary<_,_>) =
    let recurse ta =
        lock cache <| fun () ->
            cache.[ta] <-
                Choice1Of2 (fun v ->
                    let ct = lock cache <| fun () -> cache.[ta]
                    match ct with
                    | Choice1Of2 f -> f v
                    | Choice2Of2 d -> raise (NoEncodingException d)
                )
    let rec get (ta: TAttrs) =
        let derive dD =
            try
                if ta.Type.IsArray then
                    if ta.Type.GetArrayRank() = 1 then
                        Choice1Of2 (array dD fo ta)
                    else Choice2Of2 ta.Type
                elif FST.IsTuple ta.Type then
                    Choice1Of2 (tuple dD fo ta)
                elif FST.IsUnion (ta.Type, flags) then
                    recurse ta
                    Choice1Of2 (union dD fo ta)
                elif FST.IsRecord (ta.Type, flags) then
                    recurse ta
                    Choice1Of2 (record dD fo ta)
                elif ta.Type.IsEnum then
                    Choice1Of2 (enu dD fo ta)
                else
                    let tn =
                        if ta.Type.IsGenericType 
                        then Some (ta.Type.GetGenericTypeDefinition().FullName)
                        else None
                    match tn with
                    | Some "Microsoft.FSharp.Collections.FSharpMap`2" -> Choice1Of2 (map dD fo ta)
                    | Some "Microsoft.FSharp.Collections.FSharpSet`1" -> Choice1Of2 (set dD fo ta)
                    | _ -> 
                        recurse ta
                        Choice1Of2 (obj dD fo ta)
            with NoEncodingException t ->
                Choice2Of2 t
        if ta.Type = null then Choice2Of2 ta.Type else
            match serializers.TryGetValue ta.Type with
            | true, x when Option.isSome (scalar x) ->
                Choice1Of2 (scalar x).Value
            | _ ->
                let d =
                    lock cache <| fun () ->
                        match cache.TryGetValue ta with
                        | true, d -> Some d
                        | _ -> None
                match d with
                | Some d -> d
                | None ->
                    let dD ta =
                        match get ta with
                        | Choice1Of2 d -> d
                        | Choice2Of2 d -> raise (NoEncodingException d)
                    let d = derive (wrap dD)
                    lock cache <| fun () ->
                        cache.[ta] <- d
                    d
    get

module M = WebSharper.Core.Metadata

let defaultGetUnionTag t =
    let k = FST.GetUnionCases(t, flags).Length
    fun get ->
        match get "$" with
        | Some (Number n) ->
            match System.Int32.TryParse n with
            | true, tag when tag >= 0 && tag < k -> Some tag
            | _ -> None
        | _ -> None

let inferUnionTag t =
    let findInTable table get =
        table |> List.tryPick (fun (name, tag) ->
            get name |> Option.map (fun _ -> tag))
    findInTable (inferredCasesTable t)
        

let defaultEncodeUnionTag _ (tag: int) =
    Some ("$", EncodedNumber (string tag))

module TypedProviderInternals =

    let addTag (i: M.Info) (t: System.Type) =
        let mt = R.TypeDefinition.FromType t
        match i.GetAddress mt with
        | None -> id
        | Some a ->
            function
            | EncodedObject fs -> EncodedInstance (a, fs)
            | v -> v

    let pack encoded =
        let dict = Dictionary()
        let encT x =
            match dict.TryGetValue x with
            | true, y -> Number (string y)
            | _ ->
                let y = dict.Count
                dict.[x] <- y
                Number (string y)
        let rec pk = function
            | EncodedNull -> Null
            | EncodedTrue -> True
            | EncodedFalse -> False
            | EncodedNumber x -> Number x
            | EncodedString x -> String x
            | EncodedArray xs -> Array (List.map pk xs)
            | EncodedObject xs -> Object [VALUE, pko xs]
            | EncodedInstance (a, x) -> Object [TYPE, encT a; VALUE, pko x]
        and pko xs =
            Object (xs |> List.map (fun (a, b) -> (a, pk b)))
        let data = pk encoded
        let rec encA acc x =
            match x with
            | P.Global x -> Array (String x :: acc)
            | P.Local (x, y) -> encA (String y :: acc) x
        let types =
            Array (List.ofSeq (Seq.map (encA []) dict.Keys))
        Object [
            TYPES, types
            DATA, data
        ]

    let epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)

    let format info =
        {
            AddTag = addTag info
            GetEncodedFieldName = fun t ->
                info.GetFieldName (R.TypeDefinition.FromType t)
            GetUnionTag = defaultGetUnionTag
            EncodeUnionTag = defaultEncodeUnionTag
            GetEncodedUnionFieldName = fun _ i -> "$" + string i
            EncodeDateTime = fun _ (d: System.DateTime) ->
                EncodedNumber (string (d.ToUniversalTime() - epoch).TotalMilliseconds)
            DecodeDateTime = fun _ -> function
                | Number x ->
                    match tryParseDouble x with
                    | true, x -> Some (epoch + System.TimeSpan.FromMilliseconds x)
                    | _ -> None
                | _ -> None
            ConciseRepresentation = false
            Pack = pack
        }

module PlainProviderInternals =

    let culture = System.Globalization.CultureInfo.InvariantCulture
    let dtstyle = System.Globalization.DateTimeStyles.None

    let rec flatten encoded =
        let rec pk = function
            | EncodedNull -> Null
            | EncodedTrue -> True
            | EncodedFalse -> False
            | EncodedNumber x -> Number x
            | EncodedString x -> String x
            | EncodedArray xs -> Array (List.map pk xs)
            | EncodedObject xs -> pko xs
            | EncodedInstance (_, xs) -> pko xs
        and pko xs =
            Object (xs |> List.map (fun (a, b) -> (a, pk b)))
        pk encoded

    let format =
        {
            AddTag = fun _ -> id
            GetEncodedFieldName = fun t ->
                let d = Dictionary()
                let fields =
                    if FST.IsRecord t then
                        FST.GetRecordFields(t, flags)
                        |> Seq.cast<System.Reflection.MemberInfo>
                    else
                        getObjectFields t
                        |> Seq.cast<System.Reflection.MemberInfo>
                for f in fields do
                    if not (d.ContainsKey f.Name) then
                        d.Add(f.Name, TAttrs.GetName f)
                fun n ->
                    match d.TryGetValue n with
                    | true, n -> n
                    | false, _ -> n
            GetUnionTag = fun t ->
                match getDiscriminatorName t with
                | None -> defaultGetUnionTag t
                | Some None -> inferUnionTag t
                | Some (Some n) ->
                    let names =
                        Map [
                            for c in FST.GetUnionCases(t, flags) ->
                                (String (TAttrs.GetName c), c.Tag)
                        ]
                    fun get ->
                        get n |> Option.bind names.TryFind
            EncodeUnionTag = fun t ->
                match getDiscriminatorName t with
                | None -> defaultEncodeUnionTag t
                | Some None -> fun _ -> None
                | Some (Some n) ->
                    let tags =
                        [|
                            for c in FST.GetUnionCases(t, flags) ->
                                EncodedString (TAttrs.GetName c)
                        |]
                    fun tag -> Some (n, tags.[tag])
            GetEncodedUnionFieldName = fun p -> let n = TAttrs.GetName p in fun _ -> n
            EncodeDateTime = fun ta ->
                let fmt = defaultArg ta.DateTimeFormat "o"
                fun d -> EncodedString (d.ToString(fmt, culture))
            DecodeDateTime = fun ta ->
                let fmt =
                    match ta.DateTimeFormat with
                    | Some x -> [|x|]
                    // "o" only accepts 7 digits after the seconds,
                    // but JavaScript's Date.toISOString() only outputs 3.
                    // So we add a custom format to accept that too.
                    | None -> [|"o"; @"yyyy-MM-dd\Tmm:hh:ss.fff\Z"|]
                function
                | String s ->
                    match System.DateTime.TryParseExact(s, fmt, culture, dtstyle) with
                    | true, x -> Some x
                    | false, _ -> None
                | _ -> None
            ConciseRepresentation = true
            Pack = flatten
        }

[<Sealed>]
type Provider(fo: FormatSettings) =
    let decoders = Dictionary()
    let encoders = Dictionary()

    let defaultof (t: System.Type) =
        if t = typeof<string> then box "" else
        genLetMethod(<@ Unchecked.defaultof<_> @>, [|t|]).Invoke0()

    let getDefaultBuilder =
        getEncoding
            (fun _ -> Some defaultof)
            (fun dD i ta ->
                let x = box (System.Array.CreateInstance(ta.Type.GetElementType(), 0))
                fun _ -> x)
            (fun dD i ta ->
                let xs = FST.GetTupleElements ta.Type |> Array.map (fun t -> dD (TAttrs.Get(i, t)) t)
                let x = FSV.MakeTuple(xs, ta.Type)
                fun _ -> x)
            (fun dD i ta ->
                let uci = FST.GetUnionCases(ta.Type).[0]
                let xs = uci.GetFields() |> Array.map (fun f -> dD (TAttrs.Get(i, f.PropertyType, f)) f.PropertyType)
                let x = FSV.MakeUnion(uci, xs)
                fun _ -> x)
            (fun dD i ta ->
                let xs = FST.GetRecordFields ta.Type |> Array.map (fun f -> dD (TAttrs.Get(i, f.PropertyType, f)) f.PropertyType)
                let x = FSV.MakeRecord(ta.Type, xs)
                fun _ -> x)
            (fun dD i ta ->
                let x = defaultof ta.Type
                fun _ -> x)
            (fun dD i ta ->
                let x = genLetMethod(<@ Map.empty @>, ta.Type.GetGenericArguments()).Invoke0()
                fun _ -> x)
            (fun dD i ta ->
                let x = genLetMethod(<@ Set.empty @>, ta.Type.GetGenericArguments()).Invoke0()
                fun _ -> x)
            (fun _ _ _ _ -> null)
            id
            fo
            (Dictionary<_,_>())
        >> function
            | Choice1Of2 x -> x
            | Choice2Of2 x -> raise (NoDecoderException x)

    let getDecoder =
        getEncoding (fun {Decode=x} -> x)
            arrayDecoder
            tupleDecoder
            unionDecoder
            recordDecoder
            enumDecoder
            mapDecoder
            setDecoder
            objectDecoder
            id
            fo
            decoders
        >> function
            | Choice1Of2 x -> Decoder x
            | Choice2Of2 x -> raise (NoDecoderException x)

    let getEncoder =
        getEncoding (fun {Encode=x} -> x)
            arrayEncoder
            tupleEncoder
            unionEncoder
            recordEncoder
            enumEncoder
            mapEncoder
            setEncoder
            objectEncoder
            (fun dE ta ->
                if ta.Type.IsSealed || FST.IsUnion ta.Type then dE ta else
                fun x -> dE (if x = null then ta else { ta with Type = x.GetType() }) x)
            fo
            encoders
        >> function
            | Choice1Of2 x -> Encoder x
            | Choice2Of2 x -> raise (NoEncoderException x)

    static member Create() =
        Provider PlainProviderInternals.format

    static member CreateTyped (info: M.Info) =
        Provider (TypedProviderInternals.format info)

    member this.GetDecoder(t: System.Type) : Decoder =
        getDecoder (TAttrs.Get(fo, t))

    member this.GetEncoder(t: System.Type) : Encoder =
        getEncoder (TAttrs.Get(fo, t))

    member this.GetDecoder<'T>() : Decoder<'T> =
        let d = this.GetDecoder typeof<'T>
        Decoder<'T>(fun x ->
            match d.Decode x with
            | (:? 'T as x) -> x
            | _ -> raise DecoderException)

    member this.GetEncoder<'T>() : Encoder<'T> =
        let e = this.GetEncoder typeof<'T>
        Encoder<'T>(fun x -> e.Encode (box x))

    member this.BuildDefaultValue(t: System.Type) =
        getDefaultBuilder (TAttrs.Get(fo, t)) t

    member this.BuildDefaultValue<'T>() =
        match this.BuildDefaultValue typeof<'T> with
        | :? 'T as x -> x
        | _ -> raise DecoderException

    member this.Pack x = fo.Pack x

let Parse s =
    use r = new System.IO.StringReader(s)
    Read r

let Stringify v =
    use w = new System.IO.StringWriter()
    Write w v
    w.ToString()
