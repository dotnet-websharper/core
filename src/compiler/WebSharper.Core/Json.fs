// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open WebSharper
open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System.Linq.Expressions

//module P = WebSharper.Core.JavaScript.Packager
//module AST = WebSharper.Core.AST
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

let readStartedString (w: System.Text.StringBuilder) (tr: System.IO.TextReader) =
    let c (x: char) = w.Append x |> ignore
    let read () = tr.Read()
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

let readString w (tr: System.IO.TextReader) =
    match tr.Read() with
    | 34 ->
        readStartedString w tr
    | _ ->
        raise ReadException

let readIdent (w: System.Text.StringBuilder) (tr: System.IO.TextReader) =
    let c (x: char) = w.Append x |> ignore
    let read () = tr.Read()
    let peek () = tr.Peek()
    let isStartChar chr =
        (65 <= chr && chr <= 90)
        || (97 <= chr && chr <= 122)
        || chr = 95
        || chr = 36
    let isContChar chr =
        isStartChar chr
        || (48 <= chr && chr <= 57)
    match read () with
    | 34 -> readStartedString w tr
    | chr when isStartChar chr ->
        c (char chr)
        while (isContChar (peek ())) do
            c (read () |> char)
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
                let n = readIdent w tr
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
        if x = null then s "null" else
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

let Parse s =
    use r = new System.IO.StringReader(s)
    Read r

let Stringify v =
    use w = new System.IO.StringWriter()
    Write w v
    w.ToString()

exception Inner

exception DecoderException of value:Value * typ:System.Type with
    override this.Message =
        "Failed to deserialize value \"" + Stringify this.value + "\" as type " + string this.typ  

exception EncoderException

exception NoDecoderException of typ:System.Type with
    override this.Message =
        "No JSON decoder for " + string this.typ

exception NoEncoderException of typ:System.Type with
    override this.Message =
        "No JSON encoder for " + string this.typ

type Encoded =
    | EncodedNull
    | EncodedTrue
    | EncodedFalse
    | EncodedNumber of string
    | EncodedString of string
    | EncodedArray of list<Encoded>
    | EncodedObject of list<string * Encoded>
    | EncodedInstance of AST.Address * list<string * Encoded>
    | EncodedArrayInstance of AST.Address * list<Encoded>

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

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

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
        ///     * Map<_, _>, Dictionary<_> -> array of [key, value] arrays
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
                if cad.Constructor.DeclaringType = typeof<NameAttribute> ||
                    cad.Constructor.DeclaringType = typeof<CompiledNameAttribute> then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        defaultArg customName (^T : (member Name : string) (mi))

    static member Get(i: FormatSettings, t: System.Type, ?mi: #System.Reflection.MemberInfo, ?uci: Reflection.UnionCaseInfo, ?pi: System.Reflection.ParameterInfo) =
        let mcad =
            match mi with
            | Some mi -> mi.GetCustomAttributesData()
            | None -> [||] :> _
        let pcad =
            match pi with
            | Some pi -> pi.GetCustomAttributesData()
            | None -> [||] :> _
        let mcad = Seq.append mcad pcad
        let ucad =
            match uci with
            | Some uci -> uci.GetCustomAttributesData()
            | None -> [||] :> _
        let isOptionalField =
            t.IsGenericType &&
            t.GetGenericTypeDefinition() = typedefof<option<_>> &&
            (i.ConciseRepresentation ||
                (mcad |> Seq.exists (fun t ->
                    t.Constructor.DeclaringType = typeof<OptionalFieldAttribute>)))
        let isNullableUnion =
            i.ConciseRepresentation &&
            FST.IsUnion(t, flags) &&
            mcad |> Seq.exists (fun cad ->
                cad.Constructor.DeclaringType = typeof<CompilationRepresentationAttribute> &&
                let flags = cad.ConstructorArguments.[0].Value :?> CompilationRepresentationFlags
                flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0)
        let dateTimeFormat =
            mcad |> Seq.tryPick (fun cad ->
                if cad.Constructor.DeclaringType = typeof<DateTimeFormatAttribute> &&
                   cad.ConstructorArguments.Count = 1 then
                    Some (cad.ConstructorArguments.[0].Value :?> string)
                else None)
        let dateTimeFormat =
            match dateTimeFormat with
            | Some f -> Some f
            | None ->
                ucad |> Seq.tryPick (fun cad ->
                    if cad.Constructor.DeclaringType = typeof<DateTimeFormatAttribute> &&
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
            | _ -> raise (DecoderException(Number x, typeof<'T>))
        | x -> raise (DecoderException(x, typeof<'T>))
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

let tryParseSingle (x: string) = 
    System.Single.TryParse(x, 
        System.Globalization.NumberStyles.Float, 
        System.Globalization.NumberFormatInfo.InvariantInfo)

let tryParseDouble (x: string) = 
    System.Double.TryParse(x, 
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
    let encBool = function
        | true -> EncodedTrue
        | false -> EncodedFalse
    let decBool = function
        | True -> true
        | False -> false
        | x -> raise (DecoderException(x, typeof<bool>))
    add encBool decBool d
    let encChar (c: char) =
        EncodedString (string c)
    let decChar = function
        | String x ->
            match System.Char.TryParse x with
            | true, c -> c
            | _ -> raise (DecoderException(Number x, typeof<char>))
        | x -> raise (DecoderException(x, typeof<char>))
    add encChar decChar d
    let decString = function
        | String x -> x
        | x -> raise (DecoderException(x, typeof<string>))
    add EncodedString decString d
    let encTimeSpan (t: System.TimeSpan) =
        EncodedNumber (string t.TotalMilliseconds)
    let decTimeSpan = function
        | Number x ->
            match tryParseDouble x with
            | true, x -> System.TimeSpan.FromMilliseconds x
            | _ -> raise (DecoderException(Number x, typeof<System.TimeSpan>))
        | x -> raise (DecoderException(x, typeof<System.TimeSpan>))
    add encTimeSpan decTimeSpan d
    let encGuid (g: System.Guid) =
        EncodedString (string g)
    let decGuid = function
        | String g -> 
            match System.Guid.TryParse g with
            | true, g -> g
            | _ -> raise (DecoderException(String g, typeof<System.Guid>))
        | x -> raise (DecoderException(x, typeof<System.Guid>))
    add encGuid decGuid d   
    let encDecimal (d: decimal) =
        let b = System.Decimal.GetBits(d)
        EncodedArrayInstance (
            AST.Address.WSMain "Decimal", 
            b |> Seq.map (string >> EncodedNumber) |> List.ofSeq
        )
    let decDecimal = function
        | Object [ "mathjs", String "BigNumber"; "value", String d ] as x ->
            match System.Decimal.TryParse d with
            | true, d -> d
            | _ -> raise (DecoderException(x, typeof<decimal>)) 
        | x -> raise (DecoderException(x, typeof<decimal>))
    add encDecimal decDecimal d   
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
                raise (DecoderException(x, ta.Type))
        | _ ->
            raise (DecoderException(x, ta.Type))

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
            raise (DecoderException(x, ta.Type))

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
    | Lambda(_, Lambda(_, Call(None, m, [_;_])))                // callGeneric function
    | Lambda(_, Lambda(_, Lambda(_, Call(None, m, [_;_;_]))))   // callGeneric2 function
    | Call(None, m, []) -> // value
        FastInvoke.Compile(m.GetGenericMethodDefinition().MakeGenericMethod(ts))
    | _ -> failwithf "Json.genLetMethod: invalid expr passed: %A" e

let callGeneric (func: Expr<'f -> 'i -> 'o>) (dD: TAttrs -> 'f) ta (targ: System.Type) : 'i -> 'o =
    let m = genLetMethod(func, [|targ|])
    let dI = dD { ta with Type = targ }
    fun x -> unbox<'o> (m.Invoke2(dI, x))

let callGeneric2 (func: Expr<'f -> 'f -> 'i -> 'o>) (dD: TAttrs -> 'f) ta (targ1: System.Type) (targ2: System.Type) : 'i -> 'o =
    let m = genLetMethod(func, [|targ1; targ2|])
    let dI1 = dD { ta with Type = targ1 }
    let dI2 = dD { ta with Type = targ2 }
    fun x -> unbox<'o> (m.Invoke3(dI1, dI2, x))

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
        function Some v -> dec v | None -> raise (DecoderException(Null, ta.Type))

let isInlinableRecordCase (uci: Reflection.UnionCaseInfo) =
    let fields = uci.GetFields()
    fields.Length = 1 &&
    fields.[0].Name = "Item" &&
    FST.IsRecord(fields.[0].PropertyType, flags)

/// Some (Some x) if tagged [<NameUnionCases x>];
/// Some None if tagged [<NamedUnionCases>];
/// None if not tagged.
let getDiscriminatorName (t: System.Type) =
    t.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<NamedUnionCasesAttribute> then
            if cad.ConstructorArguments.Count = 1 then
                Some (Some (cad.ConstructorArguments.[0].Value :?> string))
            else Some None
        else None)

let inferredCasesTable t =
    let allCases =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let fields = c.GetFields()
            let fields =
                if isInlinableRecordCase c then
                    FST.GetRecordFields(fields.[0].PropertyType, flags)
                else fields
            let fields =
                fields
                |> Array.filter (fun f ->
                    let t = f.PropertyType
                    not (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>))
                |> Array.map TAttrs.GetName
            c.Tag, Set fields
        )
        |> Map.ofArray
    let findDistinguishingCase (cases: Map<int, Set<string>>) =
        cases
        |> Map.tryPick (fun t fs ->
            let allOtherFields =
                allCases
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
    buildTable [] allCases

module Internal =

    type TypedNull<'T> = | TypedNull

    let MakeTypedNull (t: System.Type) =
        let t = typedefof<TypedNull<_>>.MakeGenericType(t)
        FSV.MakeUnion(FST.GetUnionCases(t).[0], [||])

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
        | Null

    type UnionCaseEncoding =
        | Normal of name: string * args: (string * System.Type * UnionCaseArgFlag[])[]
        | InlineRecord of name: string * record: System.Type
        | Constant of value: UnionCaseConstantEncoding

    let getUnionCaseConstantEncoding (uci: Reflection.UnionCaseInfo) =
        let isNull = 
            uci.DeclaringType.GetCustomAttributesData()
            |> Seq.exists (fun a ->
                a.Constructor.DeclaringType = typeof<CompilationRepresentationAttribute>
                && obj.Equals(a.ConstructorArguments.[0].Value, CompilationRepresentationFlags.UseNullAsTrueValue)
            )
            && (FST.GetUnionCases uci.DeclaringType).Length < 4
            && (FST.GetUnionCases uci.DeclaringType |> Seq.tryFind (fun c -> c.GetFields().Length = 0) = Some uci) 
        if isNull then Some UnionCaseConstantEncoding.Null else
        uci.GetCustomAttributesData()
        |> Seq.tryPick (fun cad ->
            if cad.Constructor.DeclaringType = typeof<ConstantAttribute> then
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
            FST.GetUnionCases(t, flags)
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
                                if cad.Constructor.DeclaringType = typeof<DateTimeFormatAttribute> &&
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
open System.Collections.Concurrent

let unmakeList<'T> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [
        for v in unbox<list<'T>> x ->
            dV (box v)
    ]

let unionEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let t, isTypedNull =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<TypedNull<_>> then
            t.GetGenericArguments().[0], true
        else t, false
    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        let x =
            t.GetGenericArguments().[0]
            |> callGeneric <@ unmakeList @> dE ta
        if i.ConciseRepresentation then x else x >> i.AddTag t
    elif t = typeof<Encoded> then
        unbox
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
            | Some UnionCaseConstantEncoding.Null -> Choice1Of2 EncodedNull
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
    if isTypedNull then fun _ -> EncodedNull else
    fun (x: obj) ->
        match x with
        | null -> EncodedNull
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
    | x -> raise (DecoderException(x, typeof<list<'T>>))

let unionDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        t.GetGenericArguments().[0]
        |> callGeneric <@ makeList @> dD ta
    elif t = typeof<Encoded> then
        Encoded.Lift >> box
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
                    | UnionCaseConstantEncoding.Null -> (Null, null)
                )
            )
        let consts = Dictionary()
        for k, v in c do consts.Add(k, v)
        consts
    let getTag = i.GetUnionTag t
    let nullConstant =
        match consts.TryGetValue (String null) with
        | true, x -> x
        | false, _ -> null
    fun (x: Value) ->
        match x with
        | Object fields ->
            let get = table fields
            let tag =
                match getTag get with
                | Some tag -> tag
                | None -> raise (DecoderException(x, ta.Type))
            let (mk, fs) = cs.[tag]
            fs
            |> Array.map (fun (f, e) -> e (get f))
            |> mk
        | Null -> nullConstant
        | v ->
            match consts.TryGetValue v with
            | true, x -> x
            | false, _ -> raise (DecoderException(v, ta.Type))

let recordEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
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
            raise (DecoderException(x, ta.Type))

let fieldFlags =
    System.Reflection.BindingFlags.Instance
    ||| System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let fieldFlagsDeclOnly =
    fieldFlags
    ||| System.Reflection.BindingFlags.DeclaredOnly

exception NoEncodingException of System.Type with
    override this.Message =
        "No JSON encoding for " + string this.Data0

type FS = System.Runtime.Serialization.FormatterServices

let getObjectFields (t: System.Type) =
    // FlattenHierarchy flag is not enough to collect
    // backing fields of auto-properties on base classes 
    let getDecl (t: System.Type) = 
        t.GetFields fieldFlagsDeclOnly
        |> Seq.filter (fun f ->
            let nS =
                f.Attributes &&&
                System.Reflection.FieldAttributes.NotSerialized
            int nS = 0
        )
    let rec getAll (t: System.Type) =
        match t.BaseType with
        | null -> Seq.empty // this is a System.Object
        | b when b.FullName = "System.Web.UI.Control" -> Seq.empty // Don't serialize web control private fields
        | b -> Seq.append (getAll b) (getDecl t)
    getAll t |> Array.ofSeq

let unmakeFlatDictionary<'T> (dE: obj -> Encoded) (x: obj) =
    EncodedObject [
        for KeyValue(k, v) in unbox<Dictionary<string, 'T>> x ->
            k, dE (box v)
    ]

let unmakeArrayDictionary<'K, 'V when 'K : equality> (dK: obj -> Encoded) (dV: obj -> Encoded) (x: obj) =
    EncodedArray [
        for KeyValue(k, v) in unbox<Dictionary<'K, 'V>> x ->
            EncodedArray [dK (box k); dV (box v)]
    ]

let objectEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if t = typeof<System.DateTime> then
        fun (x: obj) ->
            match x with
            | :? System.DateTime as t -> i.EncodeDateTime ta t
            | _ -> raise EncoderException
    elif t = typeof<System.DateTimeOffset> then
        fun (x: obj) ->
            match x with
            | :? System.DateTimeOffset as t -> 
                EncodedObject [
                    "d", i.EncodeDateTime ta t.UtcDateTime
                    "o", EncodedNumber (string t.Offset.TotalMinutes)
                ]
            | _ -> raise EncoderException
    elif t = typeof<unit> then
        fun _ -> EncodedNull
    elif i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<Dictionary<_,_>> 
    then
        let ga = t.GetGenericArguments()
        if t.GetGenericArguments().[0] = typeof<string> then
            callGeneric <@ unmakeFlatDictionary @> dE ta ga.[1]
        else
            callGeneric2 <@ unmakeArrayDictionary @> dE ta ga.[0] ga.[1]
    else
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let es = 
        if t.IsValueType then
            fs |> Array.map (fun f ->
                let ta = TAttrs.Get(i, f.FieldType, f)
                (i.GetEncodedFieldName f.DeclaringType (f.Name.TrimEnd('@')),
                 encodeOptionalField dE ta))
        else
            fs |> Array.map (fun f ->
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

let makeFlatDictionary<'T> (dD: Value -> obj) = function
    | Object vs ->
        let d = Dictionary<string, 'T>()
        for k, v in vs do d.Add(k, unbox<'T>(dD v))
        box d
    | x -> raise (DecoderException(x, typeof<Dictionary<string,'T>>))

let makeArrayDictionary<'K, 'V when 'K : equality> (dK: Value -> obj) (dV: Value -> obj) = function
    | Array vs ->
        let d = Dictionary<'K, 'V>()
        for e in vs do
            match e with
            | Array [k; v] -> d.Add(unbox<'K>(dK k), unbox<'V>(dV v))
            | x -> raise (DecoderException(x, typeof<Dictionary<'K,'V>>))
        box d
    | x -> raise (DecoderException(x, typeof<Dictionary<'K,'V>>))

let rec decodeObj value =
    match value with
    | Null -> null
    | True -> box true
    | False -> box false
    | Number x ->
        match System.Int32.TryParse x with
        | true, n -> box n
        | false, _ ->
            match System.Double.TryParse x with
            | true, f -> box f
            | false, _ -> raise (DecoderException(value, typeof<obj>))
    | String s -> box s
    | Array xs ->
        box [| for x in xs -> decodeObj x |]
    | Object xs ->
        let d = Dictionary()
        for k, v in xs do d.Add(k, decodeObj v)
        box d

let objectDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    if t = typeof<System.DateTime> then
        fun (x: Value) ->
            match i.DecodeDateTime ta x with
            | Some d -> box d
            | None -> 
                // try to decode from serialized form of a DateTimeOffset too
                match x with
                | Object [ "d", d; "o", Number _ ] ->
                    match i.DecodeDateTime ta d with
                    | Some d -> box d 
                    | _ -> raise (DecoderException(x, typeof<System.DateTime>))
                | _ -> raise (DecoderException(x, typeof<System.DateTime>))
    elif t = typeof<System.DateTimeOffset> then
        fun (x: Value) ->
            match x with
            | Object [ "d", d; "o", Number o ] ->
                match i.DecodeDateTime ta d, System.Int32.TryParse o with
                | Some d, (true, o) -> 
                    let offset = System.TimeSpan.FromMinutes (float o)
                    box (new System.DateTimeOffset(d.Add(offset).Ticks, offset))
                | _ -> raise (DecoderException(x, typeof<System.DateTimeOffset>))
            | _ -> 
                // try to decode from an ISO string too
                match i.DecodeDateTime ta x with
                | Some d -> 
                    box (new System.DateTimeOffset(d.Ticks, System.TimeSpan.Zero))
                | None -> raise (DecoderException(x, typeof<System.DateTimeOffset>))
    elif t = typeof<unit> then
        function
        | Null -> box ()
        | x -> raise (DecoderException(x, typeof<unit>))
    elif i.ConciseRepresentation &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<Dictionary<_,_>>
    then
        let ga = t.GetGenericArguments()
        if t.GetGenericArguments().[0] = typeof<string> then
            callGeneric <@ makeFlatDictionary @> dD ta ga.[1]
        else
            callGeneric2 <@ makeArrayDictionary @> dD ta ga.[0] ga.[1]
    elif t = typeof<obj> then
        decodeObj
    elif t.IsValueType then
        let fs = t.GetFields fieldFlags
        match t.GetConstructor (fs |> Array.map (fun f -> f.FieldType)) with
        | null -> raise (NoEncodingException t)
        | _ ->
        let ds = fs |> Array.map (fun f ->
            let ta = TAttrs.Get(i, f.FieldType, f)
            (i.GetEncodedFieldName f.DeclaringType (f.Name.TrimEnd('@')),
             decodeOptionalField dD ta))
        fun (x: Value) ->
            match x with
            | Object fields ->
                let get = table fields
                let data =
                    ds
                    |> Seq.map (fun (n, dec) ->
                       dec (get n))
                    |> Seq.toArray
                System.Activator.CreateInstance(t, args = data)
            | x ->
                raise (DecoderException(x, ta.Type))
    else
    match t.GetConstructor [||] with
    | null -> 
        // look up singular parameterized constructor or the one annotated with JsonConstructorAttribute
        let ctors = t.GetConstructors()
        let ctor = 
            if ctors.Length = 1 then 
                ctors.[0] 
            else
                let jsonCtors = 
                    ctors |> Array.filter (fun c -> 
                        c.GetCustomAttributesData()
                        |> Seq.exists (fun a -> a.AttributeType.FullName = "System.Text.Json.Serialization.JsonConstructorAttribute")
                    )
                if jsonCtors.Length = 1 then 
                    jsonCtors.[0]  
                else
                    raise (NoEncodingException t)
        let ds = ctor.GetParameters() |> Array.map (fun p ->
            let prop = t.GetProperty(p.Name) |> Option.ofObj
            let ta = TAttrs.Get(i, p.ParameterType, ?mi = prop, pi = p)
            let fname =
                let bfName = "<" + p.Name + ">k__BackingField"
                if t.GetField(bfName) |> isNull then
                    bfName
                else
                    p.Name
            (i.GetEncodedFieldName t fname,
             decodeOptionalField dD ta))
        fun (x: Value) ->
            match x with
            | Null -> null
            | Object fields ->
                let get = table fields
                let data =
                    ds
                    |> Seq.map (fun (n, dec) ->
                       dec (get n))
                    |> Seq.toArray
                ctor.Invoke(data)
            | x ->
                raise (DecoderException(x, ta.Type))
    | _ ->
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
            | x ->
                raise (DecoderException(x, ta.Type))

let btree node left right height count = 
    EncodedObject [
        match left with Some l -> yield "Left", l | None -> ()
        match right with Some r -> yield "Right", r | None -> ()
        yield "Node", node  
        yield "Height", EncodedNumber height
        yield "Count", EncodedNumber count  
    ]

let unmakeFlatMap<'T> (dV: obj -> Encoded)  (x: obj) =
    EncodedObject [
        for KeyValue(k, v) in unbox<Map<string, 'T>> x ->
            k, dV (box v)
    ]

let unmakeArrayMap<'K, 'T when 'K : comparison> (dK: obj -> Encoded) (dV: obj -> Encoded) (x: obj) =
    EncodedArray [
        for KeyValue(k, v) in unbox<Map<'K, 'T>> x ->
            EncodedArray [ dK (box k); dV (box v) ]
    ]

let compileFieldAccessor<'T> (fi: FieldInfo) (t: Type) =
    let param = Expression.Parameter (typeof<obj>, "param")
    let expr = 
        Expression.Lambda<Func<obj, 'T>>(
            Expression.Convert(Expression.Field(Expression.Convert(param, t), fi), typeof<'T>)
        , param)    
    expr.Compile()

let compileFieldAccessorByName<'T> (f: string) (t: Type) =
    let fi = t.GetField(f, fieldFlags)
    compileFieldAccessor<'T> fi t

let mapEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise EncoderException
    if i.ConciseRepresentation then
        if tg.[0] = typeof<string> then
            callGeneric <@ unmakeFlatMap @> dE ta tg.[1]
        else
            callGeneric2 <@ unmakeArrayMap @> dE ta tg.[0] tg.[1]
    else
    let treeFI = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let treeF = t |> compileFieldAccessor<obj> treeFI
    let pair key value =
        EncodedObject [
            "Key", key
            "Value", value
        ]   
    let dK = dE (TAttrs.Get(i, tg.[0]))
    let dV = dE (TAttrs.Get(i, tg.[1]))
    let mapTreeTy = treeFI.FieldType
    let heightF = mapTreeTy |> compileFieldAccessorByName<int> "h"
    let keyF = mapTreeTy |> compileFieldAccessorByName<obj> "k"
    let valueF = mapTreeTy |> compileFieldAccessorByName<obj> "v"
    let mapTreeNodeTy =
        mapTreeTy.Assembly.GetType("Microsoft.FSharp.Collections.MapTreeNode`2")
            .MakeGenericType(mapTreeTy.GenericTypeArguments)
    let leftF = mapTreeNodeTy |> compileFieldAccessorByName<obj> "left"
    let rightF = mapTreeNodeTy |> compileFieldAccessorByName<obj> "right"

    fun (x: obj) ->
        let rec encNode v = 
            match v with
            | null -> None, 0
            | _ ->
            match heightF.Invoke(v) with
            | 0 -> None, 0 
            | 1 ->
                let nk = keyF.Invoke(v)
                let nv = valueF.Invoke(v)
                Some (btree (pair (dK nk) (dV nv)) None None "1" "1"), 1
            | h ->
                let nk = keyF.Invoke(v)
                let nv = valueF.Invoke(v)
                let l, lc = encNode (leftF.Invoke(v))
                let r, rc = encNode (rightF.Invoke(v))
                let c = 1 + lc + rc
                Some (btree (pair (dK nk) (dV nv)) l r (string h) (string c)), c
        match encNode (treeF.Invoke(x)) with
        | Some tr, _ -> EncodedObject [ "tree", tr ]
        | None, _ -> EncodedObject []
        |> i.AddTag t

/// Decode a Map<string, _> from { key: value, ... } JSON object
let makeFlatMap<'T> (dV: Value -> obj) = function
    | Object vs ->
        Map.ofList<string, 'T>(
            vs |> List.map (fun (k, v) -> k, unbox<'T> (dV v))
        )
        |> box
    | x -> raise (DecoderException(x, typeof<Map<string, 'T>>))

/// Decode a Map<_, _> from [ [key, value], ... ] JSON object
let makeArrayMap<'K, 'V when 'K : comparison> (dK: Value -> obj) (dV: Value -> obj) = function
    | Array vs ->
        Map.ofList<'K, 'V>(
            vs |> List.map (function
            | Array [k; v] -> unbox<'K> (dK k), unbox<'V> (dV v)
            | x -> raise (DecoderException(x, typeof<Map<'K, 'V>>)))
        )
        |> box
    | x -> raise (DecoderException(x, typeof<Map<'K, 'V>>))

let mapDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if i.ConciseRepresentation then
        if tg.[0] = typeof<string> then
            callGeneric <@ makeFlatMap @> dD ta tg.[1]
        else
            callGeneric2 <@ makeArrayMap @> dD ta tg.[0] tg.[1]
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
        | Null
        | Object []
        | Object [ "tree", Null ] ->
            let tEls = System.Array.CreateInstance(tt, 0)
            System.Activator.CreateInstance(t, tEls)
        | Object [ "tree", Object tr ] ->
            let els = walk tr |> Array.ofSeq
            let tEls = System.Array.CreateInstance(tt, els.Length)
            System.Array.Copy(els, tEls, els.Length)
            System.Activator.CreateInstance(t, tEls)
        | _ -> raise (DecoderException(x, ta.Type))

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
    let treeFI = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let treeF = t |> compileFieldAccessor<obj> treeFI
    let setTreeTy = treeFI.FieldType
    let heightF = setTreeTy |> compileFieldAccessorByName<int> "h"
    let keyF = setTreeTy |> compileFieldAccessorByName<obj> "k"
    let setTreeNodeTy =
        setTreeTy.Assembly.GetType("Microsoft.FSharp.Collections.SetTreeNode`1")
            .MakeGenericType(setTreeTy.GenericTypeArguments)
    let leftF = setTreeNodeTy |> compileFieldAccessorByName<obj> "left"
    let rightF = setTreeNodeTy |> compileFieldAccessorByName<obj> "right"

    fun (x: obj) ->
        let rec encNode v = 
            match v with
            | null -> None, 0
            | _ ->
            match heightF.Invoke(v) with
            | 0 -> None, 0
            | 1 ->
                let nk = keyF.Invoke(v)
                Some (btree (dI nk) None None "1" "1"), 1
            | h ->
                let nk = keyF.Invoke(v)
                let l, lc = encNode (leftF.Invoke(v))
                let r, rc = encNode (rightF.Invoke(v))
                let c = 1 + lc + rc
                Some (btree (dI nk) l r (string h) (string c)), c
        match encNode (treeF.Invoke(x)) with
        | Some tr, _ -> EncodedObject [ "tree", tr ]
        | None, _ -> EncodedObject []
        |> i.AddTag t

let unmakeResizeArray<'T when 'T : comparison> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [for v in unbox<ResizeArray<'T>> x -> dV v]

let resizeArrayEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ unmakeResizeArray @> dE ta tg.[0]

let unmakeQueue<'T when 'T : comparison> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [for v in unbox<Queue<'T>> x -> dV v]

let queueEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ unmakeQueue @> dE ta tg.[0]

let unmakeStack<'T when 'T : comparison> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [for v in unbox<Stack<'T>> x -> dV v]

let stackEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ unmakeStack @> dE ta tg.[0]

let unmakeLinkedList<'T when 'T : comparison> (dV: obj -> Encoded) (x: obj) =
    EncodedArray [for v in unbox<LinkedList<'T>> x -> dV v]

let linkedListEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ unmakeLinkedList @> dE ta tg.[0]

let unmakeNullable<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> System.ValueType> (dV: obj -> Encoded) (x: obj) =
    if obj.ReferenceEquals(x, null) then EncodedNull else dV x    
           
let nbleEncoder dE (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ unmakeNullable @> dE ta tg.[0]

let makeSet<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        Set.ofList<'T>(vs |> List.map (unbox<'T> << dV))
        |> box
    | x -> raise (DecoderException(x, typeof<Set<'T>>))

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
        | Null
        | Object []
        | Object [ "tree", Null ] -> mk Seq.empty
        | Object [ "tree", Object tr ] -> mk (walk tr)
        | x -> raise (DecoderException(x, ta.Type))

let makeResizeArray<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        ResizeArray(vs |> Seq.map (unbox<'T> << dV))
        |> box
    | x -> raise (DecoderException(x, typeof<ResizeArray<'T>>))

let resizeArrayDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ makeResizeArray @> dD ta tg.[0]

let makeQueue<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        Queue(vs |> Seq.map (unbox<'T> << dV))
        |> box
    | x -> raise (DecoderException(x, typeof<Queue<'T>>))

let queueDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ makeQueue @> dD ta tg.[0]

let makeStack<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        Stack(vs |> List.map (unbox<'T> << dV) |> List.rev)
        |> box
    | x -> raise (DecoderException(x, typeof<Stack<'T>>))

let stackDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ makeStack @> dD ta tg.[0]

let makeLinkedList<'T when 'T : comparison> (dV: Value -> obj) = function
    | Array vs ->
        LinkedList(vs |> Seq.map (unbox<'T> << dV))
        |> box
    | x -> raise (DecoderException(x, typeof<LinkedList<'T>>))

let linkedListDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ makeLinkedList @> dD ta tg.[0]

let makeNullable<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> System.ValueType> (dV: Value -> obj) =
    function
        | Null -> null
        | x -> box (System.Nullable(unbox<'T> (dV x)))

let nbleDecoder dD (i: FormatSettings) (ta: TAttrs) =
    let t = ta.Type
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    callGeneric <@ makeNullable @> dD ta tg.[0]

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

type TypeEncoding<'a, 'b> = (TAttrs -> 'a -> 'b) -> FormatSettings -> TAttrs -> 'a -> 'b

type Encodings<'a, 'b> =
    {
        Scalar: Serializer -> option<'a -> 'b>
        Array: TypeEncoding<'a, 'b>
        Tuple: TypeEncoding<'a, 'b>
        Union: TypeEncoding<'a, 'b>
        Record: TypeEncoding<'a, 'b>
        Enum: TypeEncoding<'a, 'b>
        Map: TypeEncoding<'a, 'b>
        Set: TypeEncoding<'a, 'b>
        ResizeArray: TypeEncoding<'a, 'b>
        Queue: TypeEncoding<'a, 'b>
        Stack: TypeEncoding<'a, 'b>
        LinkedList: TypeEncoding<'a, 'b>
        Nullable: TypeEncoding<'a, 'b>
        Object: TypeEncoding<'a, 'b>
    }

module Encodings =

    let Decode =
        {
            Scalar = fun { Decode = x } -> x
            Array = arrayDecoder
            Tuple = tupleDecoder
            Union = unionDecoder
            Record = recordDecoder
            Enum = enumDecoder
            Map = mapDecoder
            Set = setDecoder
            ResizeArray = resizeArrayDecoder
            Queue = queueDecoder
            Stack = stackDecoder
            LinkedList = linkedListDecoder
            Nullable = nbleDecoder
            Object = objectDecoder
        }

    let Encode =
        {
            Scalar = fun { Encode = x } -> x
            Array = arrayEncoder
            Tuple = tupleEncoder
            Union = unionEncoder
            Record = recordEncoder
            Enum = enumEncoder
            Map = mapEncoder
            Set = setEncoder
            ResizeArray = resizeArrayEncoder
            Queue = queueEncoder
            Stack = stackEncoder
            LinkedList = linkedListEncoder
            Nullable = nbleEncoder
            Object = objectEncoder
        }

    let private defaultof (t: System.Type) =
        if t.IsValueType then
            System.Activator.CreateInstance(t)
        else null

    let Dummy =
        {
            Scalar = fun _ -> Some defaultof
            Array = fun dD i ta ->
                let x = box (System.Array.CreateInstance(ta.Type.GetElementType(), 0))
                fun _ -> x
            Tuple = fun dD i ta ->
                let xs = FST.GetTupleElements ta.Type |> Array.map (fun t -> dD (TAttrs.Get(i, t)) t)
                let x = FSV.MakeTuple(xs, ta.Type)
                fun _ -> x
            Union = fun dD i ta ->
                let uci = FST.GetUnionCases(ta.Type, flags).[0]
                let xs = uci.GetFields() |> Array.map (fun f -> dD (TAttrs.Get(i, f.PropertyType, f)) f.PropertyType)
                let x = FSV.MakeUnion(uci, xs, flags)
                fun _ -> x
            Record = fun dD i ta ->
                let xs = FST.GetRecordFields(ta.Type, flags) |> Array.map (fun f -> dD (TAttrs.Get(i, f.PropertyType, f)) f.PropertyType)
                let x = FSV.MakeRecord(ta.Type, xs, flags)
                fun _ -> x
            Enum = fun dD i ta ->
                let x = defaultof ta.Type
                fun _ -> x
            Map = fun dD i ta ->
                let x = genLetMethod(<@ Map.empty @>, ta.Type.GetGenericArguments()).Invoke0()
                fun _ -> x
            Set = fun dD i ta ->
                let x = genLetMethod(<@ Set.empty @>, ta.Type.GetGenericArguments()).Invoke0()
                fun _ -> x
            ResizeArray = fun dD i ta _ -> null
            Queue = fun dD i ta _ -> null
            Stack = fun dD i ta _ -> null
            LinkedList = fun dD i ta _ -> null
            Nullable = fun dD i ta _ -> null
            Object = fun _ _ _ _ -> null
        }

let getEncoding e wrap (fo: FormatSettings) (cache: ConcurrentDictionary<_,_>) =
    let rec get (ta: TAttrs) =
        let derive dD =
            try
                if ta.Type.IsArray then
                    if ta.Type.GetArrayRank() = 1 then
                        e.Array dD fo ta
                    else raise (NoEncodingException ta.Type)
                elif FST.IsTuple ta.Type then
                    e.Tuple dD fo ta
                elif FST.IsUnion (ta.Type, flags) then
                    e.Union dD fo ta
                elif FST.IsRecord (ta.Type, flags) then
                    e.Record dD fo ta
                elif ta.Type.IsEnum then
                    e.Enum dD fo ta
                else
                    let tn =
                        if ta.Type.IsGenericType 
                        then Some (ta.Type.GetGenericTypeDefinition().FullName)
                        else None
                    match tn with
                    | Some "Microsoft.FSharp.Collections.FSharpMap`2" -> e.Map dD fo ta
                    | Some "Microsoft.FSharp.Collections.FSharpSet`1" -> e.Set dD fo ta
                    | Some "System.Collections.Generic.List`1" -> e.ResizeArray dD fo ta
                    | Some "System.Collections.Generic.Queue`1" -> e.Queue dD fo ta
                    | Some "System.Collections.Generic.Stack`1" -> e.Stack dD fo ta
                    | Some "System.Collections.Generic.LinkedList`1" -> e.LinkedList dD fo ta
                    | Some "System.Nullable`1" -> e.Nullable dD fo ta
                    | _ -> 
                        e.Object dD fo ta
            with
            | NoEncodingException t ->
                reraise()
            | e ->
                fun _ -> raise (System.Exception("Error during RPC JSON conversion", e))
        if ta.Type = null then raise (NoEncodingException ta.Type) else
            match serializers.TryGetValue ta.Type with
            | true, x when Option.isSome (e.Scalar x) ->
                (e.Scalar x).Value
            | _ ->
                let newRef = ref Unchecked.defaultof<_>
                lock newRef <| fun () ->
                let r = cache.GetOrAdd(ta, newRef)
                if System.Object.ReferenceEquals(r, newRef) then
                    let d = derive (wrap get)
                    r := d
                    d
                else
                    let d = !r
                    // inside recursive types, delay the lookup of the function
                    if System.Object.ReferenceEquals(d, null) then
                        fun x -> 
                            let d = !r
                            // another thread might be running derive for the type
                            // we wait for the lock to release only in this case
                            if System.Object.ReferenceEquals(d, null) then
                                let d = lock r <| fun () -> !r
                                d x 
                            else d x    
                    else d
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
        let mt = AST.Reflection.ReadTypeDefinition t
        match i.Classes.TryFind mt with
        | Some (a, _, Some { HasWSPrototype = true }) ->
            function
            | EncodedObject fs -> EncodedInstance (a, fs)
            | EncodedArray xs -> EncodedArrayInstance (a, xs)
            | v -> v
        | _ -> id

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
            | EncodedArrayInstance (a, xs) -> Object [TYPE, encT a; VALUE, Array (List.map pk xs)]
        and pko xs =
            Object (xs |> List.map (fun (a, b) -> (a, pk b)))
        let data = pk encoded
        let rec encA acc x =
            match x with
            | [] -> failwith "types array must not be empty"
            | [x] -> Array (String x :: acc)
            | y :: x -> encA (String y :: acc) x
        let types = List.ofSeq (dict.Keys |> Seq.map (fun a -> a.Address.Value |> encA []))
        match types, data with
        | _::_, _
        | _, Object (((TYPES | VALUE), _) :: _) ->
            Object [
                TYPES, Array types
                DATA, data
            ]
        | [], data -> data

    let epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)

    let format info =
        {
            AddTag = addTag info
            GetEncodedFieldName = fun t ->
                let typ = AST.Reflection.ReadTypeDefinition t
                match info.Classes.TryGetValue typ with
                | true, (_, _, Some cls) -> 
                    let fields = cls.Fields
                    fun f ->
                        let getName v =
                            match v with
                            | (M.InstanceField n, _, _)
                            | (M.OptionalField n, _, _) -> Some n
                            | (M.IndexedField i, _, _) -> Some (string i)
                            | _ ->
                                failwithf "A static field not serializable: %s.%s" 
                                    t.FullName f                                          

                        let tryGet f =
                            match fields.TryGetValue f with
                            | true, v -> getName v
                            | _ ->
                                let fl = f.ToLower()
                                let caseInsensitive =
                                    fields |> Seq.choose (fun (KeyValue(k, v)) ->
                                        if k.ToLower() = fl then
                                            Some v
                                        else 
                                            None
                                    )
                                    |> Array.ofSeq
                                match caseInsensitive with
                                | [| v |] -> getName v
                                | _ -> None
                        match tryGet f with
                        | Some n -> n                                     
                        | _ ->
                            failwithf "Failed to look up translated field name for %s in type %s with fields: %s" 
                                f typ.Value.FullName (cls.Fields.Keys |> String.concat ", ")
                | true, (_, M.FSharpRecordInfo fs, _) -> 
                    fun f ->
                        fs |> List.pick (fun rf -> 
                            if rf.Name = f then 
                                Some rf.JSName
                            else None)
                | _ -> id
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

let culture = System.Globalization.CultureInfo.InvariantCulture
let dtstyle = System.Globalization.DateTimeStyles.AdjustToUniversal ||| System.Globalization.DateTimeStyles.AssumeUniversal

type FormatOptions =
    {
        EncodeDateTime : option<string> -> System.DateTime -> Encoded
        DecodeDateTime : option<string> -> Value -> option<System.DateTime>
    }

let defaultFormatOptions =
    {
        EncodeDateTime = fun fmt ->
            let fmt = defaultArg fmt "o"
            fun d -> EncodedString (d.ToString(fmt, culture))
        DecodeDateTime = fun fmt ->
            let fmt =
                match fmt with
                | Some x -> [|x|]
                // "o" only accepts 7 digits after the seconds,
                // but JavaScript's Date.toISOString() only outputs 3.
                // So we add a custom format to accept that too.
                | None -> [|"o"; @"yyyy-MM-dd\THH:mm:ss.fff\Z"|]
            function
            | String s ->
                match System.DateTime.TryParseExact(s, fmt, culture, dtstyle) with
                | true, x -> Some x
                | false, _ -> None
            | _ -> None
    }

type FormatOptions with
    static member Default = defaultFormatOptions

module PlainProviderInternals =

    let rec flatten encoded =
        let rec pk = function
            | EncodedNull -> Null
            | EncodedTrue -> True
            | EncodedFalse -> False
            | EncodedNumber x -> Number x
            | EncodedString x -> String x
            | EncodedArray xs
            | EncodedArrayInstance (_, xs) -> Array (List.map pk xs)
            | EncodedObject xs
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
                    if FST.IsRecord(t, flags) then
                        FST.GetRecordFields(t, flags)
                        |> Seq.map (fun f ->
                            f.Name, f :> System.Reflection.MemberInfo
                        )
                    else
                        getObjectFields t
                        |> Seq.map (fun f ->
                            let fn = f.Name
                            if fn.StartsWith("<") && fn.EndsWith(">k__BackingField") then
                                let pn = fn.Replace("<", "").Replace(">k__BackingField", "")
                                fn, t.GetProperty(pn) :> System.Reflection.MemberInfo
                            else
                                fn, f :> System.Reflection.MemberInfo
                        )
                if t.FullName = "WebSharper.CSharp.Interop.Tests.Person2" then
                    printfn "This is it"
                for fn, f in fields do
                    if not (d.ContainsKey fn) then
                        d.Add(fn, TAttrs.GetName f)
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
                defaultFormatOptions.EncodeDateTime ta.DateTimeFormat
            DecodeDateTime = fun ta ->
                defaultFormatOptions.DecodeDateTime ta.DateTimeFormat
            ConciseRepresentation = true
            Pack = flatten
        }

[<Sealed>]
type Provider(fo: FormatSettings) =
    [<System.NonSerialized>]
    let decoders = ConcurrentDictionary()
    [<System.NonSerialized>]
    let encoders = ConcurrentDictionary()

    [<System.NonSerialized>]
    let getDefaultBuilder =
        getEncoding Encodings.Dummy id fo (ConcurrentDictionary<_,_>())

    [<System.NonSerialized>]
    let getDecoder =
        getEncoding Encodings.Decode id fo decoders
        >> Decoder

    [<System.NonSerialized>]
    let getEncoder =
        getEncoding Encodings.Encode
            (fun dE ta ->
                if ta.Type.IsSealed || FST.IsUnion(ta.Type, flags) then dE ta else
                fun x -> dE (if x = null then ta else { ta with Type = x.GetType() }) x)
            fo
            encoders
        >> Encoder

    static member Create() =
        Provider PlainProviderInternals.format

    static member Create (options: FormatOptions) =
        Provider
            { PlainProviderInternals.format with
                EncodeDateTime = fun ta -> options.EncodeDateTime ta.DateTimeFormat
                DecodeDateTime = fun ta -> options.DecodeDateTime ta.DateTimeFormat
            }

    static member CreateTyped (info: M.Info) =
        Provider (TypedProviderInternals.format info)

    member this.GetDecoder(t: System.Type) : Decoder =
        getDecoder (TAttrs.Get(fo, t))

    member this.GetEncoder(t: System.Type) : Encoder =
        getEncoder (TAttrs.Get(fo, t))

    member this.GetDecoder<'T>() : Decoder<'T> =
        let d = this.GetDecoder typeof<'T>
        Decoder<'T>(fun x -> d.Decode x :?> 'T)

    member this.GetEncoder<'T>() : Encoder<'T> =
        let e = this.GetEncoder typeof<'T>
        Encoder<'T>(fun x -> e.Encode (box x))

    member this.BuildDefaultValue(t: System.Type) =
        getDefaultBuilder (TAttrs.Get(fo, t)) t

    member this.BuildDefaultValue<'T>() =
        this.BuildDefaultValue typeof<'T> :?> 'T

    member this.Pack x = fo.Pack x
