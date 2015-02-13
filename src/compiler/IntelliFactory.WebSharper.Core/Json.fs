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

module IntelliFactory.WebSharper.Core.Json

open System.Runtime.CompilerServices
module A = IntelliFactory.WebSharper.Core.Attributes
module P = IntelliFactory.JavaScript.Packager
module R = IntelliFactory.WebSharper.Core.Reflection
module Re = IntelliFactory.WebSharper.Core.Resources

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
        /// If true, represent fields whose type is a union marked
        /// UseNullAsTrueValue as absent field.
        /// Also always represent options as if they were marked [<OptionalField>].
        RepresentNullUnionsAsAbsentField : bool
        /// Pack an encoded value to JSON.
        Pack : Encoded -> Value
        EncodeDateTime : System.DateTime -> Encoded
        DecodeDateTime : Value -> option<System.DateTime>
        FlattenCollections : bool
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

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue

let tupleEncoder dE (i: FormatSettings) (t: System.Type) =
    let e = Array.map dE (FST.GetTupleElements t)
    let r = FSV.PreComputeTupleReader t
    fun (x: obj) ->
        match x with
        | null ->
            raise EncoderException
        | o when o.GetType() = t ->
            EncodedArray (Array.toList (Array.map2 (fun e x -> e x) e (r o)))
        | _ ->
            raise EncoderException

let tupleDecoder dD (i: FormatSettings) (t: System.Type) =
    let e = Array.map dD (FST.GetTupleElements t)
    let c = FSV.PreComputeTupleConstructor t
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

let arrayEncoder dE (i: FormatSettings) (t: System.Type) =
    let e = dE (t.GetElementType())
    fun (x: obj) ->
        match x with
        | null ->
            EncodedNull
        | o when o.GetType() = t ->
            let o = o :?> System.Array
            Seq.cast o
            |> Seq.map e
            |> Seq.toList
            |> EncodedArray
        | _ ->
            raise EncoderException

let arrayDecoder dD (i: FormatSettings) (t: System.Type) =
    let eT = t.GetElementType()
    let e = dD eT
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

let isOptionalField (i: FormatSettings) (mi: System.Reflection.MemberInfo) (mt: System.Type) =
    mt.IsGenericType &&
    mt.GetGenericTypeDefinition() = typedefof<option<_>> &&
    (i.RepresentNullUnionsAsAbsentField ||
        (mi.GetCustomAttributes(false)
        |> Array.exists (fun t -> t.GetType() = typeof<A.OptionalFieldAttribute>)))

let isNullableUnion (i: FormatSettings) (mt: System.Type) =
    i.RepresentNullUnionsAsAbsentField &&
    FST.IsUnion mt &&
    (mt.GetCustomAttributesData()
    |> Seq.exists (fun cad ->
        cad.Constructor.DeclaringType = typeof<CompilationRepresentationAttribute> &&
        let flags = cad.ConstructorArguments.[0].Value :?> CompilationRepresentationFlags
        flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0))

let encodeOptionalField dE (i: FormatSettings) (mi: System.Reflection.MemberInfo) (mt: System.Type) : obj -> option<Encoded> =
    if isOptionalField i mi mt then
        let vt = mt.GetGenericArguments().[0]
        let enc = dE vt
        let ucis = FST.GetUnionCases(mt, flags)
        let getTag = FSV.PreComputeUnionTagReader(mt, flags)
        let getSome = FSV.PreComputeUnionReader(ucis.[1], flags)
        fun x ->
            if getTag x = 0 then
                None
            else
                Some (enc (getSome x).[0])
    elif isNullableUnion i mt then
        let enc = dE mt
        let getTag = FSV.PreComputeUnionTagReader(mt, flags)
        fun x ->
            if getTag x = 0 then
                None
            else
                Some (enc x)
    else
        let enc = dE mt
        fun x -> Some (enc x)

let decodeOptionalField dD (i: FormatSettings) (mi: System.Reflection.MemberInfo) (mt: System.Type) : option<Value> -> obj =
    if isOptionalField i mi mt then
        let vt = mt.GetGenericArguments().[0]
        let dec = dD vt
        let ucis = FST.GetUnionCases(mt, flags)
        let none = FSV.PreComputeUnionConstructor(ucis.[0], flags) [||]
        let some = FSV.PreComputeUnionConstructor(ucis.[1], flags)
        fun x ->
            match x with
            | None -> none
            | Some v -> some [| dec v |]
    elif isNullableUnion i mt then
        let dec = dD mt
        let ucis = FST.GetUnionCases(mt, flags)
        let none = FSV.PreComputeUnionConstructor(ucis.[0], flags) [||]
        function
        | Some v -> dec v
        | None -> none
    else
        let dec = dD mt
        function
        | Some v -> dec v
        | None -> raise DecoderException

let opsModule =
    typeof<Set<_>>.Assembly
        .GetType("Microsoft.FSharp.Core.Operators")

let setModule =
    typeof<Set<_>>.Assembly
        .GetType("Microsoft.FSharp.Collections.SetModule")

let mapModule =
    typeof<Set<_>>.Assembly
        .GetType("Microsoft.FSharp.Collections.MapModule")

let seqModule =
    typeof<Set<_>>.Assembly
        .GetType("Microsoft.FSharp.Collections.SeqModule")

let listModule =
    typeof<Set<_>>.Assembly
        .GetType("Microsoft.FSharp.Collections.ListModule")

let thisModule =
    typeof<FormatSettings>.Assembly
        .GetType("IntelliFactory.WebSharper.Core.Json")

let makeEmptySet (t: System.Type) =
    setModule
        .GetMethod("Empty")
        .MakeGenericMethod(t.GetGenericArguments())
        .Invoke(null, [||])

let makeEmptyList (t: System.Type) =
    listModule
        .GetMethod("Empty")
        .MakeGenericMethod(t.GetGenericArguments())
        .Invoke(null, [||])

let unionEncoder dE (i: FormatSettings) (t: System.Type) =
    if i.FlattenCollections &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        let tg = t.GetGenericArguments()
        let tI = tg.[0]
        let dI = dE tI
        let toSeq =
            FastInvoke.Compile(
                listModule.GetMethod("ToSeq").MakeGenericMethod(tg)
            ).Invoke1
        fun (x: obj) ->
            EncodedArray [for e in Seq.cast<obj> (toSeq x :?> _) -> dI e]
    else
    let tR = FSV.PreComputeUnionTagReader(t, flags)
    let cs =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let r = FSV.PreComputeUnionReader(c, flags)
            let fs =
                c.GetFields()
                |> Array.mapi (fun k f ->
                    i.GetEncodedUnionFieldName f k, encodeOptionalField dE i f f.PropertyType)
            (r, fs))
    let encodeTag = i.EncodeUnionTag t
    fun (x: obj) ->
        match x with
        | null ->
            EncodedObject (Option.toList (encodeTag 0))
            |> i.AddTag t
        | o when t.IsAssignableFrom(o.GetType()) ->
            let tag = tR o
            let (r, fs) = cs.[tag]
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
            |> i.AddTag t
        | x ->
            raise EncoderException

let unionDecoder dD (i: FormatSettings) (t: System.Type) =
    if i.FlattenCollections &&
        t.IsGenericType &&
        t.GetGenericTypeDefinition() = typedefof<list<_>>
    then
        let tg = t.GetGenericArguments()
        let tI = tg.[0]
        let dI = dD tI
        let empty = makeEmptyList t
        let ofSeq =
            FastInvoke.Compile(
                listModule.GetMethod("OfSeq").MakeGenericMethod(tg)
            ).Invoke1
        let seqCast =
            FastInvoke.Compile(
                seqModule.GetMethod("Cast").MakeGenericMethod(tg)
            ).Invoke1
        function
        | Array vs -> ofSeq (seqCast (Seq.map dI vs))
        | _ -> raise DecoderException
    else
    let cs =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let mk = FSV.PreComputeUnionConstructor(c, flags)
            let fs =
                c.GetFields()
                |> Array.mapi (fun k f ->
                    i.GetEncodedUnionFieldName f k, decodeOptionalField dD i f f.PropertyType)
            (mk, fs))
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
        | _ ->
            raise DecoderException

let recordEncoder dE (i: FormatSettings) (t: System.Type) =
    let mt = R.TypeDefinition.FromType t
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f ->
            let r = FSV.PreComputeRecordFieldReader f
            (i.GetEncodedFieldName t f.Name, r, encodeOptionalField dE i f f.PropertyType))
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

let recordDecoder dD (i: FormatSettings) (t: System.Type) =
    let mt = R.TypeDefinition.FromType t
    let mk = FSV.PreComputeRecordConstructor(t, flags)
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f ->
            (i.GetEncodedFieldName t f.Name, decodeOptionalField dD i f f.PropertyType))
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

let objectEncoder dE (i: FormatSettings) (t: System.Type) =
    if t = typeof<System.DateTime> then
        fun (x: obj) ->
            match x with
            | :? System.DateTime as t -> i.EncodeDateTime t
            | _ -> raise EncoderException
    elif not t.IsSerializable then
        raise (NoEncodingException t)
    else
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let es = fs |> Array.map (fun f ->
        (i.GetEncodedFieldName f.DeclaringType f.Name,
         encodeOptionalField dE i f f.FieldType))
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

let objectDecoder dD (i: FormatSettings) (t: System.Type) =
    if t = typeof<System.DateTime> then
        fun (x: Value) ->
            match i.DecodeDateTime x with
            | Some d -> box d
            | None -> raise DecoderException
    elif not t.IsSerializable then
        raise (NoEncodingException t)
    else
    match t.GetConstructor [||] with
    | null -> raise (NoEncodingException t)
    | _ -> ()
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let ds = fs |> Array.map (fun f ->
        (i.GetEncodedFieldName f.DeclaringType f.Name,
         decodeOptionalField dD i f f.FieldType))
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

[<MethodImpl(MethodImplOptions.NoInlining)>]
let unmakeMap<'T> (dV: obj -> Encoded) =
    let dV x = dV x // Force the method to return an FSharpFunc instead of taking 2 args
    fun (x: obj) ->
        EncodedObject [
            for KeyValue(k, v) in unbox<Map<string, 'T>> x ->
                k, dV (box v)
        ]

let mapEncoder dE (i: FormatSettings) (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise EncoderException
    let dK = dE tg.[0]
    let dV = dE tg.[1]
    if i.FlattenCollections && tg.[0] = typeof<string> then
        let x = unmakeMap dV // Prevents unmakeMap from being dead-code-eliminated
        FastInvoke.Compile(
            thisModule.GetMethod("unmakeMap",
                flags ||| System.Reflection.BindingFlags.Static
            ).MakeGenericMethod(tg.[1])
        ).Invoke1(dV)
        :?> (obj -> Encoded)
    else
    let treeF = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let pair key value =
        EncodedObject [
            "Key", key
            "Value", value
        ]   
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

[<MethodImpl(MethodImplOptions.NoInlining)>]
let makeMap<'T> (dV: Value -> obj) =
    let dV x = dV x // Force the method to return an FSharpFunc instead of taking 2 args
    fun (vs: list<string * Value>) ->
        Map.ofList<string, 'T>(
            vs |> List.map (fun (k, v) -> k, unbox (dV v))
        )
        |> box

let mapDecoder dD (i: FormatSettings) (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise DecoderException
    let dK = dD tg.[0]
    let dV = dD tg.[1]
    if i.FlattenCollections && tg.[0] = typeof<string> then
        let x = makeMap dV // Prevents makeMap from being dead-code-eliminated
        let f =
            FastInvoke.Compile(
                thisModule.GetMethod("makeMap",
                    flags ||| System.Reflection.BindingFlags.Static
                ).MakeGenericMethod(tg.[1])
            ).Invoke1(dV)
            :?> (list<string * Value> -> obj)
        function
        | Object vs -> f vs
        | _ -> raise DecoderException
    else
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

let setEncoder dE (i: FormatSettings) (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    let dI = dE tg.[0]
    if i.FlattenCollections then
        let toSeq =
            FastInvoke.Compile(
                setModule.GetMethod("ToSeq").MakeGenericMethod(tg)
            ).Invoke1
        fun (x: obj) ->
            EncodedArray [for e in Seq.cast<obj> (toSeq x :?> _) -> dI e]
    else
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

let setDecoder dD (i: FormatSettings) (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    let ti = tg.[0]
    let dI = dD ti
    let empty = makeEmptySet t
    let ofSeq =
        FastInvoke.Compile(
            setModule.GetMethod("OfSeq").MakeGenericMethod(tg)
        ).Invoke1
    let seqCast =
        FastInvoke.Compile(
            seqModule.GetMethod("Cast").MakeGenericMethod(tg)
        ).Invoke1
    if i.FlattenCollections then
        function
        | Array vs -> ofSeq (seqCast (Seq.map dI vs))
        | _ -> raise DecoderException
    else fun (x: Value) ->
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
        match x with
        | Null -> empty
        | Object [ "tree", Object tr ] ->
            ofSeq (seqCast (walk tr))
        | _ -> raise DecoderException

let enumEncoder dE (i: FormatSettings) (t: System.Type) =
    let uT = System.Enum.GetUnderlyingType t
    let uE = dE uT
    fun (x: obj) ->
        uE (System.Convert.ChangeType(x, uT)) : Encoded

let enumDecoder dD (i: FormatSettings) (t: System.Type) =
    let uT = System.Enum.GetUnderlyingType t
    let uD = dD uT
    fun (x: Value) ->
        let y : obj = uD x
        System.Enum.ToObject(t, y)

let getEncoding scalar array tuple union record enu map set obj (fo: FormatSettings)
                (cache: Dictionary<_,_>) =
    let recurse t =
        lock cache <| fun () ->
            cache.[t] <-
                Choice1Of2 (fun v ->
                    let ct = lock cache <| fun () -> cache.[t]
                    match ct with
                    | Choice1Of2 f -> f v
                    | Choice2Of2 d -> raise (NoEncodingException d)
                )
    let rec get (t: System.Type) =
        let derive dD =
            try
                if t.IsArray && t.GetArrayRank() = 1 then
                    Choice1Of2 (array dD fo t)
                elif FST.IsTuple t then
                    Choice1Of2 (tuple dD fo t)
                elif FST.IsUnion (t, flags) then
                    recurse t
                    Choice1Of2 (union dD fo t)
                elif FST.IsRecord (t, flags) then
                    recurse t
                    Choice1Of2 (record dD fo t)
                elif t.IsEnum then
                    Choice1Of2 (enu dD fo t)
                else
                    let tn =
                        if t.IsGenericType 
                        then Some (t.GetGenericTypeDefinition().FullName)
                        else None
                    match tn with
                    | Some "Microsoft.FSharp.Collections.FSharpMap`2" -> Choice1Of2 (map dD fo t)
                    | Some "Microsoft.FSharp.Collections.FSharpSet`1" -> Choice1Of2 (set dD fo t)
                    | _ -> 
                        recurse t
                        Choice1Of2 (obj dD fo t)
            with NoEncodingException t ->
                Choice2Of2 t
        if t = null then Choice2Of2 t else
            match serializers.TryGetValue t with
            | true, x when Option.isSome (scalar x) ->
                Choice1Of2 (scalar x).Value
            | _ ->
                let d =
                    lock cache <| fun () ->
                        match cache.TryGetValue t with
                        | true, d -> Some d
                        | _ -> None
                match d with
                | Some d -> d
                | None ->
                    let dD t =
                        match get t with
                        | Choice1Of2 d -> d
                        | Choice2Of2 d -> raise (NoEncodingException d)
                    let d = derive dD
                    lock cache <| fun () ->
                        cache.[t] <- d
                    d
    get

module M = IntelliFactory.WebSharper.Core.Metadata

let defaultGetUnionTag t =
    let k = FST.GetUnionCases(t, flags).Length
    fun get ->
        match get "$" with
        | Some (Number n) ->
            match System.Int32.TryParse n with
            | true, tag when tag >= 0 && tag < k -> Some tag
            | _ -> None
        | _ -> None

let defaultEncodeUnionTag _ (tag: int) =
    Some ("$", EncodedNumber (string tag))

let getDiscriminatorName (t: System.Type) =
    t.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<A.NamedUnionCasesAttribute> then
            Some (cad.ConstructorArguments.[0].Value :?> string)
        else None)

let getNameAttr (m: System.Reflection.MemberInfo) =
    m.GetCustomAttributesData()
    |> Seq.tryPick (fun cad ->
        if cad.Constructor.DeclaringType = typeof<A.NameAttribute> then
            Some (cad.ConstructorArguments.[0].Value :?> string)
        else None)

type Reflection.UnionCaseInfo with
    member this.CustomizedName =
        let aT = typeof<CompiledNameAttribute>
        match this.GetCustomAttributes aT with
        | [| :? CompiledNameAttribute as attr |] -> attr.CompiledName
        | _ -> this.Name

module TypedProviderInternals =

    let addTag (i: M.Info) (t: System.Type) (v: Encoded) =
        let mt = R.TypeDefinition.FromType t
        match i.GetAddress mt with
        | None -> v
        | Some a ->
            match v with
            | EncodedObject fs -> EncodedInstance (a, fs)
            | _ -> v

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
            RepresentNullUnionsAsAbsentField = false
            EncodeDateTime = fun (d: System.DateTime) ->
                EncodedNumber (string (d.ToUniversalTime() - epoch).TotalMilliseconds)
            DecodeDateTime = function
                | Number x ->
                    match tryParseDouble x with
                    | true, x -> Some (epoch + System.TimeSpan.FromMilliseconds x)
                    | _ -> None
                | _ -> None
            FlattenCollections = false
            Pack = pack
        }

module PlainProviderInternals =

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
                        d.Add(f.Name, defaultArg (getNameAttr f) f.Name)
                fun n ->
                    match d.TryGetValue n with
                    | true, n -> n
                    | false, _ -> n
            GetUnionTag = fun t ->
                match getDiscriminatorName t with
                | None -> defaultGetUnionTag t
                | Some n ->
                    let names =
                        Map [
                            for c in FST.GetUnionCases(t, flags) ->
                                (String c.CustomizedName, c.Tag)
                        ]
                    fun get ->
                        get n |> Option.bind names.TryFind
            EncodeUnionTag = fun t ->
                match getDiscriminatorName t with
                | None -> defaultEncodeUnionTag t
                | Some n ->
                    let tags =
                        [|
                            for c in FST.GetUnionCases(t, flags) ->
                                EncodedString c.CustomizedName
                        |]
                    fun tag -> Some (n, tags.[tag])
            GetEncodedUnionFieldName = fun p -> let n = p.Name in fun _ -> n
            RepresentNullUnionsAsAbsentField = true
            EncodeDateTime = fun d ->
                EncodedString (d.ToString("o"))
            DecodeDateTime = function
                | String s ->
                    match System.DateTime.TryParse(s) with
                    | true, x -> Some x
                    | false, _ -> None
                | _ -> None
            FlattenCollections = true
            Pack = flatten
        }

[<Sealed>]
type Provider(fo: FormatSettings) =
    let decoders = Dictionary()
    let encoders = Dictionary()

    let defaultof =
        typeof<option<_>>.Assembly
            .GetType("Microsoft.FSharp.Core.Operators")
            .GetNestedType("Unchecked")
            .GetMethod("DefaultOf")
    let defaultof (t: System.Type) =
        defaultof.MakeGenericMethod(t).Invoke(null, [||])

    let getDefaultBuilder =
        getEncoding
            (fun _ -> Some defaultof)
            (fun dD i t ->
                let x = box ([||] : obj[])
                fun _ -> x)
            (fun dD i t ->
                let xs = FST.GetTupleElements t |> Array.map (fun t -> dD t t)
                let x = FSV.MakeTuple(xs, t)
                fun _ -> x)
            (fun dD i t ->
                let uci = FST.GetUnionCases(t).[0]
                let xs = uci.GetFields() |> Array.map (fun f -> dD f.PropertyType f.PropertyType)
                let x = FSV.MakeUnion(uci, xs)
                fun _ -> x)
            (fun dD i t ->
                let xs = FST.GetRecordFields t |> Array.map (fun f -> dD f.PropertyType f.PropertyType)
                let x = FSV.MakeRecord(t, xs)
                fun _ -> x)
            (fun dD i t ->
                let x = defaultof t
                fun _ -> x)
            (fun dD i t ->
                let x =
                    typedefof<Map<_,_>>.Assembly
                        .GetType("Microsoft.FSharp.Collections.MapModule")
                        .GetMethod("Empty")
                        .MakeGenericMethod(t.GetGenericArguments())
                        .Invoke(null, [||])
                fun _ -> x)
            (fun dD i t ->
                let x = makeEmptySet t
                fun _ -> x)
            (fun _ _ _ _ -> null)
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
        getDecoder t

    member this.GetEncoder(t: System.Type) : Encoder =
        getEncoder t

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
        getDefaultBuilder t t

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
