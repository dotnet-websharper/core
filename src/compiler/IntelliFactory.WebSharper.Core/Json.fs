// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Core.Json

module A = IntelliFactory.WebSharper.Core.Attributes
module M = IntelliFactory.WebSharper.Core.Metadata
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

type Serializer =
    {
        Decode : option<M.Info -> Value -> obj>
        Encode : option<M.Info -> obj -> Encoded>
    }

let simple enc dec =
    {
        Encode = Some (fun _ x -> enc x)
        Decode = Some (fun _ x -> dec x)
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
    let epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let encDateTime (d: System.DateTime) =
        EncodedNumber (string (d.ToUniversalTime() - epoch).TotalMilliseconds)
    let decDateTime = function
        | Number x ->
            match tryParseDouble x with
            | true, x -> epoch + System.TimeSpan.FromMilliseconds x
            | _ -> raise DecoderException
        | _ -> raise DecoderException
    add encDateTime decDateTime d
    let encTimeSpan (t: System.TimeSpan) =
        EncodedNumber (string t.TotalMilliseconds)
    let decTimeSpan = function
        | Number x ->
            match tryParseDouble x with
            | true, x -> System.TimeSpan.FromMilliseconds x
            | _ -> raise DecoderException
        | _ -> raise DecoderException
    add encTimeSpan decTimeSpan d
    d

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue

let tupleEncoder dE (t: System.Type) =
    let e = Array.map dE (FST.GetTupleElements t)
    let r = FSV.PreComputeTupleReader t
    fun (i: M.Info) (x: obj) ->
        match x with
        | null ->
            raise EncoderException
        | o when o.GetType() = t ->
            EncodedArray (Array.toList (Array.map2 (fun e x -> e i x) e (r o)))
        | _ ->
            raise EncoderException

let tupleDecoder dD (t: System.Type) =
    let e = Array.map dD (FST.GetTupleElements t)
    let c = FSV.PreComputeTupleConstructor t
    fun (i: M.Info) (x: Value) ->
        match x with
        | Array xs ->
            let xs = List.toArray xs
            if xs.Length = e.Length then
                c (Array.map2 (fun e x -> e i x) e xs)
            else
                raise DecoderException
        | _ ->
            raise DecoderException

let arrayEncoder dE (t: System.Type) =
    let e = dE (t.GetElementType())
    fun (i: M.Info) (x: obj) ->
        match x with
        | null ->
            EncodedNull
        | o when o.GetType() = t ->
            let o = o :?> System.Array
            Seq.cast o
            |> Seq.map (e i)
            |> Seq.toList
            |> EncodedArray
        | _ ->
            raise EncoderException

let arrayDecoder dD (t: System.Type) =
    let eT = t.GetElementType()
    let e = dD eT
    fun (i: M.Info) (x: Value) ->
        match x with
        | Null ->
            null
        | Array xs ->
            let data =
                Seq.map (e i) xs
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

let field (n: int) =
    System.String.Format("${0}", n)

let table ts =
    let d = Dictionary()
    for (k, v) in ts do
        d.[k] <- v
    fun x ->
        match d.TryGetValue x with
        | true, x -> Some x
        | _ -> None

let addTag (i: M.Info) (t: System.Type) (v: Encoded) =
    let mt = R.TypeDefinition.FromType t
    match i.GetAddress mt with
    | None -> v
    | Some a ->
        match v with
        | EncodedObject fs -> EncodedInstance (a, fs)
        | _ -> v

let unionEncoder dE (t: System.Type) =
    let tR = FSV.PreComputeUnionTagReader(t, flags)
    let cs =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let r = FSV.PreComputeUnionReader(c, flags)
            let fs =
                c.GetFields()
                |> Array.map (fun f -> dE f.PropertyType)
            (r, fs))
    fun (i: M.Info) (x: obj) ->
        match x with
        | null ->
            EncodedObject [("$", EncodedNumber "0")]
            |> addTag i t
        | o when t.IsAssignableFrom(o.GetType()) ->
            let tag = tR o
            let (r, fs) = cs.[tag]
            let data =
                Array.mapi2 (fun k e x -> (field k, e i x)) fs (r o)
                |> Array.toList
            EncodedObject (("$", EncodedNumber (string tag)) :: data)
            |> addTag i t
        | x ->
            raise EncoderException

let unionDecoder dD (t: System.Type) =
    let cs =
        FST.GetUnionCases(t, flags)
        |> Array.map (fun c ->
            let mk = FSV.PreComputeUnionConstructor(c, flags)
            let fs =
                c.GetFields()
                |> Array.map (fun f -> dD f.PropertyType)
            (mk, fs))
    let k = cs.Length
    fun (i: M.Info) (x: Value) ->
        match x with
        | Object fields ->
            let get = table fields
            let tag =
                match get "$" with
                | Some (Number n) ->
                    match System.Int32.TryParse n with
                    | true, tag when tag >= 0 && tag < k -> tag
                    | _ -> raise DecoderException
                | _ ->
                    raise DecoderException
            let (mk, fs) = cs.[tag]
            fs
            |> Array.mapi (fun k f ->
                match get (field k) with
                | Some x -> f i x
                | None -> raise DecoderException)
            |> mk
        | _ ->
            raise DecoderException

let recordEncoder dE (t: System.Type) =
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f ->
            let r = FSV.PreComputeRecordFieldReader f
            (f.Name, r, dE f.PropertyType))
    let mt = R.TypeDefinition.FromType t
    fun (i: M.Info) (x: obj) ->
        match x with
        | null ->
            raise EncoderException
        | o when o.GetType() = t ->
            fs
            |> Array.map (fun (n, r, enc) ->
                (i.GetFieldName mt n, enc i (r o)))
            |> Array.toList
            |> EncodedObject
            |> addTag i t
        | _ ->
            raise EncoderException

let recordDecoder dD (t: System.Type) =
    let mt = R.TypeDefinition.FromType t
    let mk = FSV.PreComputeRecordConstructor(t, flags)
    let fs =
        FST.GetRecordFields(t, flags)
        |> Array.map (fun f -> (f.Name, dD f.PropertyType))
    fun (i: M.Info) (x: Value) ->
        match x with
        | Object fields ->
            let get = table fields
            fs
            |> Array.map (fun (n, dec) ->
                let n = i.GetFieldName mt n
                match get n with
                | None -> raise DecoderException
                | Some x -> dec i x)
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
    |> Seq.distinctBy (fun x -> x.Name)
    |> Seq.toArray

let objectEncoder dE (t: System.Type) =
    if not t.IsSerializable then
        raise (NoEncodingException t)
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let es = fs |> Array.map (fun f -> (f.Name, dE f.FieldType))
    fun (i: M.Info) (x: obj) ->
        match x with
        | null ->
            EncodedNull
        | o when t.IsAssignableFrom(o.GetType()) ->
            let data = FS.GetObjectData(o, ms)
            (data, es)
            ||> Array.map2 (fun x (name, enc) ->
                (name, enc i x))
            |> Array.toList
            |> EncodedObject
            |> addTag i t
        | _ ->
            raise EncoderException

let objectDecoder dD (t: System.Type) =
    if not t.IsSerializable then
        raise (NoEncodingException t)
    match t.GetConstructor [||] with
    | null -> raise (NoEncodingException t)
    | _ -> ()
    let fs = getObjectFields t
    let ms = fs |> Array.map (fun x -> x :> System.Reflection.MemberInfo)
    let ds = fs |> Array.map (fun f -> (f.Name, dD f.FieldType))
    fun (i: M.Info) (x: Value) ->
        match x with
        | Null -> null
        | Object fields ->
            let get = table fields
            let obj = System.Activator.CreateInstance t
            let data =
                ds
                |> Seq.map (fun (n, dec) ->
                   match get n with
                    | Some x -> dec i x
                    | None -> raise DecoderException)
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

let mapEncoder dE (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise EncoderException
    let dK = dE tg.[0]
    let dV = dE tg.[1]
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
    fun (i: M.Info) (x: obj) ->
        let rec encNode v = 
            match v with
            | null -> EncodedNull, 0
            | _ ->
            match tR v with
            | 0 -> EncodedNull, 0
            | 1 ->
                let u = uR.[1] v
                btree (pair (dK i u.[0]) (dV i u.[1])) EncodedNull EncodedNull "1" "1", 1
            | 2 ->
                let u = uR.[2] v
                let l, lc = encNode u.[2]
                let r, rc = encNode u.[3]
                let c = 1 + lc + rc
                btree (pair (dK i u.[0]) (dV i u.[1])) l r (string u.[4]) (string c), c 
            | _ -> raise EncoderException     
        let tr = fst (encNode (treeF.GetValue x))
        EncodedObject [ "tree", tr ] |> addTag i t

let mapDecoder dD (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 2 then raise DecoderException
    let dK = dD tg.[0]
    let dV = dD tg.[1]
    let tt = typedefof<System.Tuple<_,_>>.MakeGenericType(tg.[0], tg.[1])
    let cT = FSV.PreComputeTupleConstructor(tt)
    fun (i: M.Info) (x: Value) ->
        let rec walk fields =
            seq {
                for f in fields do
                    match f with
                    | "Node", Object [ "Key", k; "Value", v ] -> 
                        yield cT [| dK i k; dV i v |]
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

let setEncoder dE (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    let dI = dE tg.[0]
    let treeF = t.GetFields(fieldFlags) |> Array.find (fun f -> f.Name.StartsWith "tree")
    let tR = FSV.PreComputeUnionTagReader(treeF.FieldType, flags)
    let uR =
        FST.GetUnionCases(treeF.FieldType, flags)
        |> Array.map (fun c -> FSV.PreComputeUnionReader(c, flags))
    fun (i: M.Info) (x: obj) ->
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
                btree (dI i u.[0]) l r (string u.[3]) (string c), c
            | 2 ->
                let u = uR.[2] v
                btree (dI i u.[0]) EncodedNull EncodedNull "1" "1", 1
            | _ -> raise EncoderException     
        let tr = fst (encNode (treeF.GetValue x))
        EncodedObject [ "tree", tr ] |> addTag i t

let setDecoder dD (t: System.Type) =
    let tg = t.GetGenericArguments()
    if tg.Length <> 1 then raise EncoderException
    let ti = tg.[0]
    let dI = dD ti
    fun (i: M.Info) (x: Value) ->
        let rec walk fields =
            seq {
                for f in fields do
                    match f with
                    | "Node", n -> 
                        yield dI i n
                    | ("Left" | "Right"), Object st -> 
                        yield! walk st
                    | _ -> ()
            }
        match x with
        | Null -> System.Activator.CreateInstance(t)
        | Object [ "tree", Object tr ] ->
            let els = walk tr |> Array.ofSeq
            let tEls = System.Array.CreateInstance(ti, els.Length)
            System.Array.Copy(els, tEls, els.Length)
            System.Activator.CreateInstance(t, tEls)
        | _ -> raise DecoderException

let enumEncoder dE (t: System.Type) =
    let uT = System.Enum.GetUnderlyingType t
    let uE = dE uT
    fun (i: M.Info) (x: obj) ->
        uE i (System.Convert.ChangeType(x, uT)) : Encoded

let enumDecoder dD (t: System.Type) =
    let uT = System.Enum.GetUnderlyingType t
    let uD = dD uT
    fun (i: M.Info) (x: Value) ->
        let y : obj = uD i x
        System.Enum.ToObject(t, y)

let getEncoding scalar array tuple union record enu map set obj
                (cache: Dictionary<_,_>) =
    let recurse t =
        lock cache <| fun () ->
            cache.[t] <-
                Choice1Of2 (fun i v ->
                    let ct = lock cache <| fun () -> cache.[t]
                    match ct with
                    | Choice1Of2 f -> f i v
                    | Choice2Of2 d -> raise (NoEncodingException d)
                )
    let rec get (t: System.Type) =
        let derive dD =
            try
                if t.IsArray && t.GetArrayRank() = 1 then
                    Choice1Of2 (array dD t)
                elif FST.IsTuple t then
                    Choice1Of2 (tuple dD t)
                elif FST.IsUnion (t, flags) then
                    recurse t
                    Choice1Of2 (union dD t)
                elif FST.IsRecord (t, flags) then
                    recurse t
                    Choice1Of2 (record dD t)
                elif t.IsEnum then
                    Choice1Of2 (enu dD t)
                else
                    let tn =
                        if t.IsGenericType 
                        then Some (t.GetGenericTypeDefinition().FullName)
                        else None
                    match tn with
                    | Some "Microsoft.FSharp.Collections.FSharpMap`2" -> Choice1Of2 (map dD t)
                    | Some "Microsoft.FSharp.Collections.FSharpSet`1" -> Choice1Of2 (set dD t)
                    | _ -> 
                        recurse t
                        Choice1Of2 (obj dD t)
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
        Object (List.map (fun (a, b) -> (a, pk b)) xs)
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
        Object (List.map (fun (a, b) -> (a, pk b)) xs)
    pk encoded

[<Sealed>]
type Provider(info: M.Info, pack: Encoded -> Value) =
    let decoders = Dictionary()
    let encoders = Dictionary()

    static member Create() =
        Provider(M.Info.Create [], flatten)

    static member CreateTyped info =
        Provider(info, pack)

    member this.GetDecoder(t: System.Type) : Decoder =
        let c =
            getEncoding (fun {Decode=x} -> x)
                arrayDecoder
                tupleDecoder
                unionDecoder
                recordDecoder
                enumDecoder
                mapDecoder
                setDecoder
                objectDecoder
                decoders
                t
        match c with
        | Choice1Of2 x -> Decoder (x info)
        | Choice2Of2 x -> raise (NoDecoderException x)

    member this.GetEncoder(t: System.Type) : Encoder =
        let c =
            getEncoding (fun {Encode=x} -> x)
                arrayEncoder
                tupleEncoder
                unionEncoder
                recordEncoder
                enumEncoder
                mapEncoder
                setEncoder
                objectEncoder
                encoders
                t
        match c with
        | Choice1Of2 x -> Encoder (x info)
        | Choice2Of2 x -> raise (NoEncoderException x)

    member this.GetDecoder<'T>() : Decoder<'T> =
        let d = this.GetDecoder typeof<'T>
        Decoder<'T>(fun x ->
            match d.Decode x with
            | (:? 'T as x) -> x
            | _ -> raise DecoderException)

    member this.GetEncoder<'T>() : Encoder<'T> =
        let e = this.GetEncoder typeof<'T>
        Encoder<'T>(fun x -> e.Encode (box x))

    member this.Pack x = pack x

let Parse s =
    use r = new System.IO.StringReader(s)
    Read r

let Stringify v =
    use w = new System.IO.StringWriter()
    Write w v
    w.ToString()
