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

module WebSharper.Core.Binary

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type IDictionary<'T1,'T2> = System.Collections.Generic.IDictionary<'T1,'T2>
type ISet<'T> = System.Collections.Generic.ISet<'T>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>
type KeyValuePair<'T1,'T2> = System.Collections.Generic.KeyValuePair<'T1,'T2>

exception NoEncodingException of System.Type with
    override this.ToString() =
        System.String.Format("Failed to encode {0} to binary.",
            this.Data0)

[<AbstractClass>]
type Encoder() =
    abstract Decode : System.IO.BinaryReader -> obj
    abstract Encode : System.IO.BinaryWriter * obj -> unit
    abstract Type : System.Type

[<AbstractClass>]
type Encoder<'T>() =
    inherit Encoder()
    abstract Read : System.IO.BinaryReader -> 'T
    abstract Write : System.IO.BinaryWriter * 'T -> unit
    override this.Decode r = this.Read r :> _
    override this.Encode(w, x: obj) = this.Write(w, x :?> _)
    override this.Type = typeof<'T>

/// Implements user-defined binary encoding detection.  The user is expected
/// to provide an instance Encode and a static Decode methods, for example:
///
///     type T(x: int) =
///         member this.Encode(d: System.IO.BinaryWriter) = w.Write x
///         static member Decode(r: System.IO.BinaryReader) = T(r.ReadInt32())
///
module private Custom =

    type private DecodeDelegate<'T> =
        delegate of System.IO.BinaryReader -> 'T

    type private EncodeDelegate<'T> =
        delegate of 'T * System.IO.BinaryWriter -> unit

    [<AbstractClass>]
    type private DecodeAdapter() =
        abstract member Adapt : obj -> (System.IO.BinaryReader -> obj)

    [<AbstractClass>]
    type private EncodeAdapter() =
        abstract member Adapt : obj -> (System.IO.BinaryWriter -> obj -> unit)

    [<Sealed>]
    type private DecodeAdapter<'T>() =
        inherit DecodeAdapter()

        member this.Adapt(del: DecodeDelegate<'T>) =
            fun r -> del.Invoke r

        override this.Adapt(del: obj) =
            let f = this.Adapt(unbox del)
            fun r -> box (f r)

    [<Sealed>]
    type private EncodeAdapter<'T>() =
        inherit EncodeAdapter()

        member this.Adapt(del: EncodeDelegate<'T>) =
            fun w (x: obj) -> del.Invoke(unbox x, w)

        override this.Adapt(del: obj) =
            this.Adapt(unbox del)

    type private BF = System.Reflection.BindingFlags

    let private allInstance = BF.Public ||| BF.NonPublic ||| BF.Instance

    let private allStatic = BF.Public ||| BF.NonPublic ||| BF.Static

    let private guard f =
        try f () with
        | :? System.ArgumentException
        | :? System.Reflection.AmbiguousMatchException -> None

    [<Sealed>]
    type private CustomEncoder(t, encode, decode) =
        inherit Encoder()
        override this.Decode r = decode r
        override this.Encode(w, x) = encode w x
        override this.Type = t

    let GetEncoder (t: System.Type) : option<Encoder> =
        guard <| fun () ->
            let m1 = t.GetMethod("Encode", allInstance)
            if m1 = null then None else
                let k = typedefof<EncodeDelegate<_>>.MakeGenericType(t)
                let d = System.Delegate.CreateDelegate(k, m1)
                let a =
                    typedefof<EncodeAdapter<_>>.MakeGenericType(t)
                    |> System.Activator.CreateInstance :?> EncodeAdapter
                let encode = a.Adapt d
                let m2 = t.GetMethod("Decode", allStatic)
                if m2 = null then None else
                    let k = typedefof<DecodeDelegate<_>>.MakeGenericType(t)
                    let d = System.Delegate.CreateDelegate(k, m2)
                    let a =
                        typedefof<DecodeAdapter<_>>.MakeGenericType(t)
                        |> System.Activator.CreateInstance :?> DecodeAdapter
                    let decode = a.Adapt d
                    Some (CustomEncoder(t, encode, decode) :> _)

[<Sealed>]
type EncodingException =
    inherit exn

    new (message: string) =
        { inherit exn(message) }

    new (message: string, inner: exn) =
        { inherit exn(message, inner) }

[<Sealed>]
type BooleanEncoder() =
    inherit Encoder<bool>()
    override this.Read r = r.ReadBoolean()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type ByteEncoder() =
    inherit Encoder<byte>()
    override this.Read r = r.ReadByte()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type CharEncoder() =
    inherit Encoder<char>()
    override this.Read r = r.ReadChar()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type DecimalEncoder() =
    inherit Encoder<decimal>()
    override this.Read r = r.ReadDecimal()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type DoubleEncoder() =
    inherit Encoder<double>()
    override this.Read r = r.ReadDouble()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type Int16Encoder() =
    inherit Encoder<int16>()
    override this.Read r = r.ReadInt16()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type Int32Encoder() =
    inherit Encoder<int32>()
    override this.Read r = r.ReadInt32()
    override this.Write(w, x) = w.Write x

type Int64Encoder() =
    inherit Encoder<int64>()
    override this.Read r = r.ReadInt64()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type SByteEncoder() =
    inherit Encoder<sbyte>()
    override this.Read r = r.ReadSByte()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type SingleEncoder() =
    inherit Encoder<single>()
    override this.Read r = r.ReadSingle()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type StringEncoder() =
    inherit Encoder<string>()
    override this.Read r = r.ReadString()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type UInt16Encoder() =
    inherit Encoder<uint16>()
    override this.Read r = r.ReadUInt16()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type UInt32Encoder() =
    inherit Encoder<uint32>()
    override this.Read r = r.ReadUInt32()
    override this.Write(w, x) = w.Write x

[<Sealed>]
type UInt64Encoder() =
    inherit Encoder<uint64>()
    override this.Read r = r.ReadUInt64()
    override this.Write(w, x) = w.Write x

let basicEncoders =
    let d = Dictionary<System.Type,Encoder>(14)
    d.[typeof<bool>] <- BooleanEncoder()
    d.[typeof<byte>] <- ByteEncoder()
    d.[typeof<char>] <- CharEncoder()
    d.[typeof<double>] <- DoubleEncoder()
    d.[typeof<decimal>] <- DecimalEncoder()
    d.[typeof<int16>] <- Int16Encoder()
    d.[typeof<int32>] <- Int32Encoder()
    d.[typeof<int64>] <- Int64Encoder()
    d.[typeof<sbyte>] <- SByteEncoder()
    d.[typeof<single>] <- SingleEncoder()
    d.[typeof<uint16>] <- UInt16Encoder()
    d.[typeof<uint32>] <- UInt32Encoder()
    d.[typeof<uint64>] <- UInt64Encoder()
    d.[typeof<string>] <- StringEncoder()
    d

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

[<Sealed>]
type ArrayEncoder(t: System.Type, re: System.Type -> Encoder) =
    inherit Encoder()
    let eT = t.GetElementType()
    let enc : Encoder = re eT

    override this.Decode r =
        let k = r.ReadInt32()
        let res = System.Array.CreateInstance(eT, k)
        for i in 0 .. k - 1 do
            res.SetValue(enc.Decode r, i)
        res :> obj

    override this.Encode(w, o) =
        let o = o :?> System.Array
        w.Write o.Length
        for x in o do
            enc.Encode(w, x)

    override this.Type = t

[<Sealed>]
type RecordEncoder(t: System.Type, re: System.Type -> Encoder) =
    inherit Encoder()
    let mk = FSV.PreComputeRecordConstructor(t, flags)
    let fields = FST.GetRecordFields(t, flags)
    let encoders =
        fields
        |> Array.map (fun f -> re f.PropertyType)
    let reader = FSV.PreComputeRecordReader(t, flags)

    override this.Decode r =
        encoders
        |> Array.map (fun enc -> enc.Decode r)
        |> mk

    override this.Encode(w, x) =
        (encoders, reader x)
        ||> Array.iter2 (fun enc x -> enc.Encode(w, x))

    override this.Type = t

[<Sealed>]
type TupleEncoder(t: System.Type, re: System.Type -> Encoder) =
    inherit Encoder()
    let elements = FST.GetTupleElements t
    let read = FSV.PreComputeTupleReader t
    let mk = FSV.PreComputeTupleConstructor t
    let fields = Array.map re elements

    override this.Decode r =
        fields
        |> Array.map (fun e -> e.Decode r)
        |> mk

    override this.Encode(w, x) =
        (fields, read x)
        ||> Array.iter2 (fun enc x -> enc.Encode(w, x))

    override this.Type = t

[<Sealed>]
type UnionEncoder(t: System.Type, re: System.Type -> Encoder) =
    inherit Encoder()
    let tagReader = FSV.PreComputeUnionTagReader(t, flags)
    let cases = FST.GetUnionCases(t, flags)
    let fields =
        cases
        |> Array.map (fun c ->
            c.GetFields()
            |> Array.map (fun f -> re f.PropertyType))
    let readers =
        cases
        |> Array.map (fun c -> FSV.PreComputeUnionReader(c, flags))
    let constructors =
        cases
        |> Array.map (fun c -> FSV.PreComputeUnionConstructor(c, flags))

    override this.Decode r =
        let tag = int (r.ReadByte())
        fields.[tag]
        |> Array.map (fun f -> f.Decode r)
        |> constructors.[tag]

    override this.Encode(w, x) =
        let tag = tagReader x
        w.Write (byte tag)
        (fields.[tag], readers.[tag] x)
        ||> Array.iter2 (fun enc x -> enc.Encode(w, x))

    override this.Type = t

[<AbstractClass>]
type HashedProcessor() =
    abstract member FromObject : obj -> obj
    abstract member RunOnValue : (obj -> unit) -> obj -> unit

[<AbstractClass>]
type DictionaryProcessor() =
    abstract member FromArray : array<KeyValuePair<obj,obj>> -> obj
    abstract member GetLength : obj -> int
    abstract member Iterate : (obj -> obj -> unit) -> obj -> unit

[<AbstractClass>]
type SequenceProcessor() =
    abstract member FromArray : obj [] -> obj
    abstract member GetLength : obj -> int
    abstract member Iterate : (obj -> unit) -> obj -> unit

[<Sealed>]
type HashedProcessor<'T when 'T : equality and 'T : comparison>() =
    inherit HashedProcessor()
    override this.FromObject x =  
        Hashed(x :?> 'T) |> box
    override this.RunOnValue f x =
        (x :?> Hashed<'T>).Value |> f

[<Sealed>]
type DictionaryProcessor<'T1,'T2 when 'T1 : comparison>() =
    inherit DictionaryProcessor()

    override this.FromArray x =
        let d = Dictionary(x.Length)
        for KeyValue (k, v) in x do
            d.[k :?> 'T1] <- v :?> 'T2
        box d

    override this.GetLength(dict: obj) =
        (dict :?> IDictionary<'T1,'T2>).Count

    override this.Iterate f x =
        for KeyValue (k, v) in (x :?> IDictionary<'T1,'T2>) do
            f (box k) (box v)

[<Sealed>]
type HashSetProcessor<'T when 'T : comparison>() =
    inherit SequenceProcessor()

    override this.FromArray(s: array<obj>) =
        HashSet<'T>(Array.map unbox s) |> box

    override this.GetLength(x: obj) =
        (x :?> ISet<'T>).Count

    override this.Iterate f x =
        for e in (x :?> ISet<'T>) do
            f (box x)

[<Sealed>]
type ListProcessor<'T>() =
    inherit SequenceProcessor()

    override this.FromArray(x: array<obj>) =
        List.ofArray<'T> (Array.map unbox x) |> box

    override this.GetLength(x: obj) =
        (x :?> list<'T>).Length

    override this.Iterate f x =
        for e in (x :?> list<'T>) do
            f (box e)

[<Sealed>]
type MapProcessor<'T1,'T2 when 'T1 : comparison>() =
    inherit DictionaryProcessor()

    override this.FromArray x =
        x
        |> Array.map (fun (KeyValue (k, v)) -> (k :?> 'T1, v :?> 'T2))
        |> Map.ofArray
        |> box

    override this.GetLength(x: obj) =
        (x :?> Map<'T1,'T2>).Count

    override this.Iterate f x =
        for KeyValue (k, v) in (x :?> Map<'T1,'T2>) do
            f (box k) (box v)

[<Sealed>]
type SetProcessor<'T when 'T : comparison>() =
    inherit SequenceProcessor()

    override this.FromArray(x: obj []) =
        x
        |> Array.map (fun x -> x :?> 'T)
        |> Set.ofArray
        |> box

    override this.GetLength(x: obj) =
        (x :?> Set<'T>).Count

    override this.Iterate f x =
        for e in (x :?> Set<'T>) do
            f (box e)

[<Sealed>]
type HashedEncoder<'T when 'T :> HashedProcessor>
    (t: System.Type, derive: System.Type -> Encoder) =
    inherit Encoder()
    let args = t.GetGenericArguments()
    let valueType = args.[0]
    let value = derive valueType
    
    let hP : HashedProcessor =
        typedefof<'T>.MakeGenericType(valueType)       
        |> System.Activator.CreateInstance
        |> unbox

    override this.Decode r =
        hP.FromObject (value.Decode r)     

    override this.Encode (w, x) =
        hP.RunOnValue (fun v -> value.Encode(w, v)) x

    override this.Type = t


[<Sealed>]
type DictionaryEncoder<'T when 'T :> DictionaryProcessor>
    (t: System.Type, derive: System.Type -> Encoder) =
    inherit Encoder()
    let args = t.GetGenericArguments()
    let keyType = args.[0]
    let valueType = args.[1]
    let key = derive keyType
    let value = derive valueType

    let dP : DictionaryProcessor =
        typedefof<'T>.MakeGenericType(keyType, valueType)
        |> System.Activator.CreateInstance
        |> unbox

    override this.Decode r =
        let k = r.ReadInt32()
        Array.init k (fun _ ->
            let key = key.Decode r
            let value = value.Decode r
            KeyValuePair(key, value))
        |> dP.FromArray

    override this.Encode(w, x) =
        w.Write (dP.GetLength x)
        x
        |> dP.Iterate (fun k v ->
            key.Encode(w, k)
            value.Encode(w, v))

    override this.Type = t

[<Sealed>]
type SequenceEncoder<'T when 'T :> SequenceProcessor>
    (t: System.Type, derive: System.Type -> Encoder) =
    inherit Encoder()
    let args = t.GetGenericArguments()
    let elType = args.[0]
    let el = derive elType

    let sP : SequenceProcessor =
        typedefof<'T>.MakeGenericType(elType)
        |> System.Activator.CreateInstance
        |> unbox

    override this.Decode r =
        let k = r.ReadInt32()
        Array.init k (fun _ -> el.Decode r)
        |> sP.FromArray

    override this.Encode(w, x) =
        w.Write (sP.GetLength x)
        x
        |> sP.Iterate (fun e ->
            el.Encode(w, e))

    override this.Type = t

let getSpecializedEncoder (re: System.Type -> Encoder) (t: System.Type) =
    let d = t.GetGenericTypeDefinition()
    if d = typedefof<Hashed<_>> then
        Some (HashedEncoder<HashedProcessor<_>>(t, re) :> Encoder)
    elif d.IsAssignableFrom(typedefof<IDictionary<_,_>>) then
        Some (DictionaryEncoder<DictionaryProcessor<_,_>>(t, re) :> Encoder)
    elif d.IsAssignableFrom(typedefof<ISet<_>>) then
        Some (SequenceEncoder<HashSetProcessor<_>>(t, re) :> _)
    elif d = typedefof<List<_>> then
        Some (SequenceEncoder<ListProcessor<_>>(t, re) :> _)
    elif d = typedefof<Map<_,_>> then
        Some (DictionaryEncoder<MapProcessor<_,_>>(t, re) :> _)
    elif d = typedefof<Set<_>> then
        Some (SequenceEncoder<SetProcessor<_>>(t, re) :> _)
    else None

let getAlgebraicEncoder (re: System.Type -> Encoder) (t: System.Type) =
    if t.IsArray && t.GetArrayRank() = 1 then ArrayEncoder(t, re) :> Encoder
    elif FST.IsTuple t then TupleEncoder(t, re) :> _
    elif FST.IsUnion(t, flags) then UnionEncoder(t, re) :> _
    elif FST.IsRecord (t, flags) then RecordEncoder(t, re) :> _
    else raise (NoEncodingException t)

let getEncoder (re: System.Type -> Encoder) (t: System.Type) =
    if t.IsPrimitive then
        if t = typeof<nativeint> || t = typeof<unativeint> then
            raise (NoEncodingException t)
        basicEncoders.[t]
    elif t = typeof<string> || t = typeof<decimal> then
        basicEncoders.[t]
    elif t.IsEnum then 
        basicEncoders.[System.Enum.GetUnderlyingType(t)]
    else
        match Custom.GetEncoder t with
        | Some e -> e
        | None ->
            if t.IsGenericType then
                match getSpecializedEncoder re t with
                | Some e -> e
                | None -> getAlgebraicEncoder re t
            else getAlgebraicEncoder re t

[<Sealed>]
type RecursiveEncoder(cache: Dictionary<_,Encoder>, t) =
    inherit Encoder()
    let self =
        lazy
        match cache.TryGetValue t with
        | true, e -> e
        | _ -> raise (NoEncodingException t)
    override this.Decode r = self.Value.Decode r
    override this.Encode(w, x) = self.Value.Encode(w, x)
    override this.Type = t

let rec getCachedEncoder (cache: Dictionary<_,_>) t =
    match cache.TryGetValue t with
    | true, e -> e
    | _ ->
        cache.[t] <- RecursiveEncoder(cache, t) :> _
        let e = getEncoder (getCachedEncoder cache) t
        cache.[t] <- e
        e

[<Sealed>]
type InterningReader(input: System.IO.Stream, reader: System.IO.BinaryReader) =
    inherit System.IO.BinaryReader(input)

    let cache =
        let d = Dictionary()
        for i in 0 .. reader.ReadInt32() - 1 do
            let s = reader.ReadString()
            d.[uint16 i] <- s
        d

    override this.ReadString() =
        cache.[this.ReadUInt16()]

[<Sealed>]
type InterningWriter(memory: System.IO.MemoryStream) =
    inherit System.IO.BinaryWriter(memory)
    let cache = Dictionary<string,uint16>()

    override this.Write(data: string) =
        match cache.TryGetValue data with
        | true, i -> this.Write i
        | _ ->
            let i = uint16 cache.Count
            cache.[data] <- i
            this.Write i

    member this.WriteTo(output: System.IO.Stream)
        (writer: System.IO.BinaryWriter) =
        writer.Write cache.Count
        for key in cache.Keys do
            writer.Write key
        memory.WriteTo output

[<Sealed>]
type Encoding(enc: Encoder) =

    member this.Decode(stream: System.IO.Stream) : obj =
        let mode = System.IO.Compression.CompressionMode.Decompress
        use stream = new System.IO.Compression.GZipStream(stream, mode)
        use reader = new System.IO.BinaryReader(stream)
#if DEBUG
        let res =
#else
        try
#endif
            let aqn = enc.Type.AssemblyQualifiedName
            let s = reader.ReadString()
            if s <> aqn then
                let msg =
                    System.String.Format("Unexpected: {0}. Expecting: {1}",
                        s, aqn)
                raise (EncodingException msg)
            use r = new InterningReader(stream, reader)
            enc.Decode r
#if DEBUG
        res
#else
        with e ->
            let msg = System.String.Format("Failed to decode type: {0}", enc.Type)
            raise (EncodingException(msg, e))
#endif

    member this.Encode(stream: System.IO.Stream)(value: obj) =
        let mode = System.IO.Compression.CompressionMode.Compress
        use stream = new System.IO.Compression.GZipStream(stream, mode)
        use memory = new System.IO.MemoryStream(8192)
        use writer = new System.IO.BinaryWriter(stream)
#if DEBUG
        do
#else
        try
#endif
            writer.Write enc.Type.AssemblyQualifiedName
            use w = new InterningWriter(memory)
            enc.Encode(w, value)
            w.WriteTo stream writer
#if DEBUG
#else
        with e ->
            let msg = "Failed to encode: " + string value
            raise (EncodingException(msg, e))
#endif

    member this.Type = enc.Type

[<Sealed>]
type EncodingProvider() =
    let cache = Dictionary()

    member this.DeriveEncoding t =
        Encoding (getCachedEncoder cache t)

    static member Create() =
        EncodingProvider()
