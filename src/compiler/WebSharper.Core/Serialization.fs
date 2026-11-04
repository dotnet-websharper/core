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

/// Primitive readers and writers used by the generated metadata serializer (Serialization.g.fs).
/// Format notes:
/// - integers are variable-length (LEB128), signed ones zigzag-encoded;
/// - strings are indexes into a string table, written before everything else;
/// - Hashed values (type definitions, methods, constructors, addresses) are indexes into
///   per-kind tables; an entry is written the first time its value is seen, after the
///   entries it refers to, so the tables can be read in one pass before the body;
/// - Ids are written in full on first use and by index afterwards, in a scope that
///   restarts for each length-prefixed block (member expressions), so a block can be
///   read on its own;
/// - union cases are a byte tag followed by the fields.
module WebSharper.Core.Serialization

open System
open System.Collections.Generic
open System.Text

exception SerializationException of string

/// The sections of serialized metadata. Blocks (member expressions, the dependency graph)
/// are written into their own sections, so a reader can skip what it does not need.
module SectionKind =
    let [<Literal>] Strings = 0
    let [<Literal>] Tables = 1
    let [<Literal>] Main = 2
    /// Expressions of inline members, needed by every compilation using the library.
    let [<Literal>] Inline = 3
    /// Other member expressions and static constructors, needed for bundling and websites.
    let [<Literal>] Other = 4
    /// The dependency graph, needed for dead code elimination, bundling and websites.
    let [<Literal>] Graph = 5
    let [<Literal>] Count = 6

    let Name k =
        match k with
        | Strings -> "strings"
        | Tables -> "tables"
        | Main -> "main"
        | Inline -> "inline expressions"
        | Other -> "other expressions"
        | Graph -> "graph"
        | _ -> string k

/// A growable byte buffer.
[<Sealed; AllowNullLiteral>]
type ByteBuffer(capacity: int) =
    let mutable buf : byte[] = Array.zeroCreate (max capacity 16)
    let mutable pos = 0

    member private this.Ensure(n: int) =
        if pos + n > buf.Length then
            let nb = Array.zeroCreate (max (buf.Length * 2) (pos + n))
            Buffer.BlockCopy(buf, 0, nb, 0, pos)
            buf <- nb

    member this.Length = pos
    member this.Buffer = buf

    member this.Byte(b: byte) =
        if pos = buf.Length then this.Ensure 1
        buf.[pos] <- b
        pos <- pos + 1

    member this.Bytes(b: byte[], offset: int, count: int) =
        this.Ensure count
        Buffer.BlockCopy(b, offset, buf, pos, count)
        pos <- pos + count

    member this.VarUInt64(v: uint64) =
        this.Ensure 10
        let mutable v = v
        while v >= 0x80UL do
            buf.[pos] <- byte (v ||| 0x80UL)
            pos <- pos + 1
            v <- v >>> 7
        buf.[pos] <- byte v
        pos <- pos + 1

    member this.VarUInt(v: uint32) =
        this.Ensure 5
        let mutable v = v
        while v >= 0x80u do
            buf.[pos] <- byte (v ||| 0x80u)
            pos <- pos + 1
            v <- v >>> 7
        buf.[pos] <- byte v
        pos <- pos + 1

    member this.Fixed32(v: int) =
        this.Ensure 4
        buf.[pos] <- byte v
        buf.[pos + 1] <- byte (v >>> 8)
        buf.[pos + 2] <- byte (v >>> 16)
        buf.[pos + 3] <- byte (v >>> 24)
        pos <- pos + 4

    member this.Fixed64(v: int64) =
        this.Fixed32(int v)
        this.Fixed32(int (v >>> 32))

    /// Overwrites 4 bytes at a position written earlier.
    member this.PatchFixed32(at: int, v: int) =
        buf.[at] <- byte v
        buf.[at + 1] <- byte (v >>> 8)
        buf.[at + 2] <- byte (v >>> 16)
        buf.[at + 3] <- byte (v >>> 24)

    member this.Append(other: ByteBuffer) =
        this.Bytes(other.Buffer, 0, other.Length)

    member this.Clear() = pos <- 0

    member this.WriteTo(s: IO.Stream) = s.Write(buf, 0, pos)

    member this.ToArray() =
        let a = Array.zeroCreate pos
        Buffer.BlockCopy(buf, 0, a, 0, pos)
        a

/// Equality on Hashed values without boxing, comparing the cached hash first.
[<Sealed>]
type HashedComparer<'T when 'T : equality and 'T : comparison>() =
    static member val Instance = HashedComparer<'T>()
    interface IEqualityComparer<Hashed<'T>> with
        member this.Equals(a, b) = a.Hash = b.Hash && a.Value = b.Value
        member this.GetHashCode(a) = a.Hash

/// Writes metadata in the format read by MetaReader. Generated code calls these members.
[<Sealed>]
type MetaWriter(tableKinds: int) =
    let mutable out = ByteBuffer(64 * 1024)
    let tables = ByteBuffer(16 * 1024)
    let mutable tableEntries = 0
    let strings = Dictionary<string, int>()
    let stringList = ResizeArray<string>()
    let hashedTables : obj[] = Array.zeroCreate tableKinds
    let hashedCounts : int[] = Array.zeroCreate tableKinds
    let freeBuffers = Stack<ByteBuffer>()
    let normalizedIds = Dictionary<int64, int64>()
    let mutable ids = Dictionary<int64, int>()
    let mutable idCount = 0
    // saved output and Id scope while writing a table entry or a block
    let scopes = Stack<struct (ByteBuffer * Dictionary<int64, int> * int)>()
    let sections : ByteBuffer[] = Array.init SectionKind.Count (fun k -> if k >= SectionKind.Inline then ByteBuffer(16 * 1024) else null)

    let pushScope (newOut: ByteBuffer) =
        scopes.Push(struct (out, ids, idCount))
        out <- newOut
        ids <- Dictionary()
        idCount <- 0

    let popScope () =
        let struct (o, i, c) = scopes.Pop()
        out <- o
        ids <- i
        idCount <- c

    member this.Output = out

    member this.Byte(b: byte) = out.Byte b
    member this.Bool(b: bool) = out.Byte(if b then 1uy else 0uy)
    member this.Tag(t: int) = out.Byte(byte t)
    member this.Count(n: int) = out.VarUInt(uint32 n)
    member this.Int(v: int) = out.VarUInt(uint32 ((v <<< 1) ^^^ (v >>> 31)))
    member this.UInt(v: uint32) = out.VarUInt v
    member this.Int64(v: int64) = out.VarUInt64(uint64 ((v <<< 1) ^^^ (v >>> 63)))
    member this.UInt64(v: uint64) = out.VarUInt64 v
    member this.SByte(v: sbyte) = out.Byte(byte v)
    member this.Int16(v: int16) = this.Int(int v)
    member this.UInt16(v: uint16) = out.VarUInt(uint32 v)
    member this.Char(v: char) = out.VarUInt(uint32 v)
    member this.Double(v: double) = out.Fixed64(BitConverter.DoubleToInt64Bits v)
    member this.Single(v: single) = out.Fixed32(BitConverter.ToInt32(BitConverter.GetBytes v, 0))
    member this.Decimal(v: decimal) =
        for b in Decimal.GetBits v do out.Fixed32 b

    member this.String(s: string) =
        if isNull s then
            out.VarUInt 0u
        else
            match strings.TryGetValue s with
            | true, i -> out.VarUInt(uint32 (i + 1))
            | _ ->
                let i = stringList.Count
                strings.Add(s, i)
                stringList.Add s
                out.VarUInt(uint32 (i + 1))

    member private this.HashedTable<'T when 'T : equality and 'T : comparison>(kind: int) =
        match hashedTables.[kind] with
        | null ->
            let t = Dictionary<Hashed<'T>, int>(HashedComparer<'T>.Instance)
            hashedTables.[kind] <- box t
            t
        | t -> t :?> Dictionary<Hashed<'T>, int>

    /// Writes a reference to a Hashed value if it is already in its table and returns false.
    /// Otherwise returns true: the caller writes the value, then calls EndHashed.
    member this.BeginHashed<'T when 'T : equality and 'T : comparison>(kind: int, x: Hashed<'T>) =
        match this.HashedTable<'T>(kind).TryGetValue x with
        | true, i ->
            out.VarUInt(uint32 i)
            false
        | _ ->
            // the value goes to its own buffer: first-seen values nested in it
            // are added to the tables while it is written, so before it.
            // Tables are read up front, so an entry has its own Id scope.
            pushScope (if freeBuffers.Count > 0 then freeBuffers.Pop() else ByteBuffer(64))
            true

    member this.EndHashed<'T when 'T : equality and 'T : comparison>(kind: int, x: Hashed<'T>) =
        let entry = out
        popScope ()
        tables.Byte(byte kind)
        tables.Append entry
        entry.Clear()
        freeBuffers.Push entry
        tableEntries <- tableEntries + 1
        let i = hashedCounts.[kind]
        hashedCounts.[kind] <- i + 1
        this.HashedTable<'T>(kind).Add(x, i)
        out.VarUInt(uint32 i)

    /// Writes a reference to an Id already written in the current scope and returns false.
    /// Otherwise returns true: the caller writes the Id's fields, then calls EndId.
    member this.BeginId(key: int64) =
        match ids.TryGetValue key with
        | true, i ->
            out.VarUInt(uint32 (i + 1))
            false
        | _ ->
            out.VarUInt 0u
            true

    /// When set, Id numbers are written as their order of appearance, so the output does not
    /// depend on Ids created elsewhere in the compilation (used for hashing, not for reading).
    member val NormalizeIds = false with get, set

    /// Writes the number of an Id.
    member this.IdNumber(key: int64) =
        if this.NormalizeIds then
            match normalizedIds.TryGetValue key with
            | true, i -> this.Int64 i
            | _ ->
                let i = int64 normalizedIds.Count
                normalizedIds.Add(key, i)
                this.Int64 i
        else
            this.Int64 key

    /// Registers an Id after its fields, matching the reader.
    member this.EndId(key: int64) =
        ids.[key] <- idCount
        idCount <- idCount + 1

    /// Starts a block: a value written into another section, with its own Id scope.
    /// Writes the reference (section and offset) where the value would be.
    member this.BeginBlock(section: int) =
        let target = sections.[section]
        out.Byte(byte section)
        out.VarUInt(uint32 target.Length)
        pushScope target

    member this.EndBlock() =
        popScope ()

    /// The sections, indexed by SectionKind: string table, Hashed tables, main body, blocks.
    member this.Finish() : ByteBuffer[] =
        if scopes.Count > 0 then raise (SerializationException "Unfinished block or table entry")
        let str = ByteBuffer(stringList.Count * 16 + 16)
        str.VarUInt(uint32 stringList.Count)
        let enc = UTF8Encoding(false)
        let mutable bytes : byte[] = Array.zeroCreate 256
        for s in stringList do
            let n = enc.GetByteCount s
            if n > bytes.Length then bytes <- Array.zeroCreate (max n (bytes.Length * 2))
            enc.GetBytes(s, 0, s.Length, bytes, 0) |> ignore
            str.VarUInt(uint32 n)
            str.Bytes(bytes, 0, n)
        let tbl = ByteBuffer(tables.Length + 8)
        tbl.VarUInt(uint32 tableEntries)
        tbl.Append tables
        sections.[SectionKind.Strings] <- str
        sections.[SectionKind.Tables] <- tbl
        sections.[SectionKind.Main] <- out
        sections

/// Reads metadata written by MetaWriter. Generated code calls these members.
[<Sealed>]
type MetaReader(sections: ArraySegment<byte>[], tableKinds: int) =
    let mutable data : byte[] = null
    let mutable pos = 0
    let mutable limit = 0
    let mutable strings : string[] = [||]
    let hashedTables : obj[] = Array.zeroCreate tableKinds
    // Ids of all open scopes, the current scope starts at idBase
    let ids = ResizeArray<AST.Id>()
    let mutable idBase = 0
    let scopes = Stack<struct (byte[] * int * int * int)>()

    let fail () = raise (SerializationException "Unexpected end of WebSharper metadata")

    let select (k: int) (offset: int) =
        let s = sections.[k]
        if isNull s.Array then raise (SerializationException (sprintf "Metadata section not loaded: %s" (SectionKind.Name k)))
        if offset > s.Count then fail ()
        data <- s.Array
        pos <- s.Offset + offset
        limit <- s.Offset + s.Count

    /// Continues reading in another section (used for reading the sections in order).
    member this.Select(section: int) = select section 0

    /// Whether a section is available for reading blocks.
    member this.HasSection(section: int) = not (isNull sections.[section].Array)

    member this.Position = pos

    member this.Byte() =
        if pos >= limit then fail ()
        let b = data.[pos]
        pos <- pos + 1
        b

    member this.VarUInt() : uint32 =
        let mutable res = 0u
        let mutable shift = 0
        let mutable b = this.Byte()
        while b >= 0x80uy do
            res <- res ||| (uint32 (b &&& 0x7Fuy) <<< shift)
            shift <- shift + 7
            b <- this.Byte()
        res ||| (uint32 b <<< shift)

    member this.VarUInt64() : uint64 =
        let mutable res = 0UL
        let mutable shift = 0
        let mutable b = this.Byte()
        while b >= 0x80uy do
            res <- res ||| (uint64 (b &&& 0x7Fuy) <<< shift)
            shift <- shift + 7
            b <- this.Byte()
        res ||| (uint64 b <<< shift)

    member this.Fixed32() =
        if pos + 4 > limit then fail ()
        let v = int data.[pos] ||| (int data.[pos + 1] <<< 8) ||| (int data.[pos + 2] <<< 16) ||| (int data.[pos + 3] <<< 24)
        pos <- pos + 4
        v

    member this.Fixed64() =
        let lo = this.Fixed32()
        let hi = this.Fixed32()
        (int64 (uint32 lo)) ||| (int64 hi <<< 32)

    member this.Bool() = this.Byte() <> 0uy
    member this.Tag() = int (this.Byte())
    member this.Count() = int (this.VarUInt())
    member this.Int() = let v = this.VarUInt() in int (v >>> 1) ^^^ -(int (v &&& 1u))
    member this.UInt() = this.VarUInt()
    member this.Int64() = let v = this.VarUInt64() in int64 (v >>> 1) ^^^ -(int64 (v &&& 1UL))
    member this.UInt64() = this.VarUInt64()
    member this.SByte() = sbyte (this.Byte())
    member this.Int16() = int16 (this.Int())
    member this.UInt16() = uint16 (this.VarUInt())
    member this.Char() = char (this.VarUInt())
    member this.Double() = BitConverter.Int64BitsToDouble(this.Fixed64())
    member this.Single() = BitConverter.ToSingle(BitConverter.GetBytes(this.Fixed32()), 0)
    member this.Decimal() =
        let a = this.Fixed32()
        let b = this.Fixed32()
        let c = this.Fixed32()
        let d = this.Fixed32()
        Decimal([| a; b; c; d |])

    member this.String() =
        match this.VarUInt() with
        | 0u -> null
        | i -> strings.[int i - 1]

    member this.Hashed<'T when 'T : equality and 'T : comparison>(kind: int) : Hashed<'T> =
        let i = int (this.VarUInt())
        (hashedTables.[kind] :?> ResizeArray<Hashed<'T>>).[i]

    /// Called by the generated table reader for each entry.
    member this.AddHashed<'T when 'T : equality and 'T : comparison>(kind: int, value: 'T) =
        let table =
            match hashedTables.[kind] with
            | null ->
                let t = ResizeArray<Hashed<'T>>()
                hashedTables.[kind] <- box t
                t
            | t -> t :?> ResizeArray<Hashed<'T>>
        table.Add(Hashed value)

    /// Reads an Id reference: null when the Id follows in full, then the caller reads it and calls AddId.
    member this.IdRef() : AST.Id =
        match this.VarUInt() with
        | 0u -> Unchecked.defaultof<_>
        | i -> ids.[idBase + int i - 1]

    member this.AddId(id: AST.Id) =
        ids.Add id

    /// Starts a new Id scope in the current section (for each Hashed table entry).
    member this.BeginScope() =
        scopes.Push(struct (data, pos, limit, idBase))
        idBase <- ids.Count

    member this.EndScope() =
        ids.RemoveRange(idBase, ids.Count - idBase)
        let struct (_, _, _, b) = scopes.Pop()
        idBase <- b

    /// Reads a block reference. If its section is loaded, continues reading the block
    /// there with its own Id scope, returns true and the caller calls EndBlock after.
    /// Otherwise returns false: the block is skipped.
    member this.BeginBlock() =
        let section = int (this.Byte())
        let offset = int (this.VarUInt())
        if isNull sections.[section].Array then
            false
        else
            scopes.Push(struct (data, pos, limit, idBase))
            idBase <- ids.Count
            select section offset
            true

    member this.EndBlock() =
        ids.RemoveRange(idBase, ids.Count - idBase)
        let struct (d, p, l, b) = scopes.Pop()
        data <- d
        pos <- p
        limit <- l
        idBase <- b

    /// Reads the string table and the entry count of the Hashed tables.
    member this.ReadStrings() =
        let n = this.Count()
        let enc = UTF8Encoding(false)
        let s = Array.zeroCreate n
        for i in 0 .. n - 1 do
            let len = this.Count()
            if pos + len > limit then fail ()
            s.[i] <- enc.GetString(data, pos, len)
            pos <- pos + len
        strings <- s

module Helpers =

    let inline writeList (w: MetaWriter) ([<InlineIfLambda>] f: MetaWriter -> 'T -> unit) (l: list<'T>) =
        w.Count(List.length l)
        for x in l do f w x

    let inline readList (r: MetaReader) ([<InlineIfLambda>] f: MetaReader -> 'T) : list<'T> =
        match r.Count() with
        | 0 -> []
        | 1 -> [ f r ]
        | 2 ->
            let a = f r
            let b = f r
            [ a; b ]
        | n ->
            let arr = Array.zeroCreate n
            for i in 0 .. n - 1 do
                arr.[i] <- f r
            List.ofArray arr

    let inline writeOption (w: MetaWriter) ([<InlineIfLambda>] f: MetaWriter -> 'T -> unit) (o: option<'T>) =
        match o with
        | None -> w.Byte 0uy
        | Some x ->
            w.Byte 1uy
            f w x

    let inline readOption (r: MetaReader) ([<InlineIfLambda>] f: MetaReader -> 'T) : option<'T> =
        match r.Byte() with
        | 0uy -> None
        | _ -> Some (f r)

    let inline writeArray (w: MetaWriter) ([<InlineIfLambda>] f: MetaWriter -> 'T -> unit) (a: 'T[]) =
        w.Count a.Length
        for x in a do f w x

    let inline readArray (r: MetaReader) ([<InlineIfLambda>] f: MetaReader -> 'T) : 'T[] =
        let n = r.Count()
        let arr = Array.zeroCreate n
        for i in 0 .. n - 1 do
            arr.[i] <- f r
        arr

    let inline writeSet (w: MetaWriter) ([<InlineIfLambda>] f: MetaWriter -> 'T -> unit) (s: Set<'T>) =
        w.Count s.Count
        for x in s do f w x

    let inline readSet (r: MetaReader) ([<InlineIfLambda>] f: MetaReader -> 'T) : Set<'T> =
        match r.Count() with
        | 0 -> Set.empty
        | n ->
            let arr = Array.zeroCreate n
            for i in 0 .. n - 1 do
                arr.[i] <- f r
            Set.ofArray arr

    let inline writeMap (w: MetaWriter) ([<InlineIfLambda>] fk: MetaWriter -> 'K -> unit) ([<InlineIfLambda>] fv: MetaWriter -> 'V -> unit) (m: Map<'K, 'V>) =
        w.Count m.Count
        for KeyValue(k, v) in m do
            fk w k
            fv w v

    let inline readMap (r: MetaReader) ([<InlineIfLambda>] fk: MetaReader -> 'K) ([<InlineIfLambda>] fv: MetaReader -> 'V) : Map<'K, 'V> =
        let n = r.Count()
        let arr = Array.zeroCreate n
        for i in 0 .. n - 1 do
            let k = fk r
            let v = fv r
            arr.[i] <- (k, v)
        Map.ofArray arr

    let inline writeDict (w: MetaWriter) ([<InlineIfLambda>] fk: MetaWriter -> 'K -> unit) ([<InlineIfLambda>] fv: MetaWriter -> 'V -> unit) (d: IDictionary<'K, 'V>) =
        w.Count d.Count
        for KeyValue(k, v) in d do
            fk w k
            fv w v

    let inline readDict (r: MetaReader) ([<InlineIfLambda>] fk: MetaReader -> 'K) ([<InlineIfLambda>] fv: MetaReader -> 'V) : IDictionary<'K, 'V> =
        let n = r.Count()
        let d = Dictionary<'K, 'V>(n)
        for i in 0 .. n - 1 do
            let k = fk r
            let v = fv r
            d.[k] <- v
        d :> _

    /// Writes a value as a block in the given section, with its own Id scope.
    let inline writeBlock (w: MetaWriter) (section: int) ([<InlineIfLambda>] f: MetaWriter -> 'T -> unit) (x: 'T) =
        w.BeginBlock section
        f w x
        w.EndBlock()

    /// Reads a block, or returns the default value if its section is not loaded.
    let inline readBlock (r: MetaReader) ([<InlineIfLambda>] f: MetaReader -> 'T) (skipped: 'T) : 'T =
        if r.BeginBlock() then
            let res = f r
            r.EndBlock()
            res
        else
            skipped

/// Decides which section a block goes to.
module SectionHooks =
    open WebSharper.Core.Metadata

    /// Inline members (also as macro fallbacks) are needed by every compilation using the library.
    let rec memberSection (cf: CompiledMember) =
        match cf with
        | Inline _ -> SectionKind.Inline
        | Macro (_, _, Some f) -> memberSection f
        | _ -> SectionKind.Other
