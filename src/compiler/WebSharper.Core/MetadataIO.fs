// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// Reading and writing WebSharper metadata in its binary format.
///
/// Layout: "WSMD", format version, section count, then for each section its kind,
/// compression, uncompressed and stored length, followed by the section contents.
/// Sections are compressed separately, so skipped sections are not even decompressed.
/// See WebSharper.Core.Serialization for the contents.
module WebSharper.Core.MetadataIO

open System
open System.IO
open WebSharper.Constants
open WebSharper.Core.Serialization

type Info = WebSharper.Core.Metadata.Info

/// Current metadata format version, checked when reading to ensure consistency across WebSharper libraries.
let CurrentVersion = "11.0-preview.1"

/// Compression of a metadata section.
type Compression =
    | NoCompression = 0uy
    | Deflate = 1uy

/// The compression used when writing metadata.
let mutable DefaultCompression = Compression.Deflate

/// Sections smaller than this are stored uncompressed.
let private minCompressedLength = 256

/// Which optional parts of metadata to read. Skipped expressions read as Undefined,
/// skipped static constructors as Empty, a skipped graph as empty.
type ReadOptions =
    {
        /// Expressions of inline members (also as macro fallbacks).
        InlineExpressions : bool
        /// Other member expressions and static constructors.
        OtherExpressions : bool
        /// The dependency graph.
        Graph : bool
    }

    static member Full = { InlineExpressions = true; OtherExpressions = true; Graph = true }

    /// What compiling a library needs from its references: no non-inline code and no graph.
    static member Library = { InlineExpressions = true; OtherExpressions = false; Graph = false }

    /// The sections needed for metadata options, which are still applied after reading.
    static member Of (options: Metadata.MetadataOptions) =
        match options with
        | Metadata.FullMetadata -> ReadOptions.Full
        | Metadata.DiscardExpressions -> { InlineExpressions = false; OtherExpressions = false; Graph = true }
        | Metadata.DiscardInlineExpressions -> { InlineExpressions = false; OtherExpressions = true; Graph = true }
        | Metadata.DiscardNotInlineExpressions -> { InlineExpressions = true; OtherExpressions = false; Graph = true }

    member this.Reads(section: int) =
        match section with
        | SectionKind.Inline -> this.InlineExpressions
        | SectionKind.Other -> this.OtherExpressions
        | SectionKind.Graph -> this.Graph
        | _ -> true

let private magic = [| byte 'W'; byte 'S'; byte 'M'; byte 'D' |]

exception MetadataFormatException of string

let private writeInt32 (s: Stream) (v: int) =
    s.WriteByte(byte v)
    s.WriteByte(byte (v >>> 8))
    s.WriteByte(byte (v >>> 16))
    s.WriteByte(byte (v >>> 24))

let private encodeWith (compression: Compression) (normalizeIds: bool) (stream: Stream) (meta: Info) =
    let sw = Diagnostics.Stopwatch.StartNew()
    let w = MetaWriter(SerializationGenerated.HashedKinds, NormalizeIds = normalizeIds)
    SerializationGenerated.writeInfo w meta
    let sections = w.Finish()
    let encodeTime = sw.Elapsed
    sw.Restart()
    let stored =
        sections |> Array.map (fun (s: ByteBuffer) ->
            if compression = Compression.Deflate && s.Length >= minCompressedLength then
                use m = new MemoryStream(s.Length / 2 + 64)
                (
                    use d = new Compression.DeflateStream(m, Compression.CompressionLevel.Fastest, true)
                    s.WriteTo d
                )
                Compression.Deflate, m.ToArray()
            else
                Compression.NoCompression, s.ToArray()
        )
    stream.Write(magic, 0, 4)
    let v = Text.Encoding.UTF8.GetBytes CurrentVersion
    stream.WriteByte(byte v.Length)
    stream.Write(v, 0, v.Length)
    stream.WriteByte(byte sections.Length)
    for k in 0 .. sections.Length - 1 do
        let c, bytes = stored.[k]
        stream.WriteByte(byte k)
        stream.WriteByte(byte c)
        writeInt32 stream sections.[k].Length
        writeInt32 stream bytes.Length
    for _, bytes in stored do
        stream.Write(bytes, 0, bytes.Length)
    encodeTime, sw.Elapsed

/// Serializes metadata into a stream, returns the time spent encoding and compressing.
let EncodeTimed (stream: Stream) (meta: Info) =
    encodeWith DefaultCompression false stream meta

/// Serializes metadata into a stream.
let Encode (stream: Stream) (meta: Info) =
    EncodeTimed stream meta |> ignore

/// Serializes metadata into a byte array.
let EncodeToBytes (meta: Info) =
    use m = new MemoryStream(64 * 1024)
    Encode m meta
    m.ToArray()

/// A section of stored metadata.
type private SectionEntry =
    {
        Kind : int
        Compression : Compression
        RawLength : int
        Offset : int
        StoredLength : int
    }

let private fail msg = raise (MetadataFormatException msg)

let private readEntries (data: byte[]) (offset: int) (length: int) =
    let limit = offset + length
    if length < 6 || data.[offset] <> magic.[0] || data.[offset + 1] <> magic.[1] || data.[offset + 2] <> magic.[2] || data.[offset + 3] <> magic.[3] then
        fail "Not WebSharper metadata in the current format. Libraries compiled with WebSharper 10 or older need recompiling."
    let vlen = int data.[offset + 4]
    if offset + 5 + vlen >= limit then fail "Truncated WebSharper metadata header"
    let ver = Text.Encoding.UTF8.GetString(data, offset + 5, vlen)
    if ver <> CurrentVersion then
        fail (sprintf "WebSharper metadata format version mismatch, found '%s', expecting '%s'. Recompile the library with the current WebSharper version." ver CurrentVersion)
    let p = offset + 5 + vlen
    let count = int data.[p]
    let tableStart = p + 1
    let int32At i = int data.[i] ||| (int data.[i + 1] <<< 8) ||| (int data.[i + 2] <<< 16) ||| (int data.[i + 3] <<< 24)
    if tableStart + count * 10 > limit then fail "Truncated WebSharper metadata header"
    let mutable contents = tableStart + count * 10
    Array.init count (fun i ->
        let e = tableStart + i * 10
        let stored = int32At (e + 6)
        if contents + stored > limit then fail "Truncated WebSharper metadata"
        let entry =
            {
                Kind = int data.[e]
                Compression = LanguagePrimitives.EnumOfValue data.[e + 1]
                RawLength = int32At (e + 2)
                Offset = contents
                StoredLength = stored
            }
        contents <- contents + stored
        entry
    )

let private sectionContents (data: byte[]) (e: SectionEntry) =
    match e.Compression with
    | Compression.NoCompression ->
        ArraySegment(data, e.Offset, e.StoredLength)
    | Compression.Deflate ->
        let res = Array.zeroCreate e.RawLength
        use d = new Compression.DeflateStream(new MemoryStream(data, e.Offset, e.StoredLength), Compression.CompressionMode.Decompress)
        let mutable read = 0
        while read < e.RawLength do
            let n = d.Read(res, read, e.RawLength - read)
            if n = 0 then fail "Unexpected end of compressed WebSharper metadata"
            read <- read + n
        ArraySegment(res)
    | c ->
        fail (sprintf "Unknown WebSharper metadata compression: %d" (int c))

/// Deserializes the selected parts of metadata from a byte array segment.
let DecodeBytesWith (options: ReadOptions) (data: byte[]) (offset: int) (length: int) : Info =
    let entries = readEntries data offset length
    let sections = Array.zeroCreate<ArraySegment<byte>> SectionKind.Count
    for e in entries do
        if e.Kind < SectionKind.Count && options.Reads e.Kind then
            sections.[e.Kind] <- sectionContents data e
    for k in [ SectionKind.Strings; SectionKind.Tables; SectionKind.Main ] do
        if isNull sections.[k].Array then fail (sprintf "WebSharper metadata is missing its %s section" (SectionKind.Name k))
    let r = MetaReader(sections, SerializationGenerated.HashedKinds)
    r.Select SectionKind.Strings
    r.ReadStrings()
    r.Select SectionKind.Tables
    SerializationGenerated.readTables r
    r.Select SectionKind.Main
    SerializationGenerated.readInfo r

/// Deserializes metadata from a byte array segment.
let DecodeBytesAt (data: byte[]) (offset: int) (length: int) : Info =
    DecodeBytesWith ReadOptions.Full data offset length

/// Deserializes metadata from a byte array.
let DecodeBytes (data: byte[]) : Info =
    DecodeBytesAt data 0 data.Length

/// A hash of what compiling a library against this metadata reads (ReadOptions.Library),
/// independent of changes elsewhere: the library view is encoded again on its own.
let InterfaceHash (data: byte[]) =
    let lib = DecodeBytesWith ReadOptions.Library data 0 data.Length
    use m = new MemoryStream(data.Length)
    encodeWith Compression.NoCompression true m lib |> ignore
    use sha = Security.Cryptography.SHA256.Create()
    let h = sha.ComputeHash(m.GetBuffer(), 0, int m.Length)
    BitConverter.ToString(h).Replace("-", "")

/// A hash of metadata that does not depend on the numbers of Ids, for comparing compilations.
let ContentHash (meta: Info) =
    use m = new MemoryStream(64 * 1024)
    encodeWith Compression.NoCompression true m meta |> ignore
    use sha = Security.Cryptography.SHA256.Create()
    let h = sha.ComputeHash(m.GetBuffer(), 0, int m.Length)
    BitConverter.ToString(h).Replace("-", "")

/// Section names with their stored and uncompressed sizes.
let SectionSizes (data: byte[]) =
    readEntries data 0 data.Length
    |> Array.map (fun e -> SectionKind.Name e.Kind, e.StoredLength, e.RawLength)

/// Deserializes metadata from a stream.
let Decode (stream: Stream) : Info =
    match stream with
    | :? MemoryStream as m when m.Position = 0L ->
        match m.TryGetBuffer() with
        | true, seg -> DecodeBytesAt seg.Array seg.Offset seg.Count
        | _ -> DecodeBytes (m.ToArray())
    | _ ->
        use m = new MemoryStream()
        stream.CopyTo m
        DecodeBytes (m.ToArray())

/// Load metadata from an in-memory assembly, which must not be loaded as reflection-only.
let LoadMetadata (a: System.Reflection.Assembly) =
    use s = a.GetManifestResourceStream EMBEDDED_METADATA
    if isNull s then
        None
    else
        try
            Some (Decode s)
        with e ->
            raise (exn (sprintf "Failed to load metadata for: %s. Error: %s" a.FullName e.Message, e))

/// Load runtime metadata from an in-memory assembly, which must not be loaded as reflection-only.
let LoadRuntimeMetadata (a: System.Reflection.Assembly) =
    use s = a.GetManifestResourceStream EMBEDDED_RUNTIME_METADATA
    if isNull s then
        None
    else
        try
            Some (Decode s)
        with e ->
            raise (exn (sprintf "Failed to load runtime metadata for: %s. Error: %s" a.FullName e.Message, e))
