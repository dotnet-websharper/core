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

/// Reading and writing WebSharper metadata in its binary format.
module WebSharper.Core.MetadataIO

open System
open System.IO
open WebSharper.Constants
open WebSharper.Core.Serialization

type Info = WebSharper.Core.Metadata.Info

/// Current metadata format version, checked when reading to ensure consistency across WebSharper libraries.
let CurrentVersion = "11.0-rev1"

/// Compression of the metadata payload.
type Compression =
    | NoCompression = 0uy
    | Deflate = 1uy

/// The compression used when writing metadata.
let mutable DefaultCompression = Compression.Deflate

let private magic = [| byte 'W'; byte 'S'; byte 'M'; byte 'D' |]

exception MetadataFormatException of string

let private writeHeader (s: Stream) (compression: Compression) (payloadLength: int) =
    s.Write(magic, 0, 4)
    let v = Text.Encoding.UTF8.GetBytes CurrentVersion
    s.WriteByte(byte v.Length)
    s.Write(v, 0, v.Length)
    s.WriteByte(byte compression)
    let len = BitConverter.GetBytes(payloadLength)
    if not BitConverter.IsLittleEndian then Array.Reverse len
    s.Write(len, 0, 4)

/// Serializes metadata into a stream, returns the time spent encoding and compressing.
let EncodeTimed (stream: Stream) (meta: Info) =
    let sw = Diagnostics.Stopwatch.StartNew()
    let w = MetaWriter(SerializationGenerated.HashedKinds)
    SerializationGenerated.writeInfo w meta
    let payload = w.Finish()
    let encodeTime = sw.Elapsed
    sw.Restart()
    let compression = DefaultCompression
    writeHeader stream compression payload.Length
    match compression with
    | Compression.Deflate ->
        use d = new Compression.DeflateStream(stream, Compression.CompressionLevel.Fastest, true)
        payload.WriteTo d
    | _ ->
        payload.WriteTo stream
    encodeTime, sw.Elapsed

/// Serializes metadata into a stream.
let Encode (stream: Stream) (meta: Info) =
    EncodeTimed stream meta |> ignore

/// Serializes metadata into a byte array.
let EncodeToBytes (meta: Info) =
    use m = new MemoryStream(64 * 1024)
    Encode m meta
    m.ToArray()

/// Reads the header, returns the compression, payload length and the offset of the payload.
let private readHeader (data: byte[]) (offset: int) (length: int) =
    let fail msg = raise (MetadataFormatException msg)
    if length < 10 || data.[offset] <> magic.[0] || data.[offset + 1] <> magic.[1] || data.[offset + 2] <> magic.[2] || data.[offset + 3] <> magic.[3] then
        fail "Not WebSharper metadata in the current format. Libraries compiled with WebSharper 10 or older need recompiling."
    let vlen = int data.[offset + 4]
    let ver = Text.Encoding.UTF8.GetString(data, offset + 5, vlen)
    if ver <> CurrentVersion then
        fail (sprintf "WebSharper metadata format version mismatch, found '%s', expecting '%s'" ver CurrentVersion)
    let p = offset + 5 + vlen
    let compression : Compression = LanguagePrimitives.EnumOfValue data.[p]
    let payloadLength =
        int data.[p + 1] ||| (int data.[p + 2] <<< 8) ||| (int data.[p + 3] <<< 16) ||| (int data.[p + 4] <<< 24)
    compression, payloadLength, p + 5

/// Returns the uncompressed payload as buffer, offset and length.
let private payloadOf (data: byte[]) (offset: int) (length: int) =
    let compression, payloadLength, start = readHeader data offset length
    match compression with
    | Compression.NoCompression ->
        data, start, payloadLength
    | Compression.Deflate ->
        let res = Array.zeroCreate payloadLength
        use d = new Compression.DeflateStream(new MemoryStream(data, start, offset + length - start), Compression.CompressionMode.Decompress)
        let mutable read = 0
        while read < payloadLength do
            let n = d.Read(res, read, payloadLength - read)
            if n = 0 then raise (MetadataFormatException "Unexpected end of compressed WebSharper metadata")
            read <- read + n
        res, 0, payloadLength
    | c ->
        raise (MetadataFormatException (sprintf "Unknown WebSharper metadata compression: %d" (int c)))

/// Deserializes metadata from a byte array segment.
let DecodeBytesAt (data: byte[]) (offset: int) (length: int) : Info =
    let buf, start, len = payloadOf data offset length
    let r = MetaReader(buf, start, len, SerializationGenerated.HashedKinds)
    r.ReadStrings()
    SerializationGenerated.readTables r
    SerializationGenerated.readInfo r

/// Deserializes metadata from a byte array.
let DecodeBytes (data: byte[]) : Info =
    DecodeBytesAt data 0 data.Length

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
