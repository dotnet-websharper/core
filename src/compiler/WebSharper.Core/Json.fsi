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

/// Implements JSON encoding and decoding for client-server interaction.
module WebSharper.Core.Json

module M = WebSharper.Core.Metadata
module Re = WebSharper.Core.Resources

type JSModule = JSModule of AST.CodeResource

/// Represents JSON values.
type Value =
    | Null
    | True
    | False
    | Number of string
    | String of string
    | Array of list<Value>
    | Object of list<string * Value>

/// Thrown when text being read from the text reader is not valid JSON.
exception ReadException

/// Reads raw JSON. Throws ReadError.
val Read : System.IO.TextReader -> Value

/// Parses a JSON string. Throws ReadError.
val Parse : string -> Value

/// Thrown when the value being written is not valid JSON.
exception WriteException

/// Writes raw JSON. Throws WriteError.
val Write : System.IO.TextWriter -> Value -> unit

/// Converts JSON to a string. Throws WriteError.
val Stringify : Value -> string

/// Thrown when the decoder fails to reconstruct a value from JSON.
exception DecoderException of value:Value * typ:System.Type

/// Thrown when the encoder is given a value it cannot encode.
exception EncoderException

/// Thrown when no decoder can be derived for a given type.
exception NoDecoderException of typ:System.Type

/// Thrown when no encoder can be derived for a given type.
exception NoEncoderException of typ:System.Type

/// Represents an object encoded to JSON.
[<Sealed>]
type Encoded =

    /// Lifts a value.
    static member Lift : Value -> Encoded

    /// Constructs an array.
    static member Array : list<Encoded> -> Encoded

    /// Constructs an object.
    static member Object : list<string * Encoded> -> Encoded

/// Represents a decoder.
[<Sealed>]
type Decoder =
    member Decode : Value -> obj

/// Represents a typed decoder.
[<Sealed>]
type Decoder<'T> =
    member Decode : Value -> 'T

/// Represents an encoder.
[<Sealed>]
type Encoder =
    member Encode : obj -> Encoded

/// Represents a typed encoder.
[<Sealed>]
type Encoder<'T> =
    member Encode : 'T -> Encoded

type FormatOptions =
    {
        EncodeDateTime : option<string> -> System.DateTime -> Encoded
        DecodeDateTime : option<string> -> Value -> option<System.DateTime>
    }

    static member Default : FormatOptions

/// Provides JSON encodings for arbitrary types.
[<Sealed>]
type Provider =

    /// Constructs a basic JSON encoding provider.
    /// This provider uses an untyped encoding
    /// and is suitable for use with external APIs.
    /// It is the encoding used by Sitelets (Infer, JsonContent)
    /// and WebSharper.Json.Serialize/Deserialize.
    static member Create : unit -> Provider

    /// Constructs a basic JSON encoding provider.
    /// This provider uses an untyped encoding
    /// and is suitable for use with external APIs.
    /// It is the encoding used by Sitelets (Infer, JsonContent)
    /// and WebSharper.Json.Serialize/Deserialize.
    static member Create : FormatOptions -> Provider

    /// Constructs a typed JSON encoding provider.
    /// This provider uses a WebSharper-specific encoding of types
    /// and is only suitable for internal uses.
    /// It is the encoding used for Remoting and Web.Control initialization.
    static member CreateTyped : M.Info -> Provider

    /// Derives a decoder for a given type.
    member GetDecoder : System.Type -> Decoder

    /// Derives a decoder for a given type.
    member GetDecoder<'T> : unit -> Decoder<'T>

    /// Derives an encoder for a given type.
    member GetEncoder : System.Type -> Encoder

    /// Derives an encoder for a given type.
    member GetEncoder<'T> : unit -> Encoder<'T>

    member BuildDefaultValue : System.Type -> obj
    member BuildDefaultValue<'T> : unit -> 'T

    /// Packs an encoded value to JSON.
    member Pack : Encoded -> Value
    member PackWithTypes : Encoded -> Value * list<AST.Address>

/// Common functions about the JSON encoding.
module Internal =

    open System.Collections.Generic
    open System.Reflection

    type TypedNull<'T> =
        | TypedNull

    val MakeTypedNull : System.Type -> obj

    /// Get the (potentially customized) name of a field or property.
    val inline GetName : ^T -> string
        when ^T : (member GetCustomAttributesData : unit -> IList<CustomAttributeData>)
         and ^T : (member Name : string)

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

    /// Get the encoding characteristics of a discriminated union.
    /// t is assumed to be a discriminated union.
    val GetUnionEncoding : t: System.Type -> UnionDiscriminator * UnionCaseEncoding[]
