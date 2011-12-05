// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

/// Implements JSON encoding and decoding for client-server interaction.
module IntelliFactory.WebSharper.Core.Json

module M  = IntelliFactory.WebSharper.Core.Metadata
module Re = IntelliFactory.WebSharper.Core.Resources

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
exception DecoderException

/// Thrown when the encoder is given a value it cannot encode.
exception EncoderException

/// Thrown when no decoder can be derived for a given type.
exception NoDecoderException of System.Type

/// Thrown when no encoder can be derived for a given type.
exception NoEncoderException of System.Type

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

/// Provides JSON encodings for arbitrary types.
[<Sealed>]
type Provider =

    /// Constructs a basic JSON encoding provider.
    static member Create : unit -> Provider

    /// Constructs a typed JSON encoding provider.
    static member CreateTyped : M.Info -> Provider

    /// Derives a decoder for a given type.
    member GetDecoder : System.Type -> Decoder

    /// Derives a decoder for a given type.
    member GetDecoder<'T> : unit -> Decoder<'T>

    /// Derives an encoder for a given type.
    member GetEncoder : System.Type -> Encoder

    /// Derives an encoder for a given type.
    member GetEncoder<'T> : unit -> Encoder<'T>

    /// Packs an encoded value to JSON.
    member Pack : Encoded -> Value
