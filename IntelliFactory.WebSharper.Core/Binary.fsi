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

/// Implements binary serialization for server-side use.
/// The encoding supports all records, unions, numeric types,
/// strings, rank-1 arrays, maps, sets, lists and dictionaries.
/// Strings are interned for efficiency. The encoding also uses
/// binary compression.
module IntelliFactory.WebSharper.Core.Binary

/// Thrown when the decoder fails to reconstruct a value.
[<Sealed>]
type EncodingException =
    class inherit exn end

/// Thrown when no decoder can be derived for a given type.
exception NoEncodingException of System.Type

/// Represents an encoding for a given type.
[<Sealed>]
type Encoding =

    /// Decodes an object from a stream.
    member Decode : System.IO.Stream -> obj

    /// Encodes an object to a stream.
    member Encode : System.IO.Stream -> obj -> unit

    /// The type for which operations are supported.
    member Type : System.Type

/// Constructs Encoding objects.
[<Sealed>]
type EncodingProvider =

    /// Derives an encoding for a given type.
    member DeriveEncoding : System.Type -> Encoding

    /// Constructs a new EncodingProvider.
    static member Create : unit -> EncodingProvider
