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
