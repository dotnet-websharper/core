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

namespace WebSharper

/// Facilities for dealing with qualified names required by the compiler.
module internal QualifiedNames =
    open System
    open System.Collections.Generic

    /// Factory object for `Name` and `Id` objects.
    [<Sealed>]
    type Builder

    /// Configures the qualified name builder.
    type Config =
        {
            /// Concatenates a non-empty list of valid identifiers into
            /// a textual qualified name representation.
            /// Default: `String.concat "."`.
            ConcatIdentifiers : list<string> -> string

            /// Builds an exception to throw for invalid identifiers.
            /// Default: throws `InvalidIdentifier`.
            InvalidId : string -> exn

            /// Checks that an identifier is valid.
            /// Default: checks that the identifier is alphanumeric.
            IsValidId : string -> bool
        }

    /// Represents a simple non-qualified identifier.
    [<Sealed>]
    type Id =
        interface IComparable
        interface IComparable<Id>

    /// A vector of identifiers.
    [<Sealed>]
    type IdVector

    /// Represents a qualified name.
    [<Sealed>]
    type Name =
        interface IComparable
        interface IComparable<Name>

    /// Combinators for manipulating named maps.
    [<Sealed>]
    type NameMap

    /// A finite map from qualified names to values of a given type.
    [<Sealed>]
    type NameMap<'T>

    /// Thrown when an identifier (qualified name part) is invalid.
    [<Sealed>]
    exception InvalidIdentifier of string

    /// Thrown when names collide.
    [<Sealed>]
    exception NameClash of Name

    // ------------------------------------------------------------------------

    /// Builder members.
    type Builder with

        /// Constructs an identifier.
        member Id : string -> Id

        /// Constructs a nested address.
        member Nested : Name * Id -> Name

        /// Constructs a global non-nested address from a text form of an.
        member Root : Id -> Name

        /// Current configuration.
        member Config : Config

        /// Creates a new instance.
        static member Create : ?config: Config -> Builder

    /// Config members.
    type Config with

        /// The default configuration.
        static member Default : Config

    /// Id combinators.
    type Id with

        /// The associated builder.
        member Builder : Builder

        /// Text form of the identifer.
        member Text : string

    /// IdVector combinators.
    type IdVector with

        /// Returns the n-th item in the vector.
        member Item : int -> option<Id> with get

        /// Returns the index of the first occurence of identifier, or -1.
        member IndexOf : Id -> int

        /// Creates a vector.
        static member Create : seq<Id> -> IdVector

    /// Name members.
    type Name with

        /// The associated builder.
        member Builder : Builder

        /// Constructs a nested name.
        member Item : Id -> Name with get

        /// The local name without the namespace.
        member Id : Id

        /// Text form of the name.
        member Text : string

    /// NameMap instance members.
    type NameMap<'T> with
        member ContainsKey : Name -> bool
        member Iterate : (Name -> 'T -> unit) -> unit
        member TryFind : Name -> option<'T>

    /// NameMap static members.
    type NameMap with
        static member Merge : seq<NameMap<'T>> -> NameMap<'T>
        static member Singleton : Name * 'T -> NameMap<'T>

    /// Decomposes a qualified name.
    val (|Nested|Root|) : Name -> Choice<Name * Id, Id>
