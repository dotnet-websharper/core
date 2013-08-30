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

namespace IntelliFactory.WebSharper

/// Implements a [TypeScript][1] definitions writer.
/// Currently targets version 0.9.1 of the language.
///
/// [1]: http://www.typescriptlang.org/
module internal TypeScriptGenerator =
    open System
    open System.Collections.Generic
    open System.IO

    /// Qualified name of a value binding or contract.
    [<Sealed>]
    type Address =
        interface IComparable
        interface IComparable<Address>

    /// Factory object for `Address` values.
    [<Sealed>]
    type AddressBuilder

    /// Forward-declared named contract.
    [<Sealed>]
    type Declaration

    /// Definition set.
    [<Sealed>]
    type Definitions

    /// A contract (type) to describe values.
    [<Sealed>]
    type Contract

    /// Object contract consisting of method and property signatures.
    [<Sealed>]
    type Interface

    /// Interface members.
    [<Sealed>]
    type Member

    /// Call, constructor or method signature.
    [<Sealed>]
    type Signature

    /// Options for writing definitions.
    type WriteOptions =
        {
            /// Prefix top-level declaration with `export` keyword.
            ExportDeclarations : bool
        }

    // ------------------------------------------------------------------------

    /// `Address` members.
    type Address with

        /// The related builder.
        member Builder : AddressBuilder

        /// Constructs a nested address.
        member Item : string -> Address with get

    /// `AddressBuilder` members.
    type AddressBuilder with

        /// Creates nested address.
        member Nested : Address * string -> Address

        /// Creates root address.
        member Root : string -> Address

        /// Creates a new instance.
        static member Create : unit -> AddressBuilder

    /// Declaration combinators.
    type Declaration with

        /// Accesses the generic argument by position.
        member Item : int -> Contract with get

        /// Declares a new named contract.
        static member Create : Address * ?generics: list<string> -> Declaration

    /// Definitions combinators.
    type Definitions with

        /// Writes the definitions in TypeScript format to the writer.
        member Write : writer: TextWriter * ?options: WriteOptions -> unit

        /// Checks definitions invariants, throwing exceptions on inconsistencies.
        member Verify : unit -> unit

        /// Provides a body (object type) to a named type declaration.
        static member Define : Declaration * Interface -> Definitions

        /// Merges definitions.
        static member Merge : seq<Definitions> -> Definitions

        /// Defines a value binding.
        static member Var : Address * Contract -> Definitions

    /// Contract combinators.
    type Contract with

        /// Defines an anonymous contract.
        static member Anonymous : Interface -> Contract

        /// Array contract.
        static member Array : Contract -> Contract

        /// A generic variable defined at the contract level.
        static member Generic : Declaration * position: int -> Contract

        /// A generic variable defined at the signature level.
        static member Generic : Signature * position: int -> Contract

        /// Named pre-declared contract, possibly with generic arguments.
        static member Named : Declaration * ?generics: list<Contract> -> Contract

        /// Built-in 'any' contract.
        static member Any : Contract

        /// Built-in 'boolean' contract.
        static member Boolean : Contract

        /// Built-in 'number' contract.
        static member Number : Contract

        /// Built-in 'string' contract.
        static member String : Contract

        /// Built-in 'void' contract.
        static member Void : Contract

    /// Interface combinators.
    type Interface with

        /// Defines an interface.
        static member Create : seq<Member> -> Interface

    /// Member combinators.
    type Member with

        /// Defines a by-number indexer.
        static member ByNumber : contract: Contract * ?name: string -> Member

        /// Defines a by-string indexer.
        static member ByString : contract: Contract * ?name: string -> Member

        /// Defines a call member.
        static member Call : Signature -> Member

        /// Defines a constructor.
        static member Construct : Signature -> Member

        /// Defines a named method.
        static member Method : name: string * sign: Signature -> Member

        /// Defines a numerically named method.
        static member NumericMethod : pos: int * sign: Signature -> Member

        /// Defins a numerically named property.
        static member NumericProperty : pos: int * contract: Contract -> Member

        /// Defins a named propery.
        static member Property : name: string * contract: Contract -> Member

    /// Signature combinators.
    type Signature with

        /// Accesses the generic argument by position.
        member Item : int -> Contract with get

        /// Adds an argument by functional update.
        member WithArgument : name: string * Contract -> Signature

        /// Sets the return type by functional update.
        member WithReturn : Contract -> Signature

        /// Defines a new signature.
        static member Create : ?generics: list<string> -> Signature

