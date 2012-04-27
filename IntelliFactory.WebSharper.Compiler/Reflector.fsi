// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Reflects assemblies to detect all annotated
/// WebSharper members and load quotations.
module internal IntelliFactory.WebSharper.Compiler.Reflector

module C = IntelliFactory.JavaScript.Core
module M = IntelliFactory.WebSharper.Core.Macros
module P = IntelliFactory.JavaScript.Packager
module Q = IntelliFactory.WebSharper.Core.Quotations
module R = IntelliFactory.WebSharper.Core.Reflection

/// Represents name annotations.
type Name =
    | RelativeName of string
    | AbsoluteName of P.Address

/// Represents a custom attribute annotation.
type Annotation =
    | Constant of Value
    | Curry of list<int>
    | Direct of string
    | Field of int
    | Inline of option<string>
    | JavaScript of Q.Expression
    | Macro of R.Type
    | Name of Name
    | Proxy of R.TypeDefinition
    | Remote
    | Require of R.TypeDefinition
    | Stub

[<Sealed>]
type AddressSlot =
    member Address : P.Address with get, set

[<Sealed>]
type ClassSlot =
    member BaseType : option<P.Address> with get, set

[<Sealed>]
type MemberSlot =
    member Address : AddressSlot
    member Field : P.Expression with set
    member Method : P.Expression with set
    member Member : option<P.Member>

/// Represents special kinds of F# types, such as modules or records.
type Kind =
    | Class of ClassSlot
    | Enum
    | Exception
    | Interface
    | Module
    | Record of list<Member<Mono.Cecil.PropertyDefinition>>
    | Union of list<UnionCase>

/// Represents members.
and Member<'T> =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : 'T
        Location : Location
        MemberSlot : MemberSlot
    }

/// Represents union cases.
and UnionCase =
    {
        Name : string
        Member : Member<Mono.Cecil.MethodDefinition>
    }

/// Represents properties.
type Property =
    {
        Member : Member<Mono.Cecil.PropertyDefinition>
        Getter : option<Member<Mono.Cecil.MethodDefinition>>
        Setter : option<Member<Mono.Cecil.MethodDefinition>>
    }

/// Represents a reflected type.
type Type =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : Mono.Cecil.TypeDefinition
        Kind : Kind
        Location : Location
        Methods : list<Member<Mono.Cecil.MethodDefinition>>
        Nested : list<Type>
        Properties : list<Property>
    }

/// Represents a reflected assembly.
type Assembly =
    {
        Name : R.AssemblyName
        Annotations : list<Annotation>
        Location : Location
        Types : list<Type>
    }

/// Reflects an assembly.
val Reflect : Logger -> Mono.Cecil.AssemblyDefinition -> Assembly

/// A utility class for pooling loaded macro definitions.
[<Sealed>]
type Pool =

    /// Loads a macro definition from a given type.
    member Load : R.Type -> M.Macro

    /// Constructs a macro pool.
    static member Create : Logger -> Pool
