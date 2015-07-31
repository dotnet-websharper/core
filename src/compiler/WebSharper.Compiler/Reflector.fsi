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

/// Reflects assemblies to detect all annotated
/// WebSharper members and load quotations.
module internal WebSharper.Compiler.Reflector

module C = WebSharper.Core.JavaScript.Core
module M = WebSharper.Core.Macros
module P = WebSharper.Core.JavaScript.Packager
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection

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
    | Generated of R.Type
    | Name of Name
    | Proxy of R.TypeDefinition
    | Remote
    | RemotingProvider of R.TypeDefinition
    | Require of R.TypeDefinition
    | Stub
    | OptionalField

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
    | Record of list<Member<PropertyDefinition>>
    | Union of list<UnionCase>

/// Represents members.
and Member<'T> =
    {
        Annotations : list<Annotation>
        Definition : 'T
        Location : Location
        MemberSlot : MemberSlot
    }

    member AddressSlot : AddressSlot

/// Represents union cases.
and UnionCase =
    {
        Name : string
        Member : Member<MethodDefinition>
    }

/// Represents properties.
type Property =
    {
        Member : Member<PropertyDefinition>
        Getter : option<Member<MethodDefinition>>
        Setter : option<Member<MethodDefinition>>
    }

/// Represents a reflected type.
type Type =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : TypeDefinition
        Kind : Kind
        Location : Location
        Methods : list<Member<MethodDefinition>>
        Nested : list<Type>
        Properties : list<Property>
        Fields : list<Member<FieldDefinition>>
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
val Reflect : Logger -> AssemblyDefinition -> Assembly

/// A utility class for pooling loaded macro definitions.
[<Sealed>]
type Pool =

    /// Loads a macro definition from a given type.
    member LoadMacro : R.Type -> M.IMacro

    /// Loads a generator definition from a given type.
    member LoadGenerator : R.Type -> M.IGenerator

    /// Constructs a macro pool.
    static member Create : Logger -> Pool
