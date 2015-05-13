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

/// Provides an alternative for System.Reflection to deal with
/// assembly metadata. In particular, the representations
/// are lightweight and can be serialized.
module WebSharper.Core.Reflection

/// Represents the generics count.
type Count = int

/// Represents assembly names, for example FSharp.Core.
[<Sealed>]
type AssemblyName =

    /// Simple name, for example FSharp.Core.
    member Name : string

    /// Full name including the version and public key token.
    member FullName : string

    /// Parses a new assembly name.
    static member Parse : string -> AssemblyName

    /// Converts a System.Reflection representation to an AssemblyName.
    static member Convert : System.Reflection.AssemblyName -> AssemblyName

    /// Retrieves the name of the given assembly.
    static member FromAssembly : System.Reflection.Assembly -> AssemblyName

/// Represents type names, for example Microsoft.FSharp.Core.FSharpFunc`2.
type FullName = string

/// Represents namespaces, for example Microsoft.FSharp.
type Namespace = string

/// Represents loader-friendly names, for example
/// Microsoft.FSharp.Core.FSharpFunc`2, FSharp.Core.
type AssemblyQualifiedName = string

/// Represents simple names.
type Name = string

/// Represents positions within a sequence.
type Position = int

/// Represents array ranks.
type Rank = int

/// Thrown when the reflected type is not valid for a given operation,
/// for example when it is a generic parameter and this is disallowed.
exception InvalidType of message: string

/// Represents system and user-defined type definitions.
[<Sealed>]
type TypeDefinition =

//    interface System.IComparable

    /// The name of the declaring assembly.
    member AssemblyName : AssemblyName

    /// The loader-friendly name.
    member AssemblyQualifiedName : AssemblyQualifiedName

    /// The declaring type, if any.
    member DeclaringType : option<TypeDefinition>

    /// The full name.
    member FullName : FullName

    /// The name, such as Queue`1.
    member Name : Name

    /// The namespace, if any.
    member Namespace : option<Namespace>

    /// Tries to load the type. Throws exceptions on failure.
    member Load : unit -> System.Type

    /// Parses type definitions. Throws exceptions on failure.
    static member FromType : System.Type -> TypeDefinition

    /// Parses a name.
    static member Parse : AssemblyQualifiedName -> TypeDefinition

    /// Creates a top-level type definition.
    static member Create :
        AssemblyName -> Namespace -> Name -> TypeDefinition

    /// Creates a nested type definition.
    static member CreateNested :
        TypeDefinition -> Name -> TypeDefinition

    /// The count of generic arguments.
    member GenericsCount : int

/// Represents method parameter types.
[<Sealed>]
type Type =

    /// The definition.
    member DeclaringType : TypeDefinition

    /// The loader-friendly name.
    member AssemblyQualifiedName : AssemblyQualifiedName

    /// The full name.
    member FullName : FullName

    /// The name, such as Queue`1.
    member Name : Name

    /// Tries to load the class. Throws exceptions on failure.
    member Load : ?allowGeneric: bool -> System.Type

    /// Parses types. Throws exceptions on failure.
    static member FromType : System.Type -> Type

/// Provides utilities for working with Type values.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =

    /// Constructs an array type.
    val Array : Type * Rank -> Type

    /// Constructs a type by specializing a type definition.
    val Concrete : TypeDefinition * list<Type> -> Type

    /// Constructs a generic parameter type.
    val Generic : Position -> Type

    /// Matches type kinds.
    val (|Array|Concrete|Generic|) :
        Type -> Choice<Type*Rank,TypeDefinition*list<Type>,Position>

/// Represents generic arguments.
type Generics = list<Type>

/// Represents method signatures as parameter types.
type Signature = list<Type>

/// Represents method definitions.
[<Sealed>]
type Method =

//    interface System.IComparable

    /// The declaring type.
    member DeclaringType : TypeDefinition

    /// Updates the declaring type.
    member WithDeclaringType : TypeDefinition -> Method

    /// The name.
    member Name : Name

    /// Tries to load the method definition. Throws exceptions on failure.
    member Load : option<Generics> -> System.Reflection.MethodInfo

    /// Parses the method.
    static member Parse : System.Reflection.MethodInfo -> Method

    /// Constructs a new method with an explicit signature.
    static member Create :
        TypeDefinition -> Name -> Count -> Signature -> Type -> Method

    /// Constructs a new method ignoring the signature.
    static member internal CreateReference :
        TypeDefinition -> Name -> Method

/// Represents instance constructors.
[<Sealed>]
type Constructor =

    /// The declaring type.
    member DeclaringType : TypeDefinition

    /// Updates the declaring type.
    member WithDeclaringType : TypeDefinition -> Constructor

    /// Constructs a new Constructor with an explicit signature.
    static member Create : TypeDefinition -> Signature -> Constructor

//    interface System.IComparable

/// Represents property definitions.
[<Sealed>]
type Property =

    /// The declaring type.
    member DeclaringType : TypeDefinition

    /// Updates the declaring type.
    member WithDeclaringType : TypeDefinition -> Property

    /// The name.
    member Name : Name

    /// Parses the property.
    static member Parse : System.Reflection.PropertyInfo -> Property

    /// Constructs a new property with an explicit type and signature.
    static member Create :
        TypeDefinition -> Name -> Type -> Signature -> Property

    /// Constructs a new property ignoring the type.
    static member internal CreateReference :
        TypeDefinition -> Name -> Property

//    interface System.IComparable

/// Represents field definitions.
[<Sealed>]
type Field =

    /// The declaring type.
    member DeclaringType : TypeDefinition

    /// Updates the declaring type.
    member WithDeclaringType : TypeDefinition -> Field

    /// The name.
    member Name : Name

    /// Constructs a new field.
    static member Create : TypeDefinition -> Name -> Field

/// Represents field definitions.
[<Sealed>]
type UnionCase =

    /// The declaring type.
    member DeclaringType : TypeDefinition

    /// Updates the declaring type.
    member WithDeclaringType : TypeDefinition -> UnionCase

    /// The name.
    member Name : Name

    /// Constructs a new UnionCase.
    static member Create : TypeDefinition -> Name -> UnionCase
