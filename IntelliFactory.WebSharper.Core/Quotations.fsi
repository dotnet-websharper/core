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

/// Parses F# quotations metadata associated with F#
/// assemblies, provides an alternative quotations representation.
///
/// F# reflected definitions are implemented by storing method
/// bodies as serialized quotations.  In F# this information can be
/// accessed by applying patterns from the
/// Quotations.DerivedPatterns module, such as the
/// MethodWithReflectedDefinition pattern.  The problem is that
/// these patterns are implemented in terms of
/// System.Reflection types and that practically assumes that
/// the runtime has loaded the assembly being analyzed.
///
/// This module allows to read the reflected definitions without
/// binding to the assembly that defines them.  It can improve
/// performance and stability of applications that need to analyze
/// reflected definitions.
///
/// This module works by reading the embedded resource F# dedicates
/// to store the quotation information.  The parser has been developed
/// by analyzing the F# compiler sources.

module IntelliFactory.WebSharper.Core.Quotations

module R = IntelliFactory.WebSharper.Core.Reflection

/// Thrown when the parser fails to recognize serialized metadata.
exception InvalidFormatException

/// Represents names.
type Name = string

/// Represents file-system paths.
type Path = string

/// Represents definitions.
type Definition =
    | ConstructorDefinition of R.Constructor
    | MethodDefinition of R.Method
    | PropertyDefinition of R.Property

/// Represents variables in quotations.
[<Sealed>]
type Id =

    /// An informative name.
    member Name : Name

    /// The type of the variable.
    member Type : R.Type

    /// A flag distinguishing mutable variables.
    member Mutable : bool

    /// Creates a simple Id.
    static member Create : Name -> R.Type -> Id

    /// Creates a mutable Id.
    static member CreateMutable : Name -> R.Type -> Id

    /// Satisfies the comparison constraint.
    interface System.IComparable

/// Represents generic instantiations.
type Concrete<'T> =
    {
        Entity : 'T
        Generics : list<R.Type>
    }

/// Represents various literal values.
type Literal =
    | Bool of bool
    | Byte of byte
    | Char of char
    | Double of double
    | Int of int
    | Int16 of int16
    | Int64 of int64
    | SByte of sbyte
    | Single of single
    | String of string
    | Unit
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64

/// Represents quotations.  For certain cases, such as method calls
/// and property accesses, static and instance variants are not
/// distinguished as that information is not present in quotation
/// metadata and can only be reconstructed by analyzing the referenced
/// members.  Property argument order, in particular, is not
/// sufficiently defined by this type.  For instance properties, the
/// argument order is target-value?-argument*.  For static properties
/// it is value?-argument*.
type Expression =
    | AddressOf of E
    | AddressSet of E * E
    | Application of E * E
    | Call of Concrete<R.Method> * list<E>
    | CallModule of Concrete<R.Method> * list<E>
    | Coerce of R.Type * E
    | DefaultValue of R.Type
    | FieldGetInstance of E * Concrete<R.Field>
    | FieldGetRecord of E * Concrete<R.Property>
    | FieldGetStatic of Concrete<R.Field>
    | FieldGetUnion of E * Concrete<R.UnionCase> * int
    | FieldSetRecord of E * Concrete<R.Property> * E
    | FieldSetInstance of E * Concrete<R.Field> * E
    | FieldSetStatic of Concrete<R.Field> * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Hole of R.Type * int
    | IfThenElse of E * E * E
    | Lambda of Id * E
    | Let of Id * E * E
    | LetRecursive of list<Id * E> * E
    | NewArray of R.Type * list<E>
    | NewDelegate of R.Type * E
    | NewObject of Concrete<R.Constructor> * list<E>
    | NewRecord of R.Type * list<E>
    | NewTuple of list<E>
    | NewUnionCase of Concrete<R.UnionCase> * list<E>
    | PropertyGet of Concrete<R.Property> * list<E>
    | PropertySet of Concrete<R.Property> * list<E>
    | Quote of E
    | Sequential of E * E
    | TupleGet of int * E
    | TryFinally of E * E
    | TryWith of E * Id * E * Id * E
    | TypeTest of R.Type * E
    | UnionCaseTest of Concrete<R.UnionCase> * E
    | Value of Literal
    | Var of Id
    | VarSet of Id * E
    | WhileLoop of E * E

and private E = Expression

/// Represents a set of reflected definitions.
type Definitions = list<Definition * Expression>

/// Parses reflected definitions from their binary representation.
val ReadStream :
    System.Reflection.AssemblyName -> System.IO.Stream -> Definitions

/// Parses reflected definitions associated with an assembly by
/// reading the dedicated embedded resource.  Fails when the assembly
/// was not produced with F#.
val ReadAssembly : System.Reflection.Assembly -> option<Definitions>

/// Loads an F# assembly from a given path with
/// ReflectionOnlyLoadFrom and reads its reflected definitions.
/// Fails when the assembly was not produced with F#.
val ReadAssemblyFile : Path -> option<Definitions>

/// Maps over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda and LetRecursive are the only variable-binding forms.
val Transform : (E -> E) -> E -> E

/// Folds over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda and LetRecursive are the only variable-binding form.
val Fold<'T> : ('T -> E -> 'T) -> 'T -> E -> 'T

/// Performs alpha-renaming of all variables.
val Alpha : E -> E
