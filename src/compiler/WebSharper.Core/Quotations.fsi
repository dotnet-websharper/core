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

module WebSharper.Core.Quotations

module R = WebSharper.Core.Reflection
module S = WebSharper.Core.JavaScript.Syntax

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
    private
    | AddressOf           of E
    | AddressSet          of E * E
    | Application         of E * E
    | Call                of Concrete<R.Method> * list<E>
    | CallModule          of Concrete<R.Method> * list<E>
    | Coerce              of R.Type * E
    | DefaultValue        of R.Type
    | FieldGetInstance    of E * Concrete<R.Field>
    | FieldGetRecord      of E * Concrete<R.Property>
    | FieldGetStatic      of Concrete<R.Field>
    | FieldGetUnion       of E * Concrete<R.UnionCase> * int
    | FieldSetRecord      of E * Concrete<R.Property> * E
    | FieldSetInstance    of E * Concrete<R.Field> * E
    | FieldSetStatic      of Concrete<R.Field> * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Hole                of R.Type * int
    | IfThenElse          of E * E * E
    | Lambda              of Id * E
    | Let                 of Id * E * E
    | LetRecursive        of list<Id * E> * E
    | NewArray            of R.Type * list<E>
    | NewDelegate         of R.Type * E
    | NewObject           of Concrete<R.Constructor> * list<E>
    | NewRecord           of R.Type * list<E>
    | NewTuple            of list<E>
    | NewUnionCase        of Concrete<R.UnionCase> * list<E>
    | PropertyGet         of Concrete<R.Property> * list<E>
    | PropertySet         of Concrete<R.Property> * list<E>
    | Quote               of E
    | Sequential          of E * E
    | TupleGet            of int * E
    | TryFinally          of E * E
    | TryWith             of E * Id * E * Id * E
    | TypeTest            of R.Type * E
    | UnionCaseTest       of Concrete<R.UnionCase> * E
    | Value               of Literal
    | Var                 of Id
    | VarSet              of Id * E
    | WhileLoop           of E * E
    | SourcePos           of E * S.SourcePos
    | NoMacro             of E

and private E = Expression

val (|AddressOf          |_|) : E -> (E                                ) option
val (|AddressSet         |_|) : E -> (E * E                            ) option
val (|Application        |_|) : E -> (E * E                            ) option
val (|Call               |_|) : E -> (Concrete<R.Method> * list<E>     ) option
val (|CallModule         |_|) : E -> (Concrete<R.Method> * list<E>     ) option
val (|CallOrCallModule   |_|) : E -> (Concrete<R.Method> * list<E>     ) option
val (|Coerce             |_|) : E -> (R.Type * E                       ) option
val (|DefaultValue       |_|) : E -> (R.Type                           ) option
val (|FieldGetInstance   |_|) : E -> (E * Concrete<R.Field>            ) option
val (|FieldGetRecord     |_|) : E -> (E * Concrete<R.Property>         ) option
val (|FieldGetStatic     |_|) : E -> (Concrete<R.Field>                ) option
val (|FieldGetUnion      |_|) : E -> (E * Concrete<R.UnionCase> * int  ) option
val (|FieldSetRecord     |_|) : E -> (E * Concrete<R.Property> * E     ) option
val (|FieldSetInstance   |_|) : E -> (E * Concrete<R.Field> * E        ) option
val (|FieldSetStatic     |_|) : E -> (Concrete<R.Field> * E            ) option
val (|ForIntegerRangeLoop|_|) : E -> (Id * E * E * E                   ) option
val (|Hole               |_|) : E -> (R.Type * int                     ) option
val (|IfThenElse         |_|) : E -> (E * E * E                        ) option
val (|Lambda             |_|) : E -> (Id * E                           ) option
val (|Let                |_|) : E -> (Id * E * E                       ) option
val (|LetRecursive       |_|) : E -> (list<Id * E> * E                 ) option
val (|NewArray           |_|) : E -> (R.Type * list<E>                 ) option
val (|NewDelegate        |_|) : E -> (R.Type * E                       ) option
val (|NewObject          |_|) : E -> (Concrete<R.Constructor> * list<E>) option
val (|NewRecord          |_|) : E -> (R.Type * list<E>                 ) option
val (|NewTuple           |_|) : E -> (list<E>                          ) option
val (|NewUnionCase       |_|) : E -> (Concrete<R.UnionCase> * list<E>  ) option
val (|PropertyGet        |_|) : E -> (Concrete<R.Property> * list<E>   ) option
val (|PropertySet        |_|) : E -> (Concrete<R.Property> * list<E>   ) option
val (|Quote              |_|) : E -> (E                                ) option
val (|Sequential         |_|) : E -> (E * E                            ) option
val (|TupleGet           |_|) : E -> (int * E                          ) option
val (|TryFinally         |_|) : E -> (E * E                            ) option
val (|TryWith            |_|) : E -> (E * Id * E * Id * E              ) option
val (|TypeTest           |_|) : E -> (R.Type * E                       ) option
val (|UnionCaseTest      |_|) : E -> (Concrete<R.UnionCase> * E        ) option
val (|Value              |_|) : E -> (Literal                          ) option
val (|Var                |_|) : E -> (Id                               ) option
val (|VarSet             |_|) : E -> (Id * E                           ) option
val (|WhileLoop          |_|) : E -> (E * E                            ) option
val (|SourcePos          |_|) : E -> (E * S.SourcePos                  ) option
val (|NoMacro            |_|) : E -> (E                                ) option

val AddressOf           : E                                 -> E
val AddressSet          : E * E                             -> E
val Application         : E * E                             -> E
val Call                : Concrete<R.Method> * list<E>      -> E
val CallModule          : Concrete<R.Method> * list<E>      -> E
val Coerce              : R.Type * E                        -> E
val DefaultValue        : R.Type                            -> E
val FieldGetInstance    : E * Concrete<R.Field>             -> E
val FieldGetRecord      : E * Concrete<R.Property>          -> E
val FieldGetStatic      : Concrete<R.Field>                 -> E
val FieldGetUnion       : E * Concrete<R.UnionCase> * int   -> E
val FieldSetRecord      : E * Concrete<R.Property> * E      -> E
val FieldSetInstance    : E * Concrete<R.Field> * E         -> E
val FieldSetStatic      : Concrete<R.Field> * E             -> E
val ForIntegerRangeLoop : Id * E * E * E                    -> E
val Hole                : R.Type * int                      -> E
val IfThenElse          : E * E * E                         -> E
val Lambda              : Id * E                            -> E
val Let                 : Id * E * E                        -> E
val LetRecursive        : list<Id * E> * E                  -> E
val NewArray            : R.Type * list<E>                  -> E
val NewDelegate         : R.Type * E                        -> E
val NewObject           : Concrete<R.Constructor> * list<E> -> E
val NewRecord           : R.Type * list<E>                  -> E
val NewTuple            : list<E>                           -> E
val NewUnionCase        : Concrete<R.UnionCase> * list<E>   -> E
val PropertyGet         : Concrete<R.Property> * list<E>    -> E
val PropertySet         : Concrete<R.Property> * list<E>    -> E
val Quote               : E                                 -> E
val Sequential          : E * E                             -> E
val TupleGet            : int * E                           -> E
val TryFinally          : E * E                             -> E
val TryWith             : E * Id * E * Id * E               -> E
val TypeTest            : R.Type * E                        -> E
val UnionCaseTest       : Concrete<R.UnionCase> * E         -> E
val Value               : Literal                           -> E
val Var                 : Id                                -> E
val VarSet              : Id * E                            -> E
val WhileLoop           : E * E                             -> E
val NoMacro             : E                                 -> E

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
