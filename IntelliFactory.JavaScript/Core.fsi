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

/// Defines a reduced language that is suitable
/// for simple optimization, and efficient compilation to JavaScript.
///
/// The goal of Core is to define a JavaScript-like
/// language satisfying the following criteria:
///
/// (1) Has tractable semantics to simplify optimization.
///
/// (2) Is rich enough to express JavaScript efficiently.
///
/// (3) Can be read from a JavaScript subset and elaborated to
///     JavaScript with minimal loss of structure.
///
/// Expanding all JavaScript programs into Core
/// has not been a design goal, therefore certain constructs, such as
/// mutable variables and break statements have been removed for
/// simplicity.
module IntelliFactory.JavaScript.Core

/// Represents unary operators. These are JavaScript unary operators
/// excluding increment, decrement and delete.
type UnaryOperator =
    | ``~`` = 0
    | ``-`` = 1
    | ``!`` = 2
    | ``+`` = 3
    | ``typeof`` = 4
    | ``void`` = 5

/// Represents binary operators. These are JavaScript binary operators
/// excluding assignment, sequence and member operators.
type BinaryOperator =
    | ``!==`` = 0
    | ``!=`` = 1
    | ``%`` = 2
    | ``&&`` = 3
    | ``&`` = 4
    | ``*`` = 5
    | ``+`` = 6
    | ``-`` = 7
    | ``/`` = 8
    | ``<<`` = 9
    | ``<=`` = 10
    | ``<`` = 11
    | ``===`` = 12
    | ``==`` = 13
    | ``>=`` = 14
    | ``>>>`` = 15
    | ``>>`` = 16
    | ``>`` = 17
    | ``^`` = 18
    | ``in`` = 19
    | ``instanceof`` = 20
    | ``|`` = 21
    | ``||`` = 22

/// Represents identifiers.  Reference equality is used in
/// representing bindings.  Names can be optionally provided, their
/// role is advisory.
[<Sealed>]
type Id =

    /// Constructs a new identifier.
    new : unit -> Id

    /// Constructs a new identifier with a given readable name hint.
    new : string -> Id

    /// Returns the readable name hint of the identifier, if provided.
    member Name : option<string> with get, set

    /// Satisfies the comparison constraint.
    interface System.IComparable

/// Represents self-evaluating literals.
type Literal =
    | Double of double
    | False
    | Integer of int64
    | Null
    | String of string
    | True
    | Undefined

    /// Lifts to an expression.
    static member ( !~ ) : Literal -> Expression

/// Represents expressions of the language.
and Expression =
    | Application of E * list<E>
    | Binary of E * BinaryOperator * E
    | Call of E * E * list<E>
    | Constant of Literal
    | FieldDelete of E * E
    | FieldGet of E * E
    | FieldSet of E * E * E
    | ForEachField of Id * E * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Global of list<string>
    | IfThenElse of E * E * E
    | Lambda of option<Id> * list<Id> * E
    | Let of Id * E * E
    | LetRecursive of list<Id * E> * E
    | New of E * list<E>
    | NewArray of list<E>
    | NewObject of list<string * E>
    | NewRegex of string
    | Runtime
    | Sequential of E * E
    | Throw of E
    | TryFinally of E * E
    | TryWith of E * Id * E
    | Unary of UnaryOperator * E
    | Var of Id
    | WhileLoop of E * E

    static member ( + ) : E * E -> E
    static member ( - ) : E * E -> E
    static member ( * ) : E * E -> E
    static member ( / ) : E * E -> E
    static member ( % ) : E * E -> E
    static member ( &== ) : E * E -> E
    static member ( &!= ) : E * E -> E
    static member ( &=== ) : E * E -> E
    static member ( &!== ) : E * E -> E
    static member ( &< ) : E * E -> E
    static member ( &> ) : E * E -> E
    static member ( &<= ) : E * E -> E
    static member ( &>= ) : E * E -> E
    static member ( ? ) : E * string -> E
    static member ( ! ) : E -> E
    static member ( ~+ ) : E -> E
    static member ( ~- ) : E -> E

    member Void : E
    member TypeOf : E
    member In : E -> E
    member InstanceOf : E -> E

    member Item : E -> E        with get
    member Item : list<E> -> E  with get

and private E = Expression

/// Maps over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda is the only variable-binding form.
val Transform : (E -> E) -> E -> E

/// Folds over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda is the only variable-binding form.
val Fold<'T> : ('T -> E -> 'T) -> 'T -> E -> 'T

/// Checks that no identifier is bound twice in an expression.
val IsAlphaNormalized : E -> bool

/// Refreshes bound identifiers, so that no identifier is bound twice.
val AlphaNormalize : E -> E

/// Finds all free variables in an expression.
val GetFreeIds : E -> Set<Id>

/// Tests if an expression is ground (contains no free
/// identifiers).
val IsGround : E -> bool

/// Replaces free variables in an expression.
val Substitute : (Id -> option<E>) -> E -> E

/// Performs an optimization pass. Current optimizations include
/// local tail-call elimination that transforms LetRecursive nodes
/// to loops.  The complexity is bounded by O(n) in
/// the number of sub-expressions.
val Optimize : E -> E

/// Elaborates an expression to a JavaScript program.
val ToProgram : Preferences -> E -> Syntax.Program

/// Recognizes expressions from a subset of syntactic forms.
val Recognize : Syntax.Expression -> option<E>

val internal GlobalName : Preferences -> string
val internal RuntimeName : Preferences -> string
