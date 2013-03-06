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

/// Defines the JavaScript abstract syntax tree.
module IntelliFactory.JavaScript.Syntax

/// Represents JavaScript identifiers.
type Id = string

/// Represents JavaScript labels.
type Label = string

/// Represents JavaScript regular expression literals verbatim.
type Regex = string

/// Represents JavaScript prefix operators.
type UnaryOperator =
    | ``!`` = 0
    | ``++`` = 1
    | ``+`` = 2
    | ``--`` = 3
    | ``-`` = 4
    | ``delete`` = 5
    | ``typeof`` = 6
    | ``void`` = 7
    | ``~`` = 8

/// Represents JavaScript postfix operators.
type PostfixOperator =
    | ``++`` = 0
    | ``--`` = 1

/// Represents JavaScript binary operators.
type BinaryOperator =
    | ``!==`` = 0
    | ``!=`` = 1
    | ``%=`` = 2
    | ``%`` = 3
    | ``&&`` = 4
    | ``&=`` = 5
    | ``&`` = 6
    | ``*=`` = 7
    | ``*`` = 8
    | ``+=`` = 9
    | ``+`` = 10
    | ``,`` = 11
    | ``-=`` = 12
    | ``-`` = 13
    | ``.`` = 14
    | ``/=`` = 15
    | ``/`` = 16
    | ``<<=`` = 17
    | ``<<`` = 18
    | ``<=`` = 19
    | ``<`` = 20
    | ``===`` = 21
    | ``==`` = 22
    | ``=`` = 23
    | ``>=`` = 24
    | ``>>=`` = 25
    | ``>>>=`` = 26
    | ``>>>`` = 27
    | ``>>`` = 28
    | ``>`` = 29
    | ``^=`` = 30
    | ``^`` = 31
    | ``in`` = 32
    | ``instanceof`` = 33
    | ``|=`` = 34
    | ``|`` = 35
    | ``||`` = 36

/// Represents literals.
type Literal =
    | False
    | Null
    | Number of string
    | String of string
    | True

    /// Lifts to an expression.
    static member ( !~ ) : Literal -> Expression

/// Represents JavaScript expressions.
and Expression =
    | Application of E * list<E>
    | Binary of E * BinaryOperator * E
    | Conditional of E * E * E
    | Constant of Literal
    | Lambda of option<Id> * list<Id> * list<ProgramElement>
    | New of E * list<E>
    | NewArray of list<option<E>>
    | NewObject of list<Id * E>
    | NewRegex of Regex
    | Postfix of E * PostfixOperator
    | This
    | Unary of UnaryOperator * E
    | Var of Id

    static member ( + ) : E * E -> E
    static member ( - ) : E * E -> E
    static member ( * ) : E * E -> E
    static member ( / ) : E * E -> E
    static member ( % ) : E * E -> E
    static member ( ^= ) : E * E -> E
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

    member Delete : E
    member Void : E
    member TypeOf : E
    member In : E -> E
    member InstanceOf : E -> E

    member Item : E -> E with get
    member Item : list<E> -> E  with get

/// JavaScript statements.
and Statement =
    | Block of list<S>
    | Break of option<Label>
    | Continue of option<Label>
    | Debugger
    | Do of S * E
    | Empty
    | For of option<E> * option<E> * option<E> * S
    | ForIn of E * E * S
    | ForVarIn of Id * option<E> * E * S
    | ForVars of list<Id * option<E>> * option<E> * option<E> * S
    | If of E * S * S
    | Ignore of E
    | Labelled of Label * S
    | Return of option<E>
    | Switch of E * list<SwitchElement>
    | Throw of E
    | TryFinally of S * S
    | TryWith of S * Id * S * option<S>
    | Vars of list<Id * option<E>>
    | While of E * S
    | With of E * S

/// Represents switch elements.
and SwitchElement =
    | Case of E * list<S>
    | Default of list<S>

and private E = Expression
and private S = Statement

/// Represents program elements.
and ProgramElement =
    | Action of S
    | Function of Id * list<Id> * list<ProgramElement>

/// Represents complete programs.
type Program = list<ProgramElement>

/// Maps over the immediate sub-nodes of an expression.
val TransformExpression : (E -> E) -> (S -> S) -> E -> E

/// Maps over the immediate sub-nodes of a statement.
val TransformStatement : (E -> E) -> (S -> S) -> S -> S

/// Performs a fold over the immediate sub-nodes of an expression.
val FoldExpression<'T> : ('T -> E -> 'T) -> ('T -> S -> 'T) -> 'T -> E -> 'T

/// Performs a fold over the immediate sub-nodes of a statement.
val FoldStatement<'T> : ('T -> E -> 'T) -> ('T -> S -> 'T) -> 'T -> S -> 'T

/// Performs a variable renaming pass that closes the expression.
/// All global references are rewired to explicit field lookups on the
/// global object, represented by the given identifier.
val Close : Id -> E -> E

/// Performs simple optimizations.
val Optimize : E -> E
