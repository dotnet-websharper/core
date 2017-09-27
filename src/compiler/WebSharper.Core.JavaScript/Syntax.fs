// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Defines the JavaScript abstract syntax tree.
module WebSharper.Core.JavaScript.Syntax

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

type private B = BinaryOperator

type SourcePos =
    {
        File : string
        Line : int
        Column : int
        EndLine : int
        EndColumn : int
    }

/// Represents literals.
type Literal =
    | False
    | Null
    | Number of string
    | String of string
    | True

    /// Lifts to an expression.
    static member ( !~ ) lit = Constant lit

    override this.ToString() =
        match this with
        | False -> "false"
        | Null -> "null"
        | Number x -> x
        | String x -> System.String.Format("\"{0}\"", x)
        | True -> "true"

/// Represents JavaScript identifiers.
and Id =
    {
        Name : string
        Optional : bool
        Type : option<E>
    }

    member this.ToNonTyped() =
        match this.Type with
        | None -> this
        | _ -> { this with Type = None }

    member this.WithType(t) =
        { this with Type = Some t }

    static member New(name, ?opt, ?typ) =
        {
            Name = name
            Optional = defaultArg opt false
            Type = typ
        }

/// Represents JavaScript expressions.
and Expression =
    | Application of E * list<E>
    | Binary      of E * BinaryOperator * E
    | Conditional of E * E * E
    | Constant    of Literal
    | Lambda      of option<Id> * list<Id> * list<S>
    | New         of E * list<E>
    | NewArray    of list<option<E>>
    | NewObject   of list<string * E>
    | NewRegex    of Regex
    | Postfix     of E * PostfixOperator
    | This
    | Super
    | Unary       of UnaryOperator * E
    | Var         of Id
    | VarNamed    of Id * string
    | Cast        of Expression * Expression
    | ExprPos     of Expression * SourcePos
    | ExprComment of E * string

    static member ( + ) (a, b) = Binary (a, B.``+``, b)
    static member ( - ) (a, b) = Binary (a, B.``-``, b)
    static member ( * ) (a, b) = Binary (a, B.``*``, b)
    static member ( / ) (a, b) = Binary (a, B.``/``, b)
    static member ( % ) (a, b) = Binary (a, B.``%``, b)

    static member ( ^= )   (a, b) = Binary (a, B.``=``, b)
    static member ( &== )  (a, b) = Binary (a, B.``==``, b)
    static member ( &!= )  (a, b) = Binary (a, B.``!=``, b)
    static member ( &=== ) (a, b) = Binary (a, B.``===``, b)
    static member ( &!== ) (a, b) = Binary (a, B.``!==``, b)
    static member ( &< )   (a, b) = Binary (a, B.``<``, b)
    static member ( &> )   (a, b) = Binary (a, B.``>``, b)
    static member ( &<= )  (a, b) = Binary (a, B.``<=``, b)
    static member ( &>= )  (a, b) = Binary (a, B.``>=``, b)

    static member ( ! ) a = Unary (UnaryOperator.``!``, a)
    static member ( ~+ ) a = Unary (UnaryOperator.``+``, a)
    static member ( ~- ) a = Unary (UnaryOperator.``-``, a)

    member this.Delete = Unary (UnaryOperator.``delete``, this)
    member this.Void = Unary (UnaryOperator.``void``, this)
    member this.TypeOf = Unary (UnaryOperator.``typeof``, this)

    member this.In x = Binary (this, B.``in``, x)
    member this.InstanceOf x = Binary (this, B.``instanceof``, x)

    member this.Item with get (x: E) = Binary (this, B.``.``, x)
    member this.Item with get xs = Application (this, xs)

    static member ( ? ) (e: E, msg: string) =
        Binary (e, B.``.``, Constant (String msg))

/// JavaScript statements.
and Statement =
    | Block        of list<S>
    | Break        of option<Label>
    | Continue     of option<Label>
    | Debugger     
    | Do           of S * E
    | Empty        
    | For          of option<E> * option<E> * option<E> * S
    | ForIn        of E * E * S
    | ForVarIn     of Id * option<E> * E * S
    | ForVars      of list<Id * option<E>> * option<E> * option<E> * S
    | If           of E * S * S
    | Ignore       of E
    | Labelled     of Label * S
    | Return       of option<E>
    | Switch       of E * list<SwitchElement>
    | Throw        of E
    | TryFinally   of S * S
    | TryWith      of S * Id * S * option<S>
    | Vars         of list<Id * option<E>>
    | While        of E * S
    | With         of E * S
    | Function     of Id * list<Id> * list<S>
    | Export       of S
    | ImportAll    of option<Id> * string
    | Declare      of S
    | Namespace    of Id * list<S>
    | Class        of Id * bool * option<E> * list<E> * list<Member>
    | Interface    of Id * list<E> * list<Member>
    | StatementPos of S * SourcePos
    | StatementComment of S * string

and Member =
    | Method      of bool * Id * list<Id> * option<list<S>>
    | Constructor of list<Id> * option<list<S>>
    | Property    of bool * Id 

/// Represents switch elements.
and SwitchElement =
    | Case of E * list<S>
    | Default of list<S>

and private E = Expression
and private S = Statement

/// Represents complete programs.
type Program = list<S>
  
/// Remove source position wrapper.
let rec IgnoreExprPos e =
    match e with 
    | ExprPos(e, _) -> IgnoreExprPos e 
    | _ -> e

/// Remove source position wrapper.
let (|IgnoreEPos|) e = IgnoreExprPos e 

/// Remove source position wrapper.
let rec IgnoreStatementPos s =
    match s with 
    | StatementPos(s, _) -> IgnoreStatementPos s 
    | _ -> s

/// Remove source position wrapper.
let (|IgnoreSPos|) e = IgnoreStatementPos e 
