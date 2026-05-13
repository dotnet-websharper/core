// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
    | ``**`` = 37
    | ``??`` = 38
    | ``??=`` = 39

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
        Rest : bool
        IsPrivate : bool
        IsTypeName : bool
        Type : option<E>
        Generics : list<Id>
    }

    member this.ToNonTyped() =
        match this.Type with
        | None -> this
        | _ -> { this with Type = None }

    member this.WithType(t) =
        { this with Type = Some t }

    member this.WithGenerics(g) =
        match g with
        | [] -> this 
        | _ -> { this with Generics = g }

    static member New(name, ?opt, ?rest, ?typ, ?gen, ?typn, ?priv) =
        {
            Name = name
            Optional = defaultArg opt false
            Rest = defaultArg rest false
            IsPrivate = defaultArg priv false
            IsTypeName = defaultArg typn false 
            Type = typ
            Generics = defaultArg gen []
        }

and Modifiers =
    | None = 0
    | Private = 1
    | Public = 2
    | ReadOnly = 4

and DeclKind =
    | VarDecl
    | ConstDecl
    | LetDecl

and Accessor =
    | Get
    | Set
    | Simple

/// Represents JavaScript expressions.
and Expression =
    | Application of func: E * generics: list<Id> * args: list<E>
    | Binary      of left: E * op: BinaryOperator * right: E
    | Conditional of test: E * consequent: E * alternate: E
    | Constant    of lit: Literal
    | Lambda      of name: option<Id> * formals: list<Id> * body: list<S> * isArrow: bool
    | New         of callee: E * generics: list<Id> * args: list<E>
    | NewArray    of elements: list<option<E>>
    | NewObject   of fields: list<string * Accessor * E>
    | NewRegex    of Regex
    | Postfix     of expr: E * op: PostfixOperator
    | This
    | Super
    | Unary       of op: UnaryOperator * expr: E
    | Var         of id: Id
    | VarNamed    of id: Id * name: string
    | Cast        of target: Expression * expr: Expression
    | ExprPos     of expr: Expression * pos: SourcePos
    | ExprComment of expr: E * comment: string
    | ImportFunc
    | ClassExpr   of name: option<Id> * baseAndGenerics: option<E * list<Id>> * args: list<E> * members: list<Member>
    | Verbatim    of tokens: list<string> * exprs: list<E>

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
    member this.Item with get xs = Application (this, [], xs)

    static member ( ? ) (e: E, msg: string) =
        Binary (e, B.``.``, Constant (String msg))

/// JavaScript statements.
and Statement =
    | Block            of statements: list<S>
    | Break            of label: option<Label>
    | Continue         of label: option<Label>
    | Debugger     
    | Do               of body: S * expr: E
    | Empty        
    | For              of init: option<E> * cond: option<E> * step: option<E> * body: S
    | ForIn            of lhs: E * rhs: E * body: S
    | ForVarIn         of id: Id * value: option<E> * expr: E * body: S
    | ForVars          of vars: list<Id * option<E>> * cond: option<E> * step: option<E> * body: S
    | If               of test: E * thenBranch: S * elseBranch: S
    | Ignore           of expr: E
    | Labelled         of label: Label * statement: S
    | Return           of expr: option<E>
    | Switch           of expr: E * cases: list<SwitchElement>
    | Throw            of expr: E
    | TryFinally       of tryBlock: S * finallyBlock: S
    | TryWith          of tryBlock: S * id: Id * catchBlock: S * finallyOpt: option<S>
    | Vars             of vars: list<Id * option<E>> * kind: DeclKind
    | While            of test: E * body: S
    | With             of expr: E * body: S
    | Function         of id: Id * args: list<Id> * bodyOpt: option<list<S>>
    | Export           of isDefault: bool * decl: S 
    | ExportAlias      of fromId: Id * toId: Id 
    | Import           of a: option<Id> * b: option<Id> * items: list<string  * Id> * path: string
    | ImportAll        of id: option<Id> * path: string
    | ImportAlias      of id: Id * expr: E
    | TypeAlias        of id: Id * expr: E
    | Declare          of decl: S
    | DeclareGlobal    of items: list<S>
    | Namespace        of id: Id * body: list<S>
    | Class            of id: Id * isExport: bool * baseAndGenerics: option<E * list<Id>> * implements: list<E> * members: list<Member>
    | Interface        of id: Id * generics: list<E> * members: list<Member>
    | StatementPos     of stmt: S * pos: SourcePos
    | StatementComment of stmt: S * comment: string

and Member =
    | Method      of isStatic:bool * isAbstract:bool * accessor:Accessor * Id * list<Id> * option<list<S>>
    | Constructor of args: list<Id * Modifiers> * body: option<list<S>>
    | Property    of isStatic:bool * Id * option<E>
    | Static      of body: list<S>

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
