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

module IntelliFactory.JavaScript.Syntax

type Id = string
type Label = string
type Regex = string

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

type PostfixOperator =
    | ``++`` = 0
    | ``--`` = 1

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

type B = BinaryOperator

type Literal =
    | False
    | Null
    | Number of string
    | String of string
    | True

    static member ( !~ ) lit = Constant lit

    override this.ToString() =
        match this with
        | False -> "false"
        | Null -> "null"
        | Number x -> x
        | String x -> System.String.Format("\"{0}\"", x)
        | True -> "true"

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

and SwitchElement =
    | Case of E * list<S>
    | Default of list<S>

and E = Expression
and S = Statement

and ProgramElement =
    | Action of S
    | Function of Id * list<Id> * list<ProgramElement>

type Program = list<ProgramElement>

let TransformExpression (!) (!^) expr =
    let (!!) = List.map (!)
    let rec tE = function Action s -> Action (!^ s)
                        | Function (n, f, b) -> Function (n, f, tB b)
    and tB x = List.map tE x
    match expr with
    | Application (x, xs) -> Application (!x, !!xs)
    | Binary (x, o, y) -> Binary (!x, o, !y)
    | Conditional (x, y, z) -> Conditional (!x, !y, !z)
    | Lambda (name, vars, body) -> Lambda (name, vars, tB body)
    | New (x, xs) -> New (!x, !!xs)
    | NewArray xs -> NewArray (List.map (Option.map (!)) xs)
    | NewObject xs -> NewObject [for (k,v) in xs -> (k,!v)]
    | Postfix (x, o) -> Postfix (!x, o)
    | Unary (o, x) -> Unary (o, !x)
    | Constant _
    | NewRegex _
    | This
    | Var _ -> expr

let TransformStatement (!) (!^) stmt =
    let (!?) = Option.map (!)
    let (!^+) = List.map (!^)
    match stmt with
    | Block x -> Block (List.map (!^) x)
    | Do (s, e) -> Do (!^s, !e)
    | For (a, b, c, d) -> For (!?a, !?b, !?c, !^d)
    | ForIn (a, b, c) -> ForIn (!a, !b, !^c)
    | ForVarIn (a, b, c, d) -> ForVarIn (a, !?b, !c, !^d)
    | ForVars (a, b, c, d) ->
        let a = [for (k, v) in a -> (k, !?v)]
        ForVars (a, !?b, !?c, !^d)
    | If (x, y, z) -> If (!x, !^y, !^z)
    | Ignore x -> Ignore !x
    | Labelled (x, y) -> Labelled (x, !^y)
    | Return x -> Return !?x
    | Switch (x, y) ->
        let f = function
            | Case (x, s) -> Case (!x, !^+s)
            | Default s -> Default !^+s
        Switch (!x, List.map f y)
    | Throw x -> Throw !x
    | TryFinally (x, y) -> TryFinally (!^x, !^y)
    | TryWith (a, b, c, d) -> TryWith (!^a, b, !^c, Option.map (!^) d)
    | Vars vs -> Vars [for (k, v) in vs -> (k, !?v)]
    | While (x, y) -> While (!x, !^y)
    | With (x, y) -> With (!x, !^y)
    | Break _
    | Continue _
    | Debugger
    | Empty -> stmt

let Fold t fE fS init x =
    let state = ref init
    let tE x = state := fE !state x; x
    let tS x = state := fS !state x; x
    ignore (t tE tS x)
    !state

let FoldExpression fE fS init expr =
    Fold TransformExpression fE fS init expr

let FoldStatement fE fS init stmt =
    Fold TransformStatement fE fS init stmt

/// Gets all locally scoped variables.
let getLocals (body: list<ProgramElement>) acc =
    let rec elem acc e =
        match e with
        | Action s -> stmt acc s
        | Function (id, _, _) -> Set.add id acc
    and stmt acc s =
        match s with
        | ForVars (v, _, _, _)
        | Vars v ->
            let f vs (v, _) = Set.add v vs
            List.fold f acc v
        | ForVarIn (id, _, _, _) -> Set.add id acc
        | Labelled (id, s) -> stmt (Set.add id acc) s
        | _ -> FoldStatement (fun x _ -> x) stmt acc s
    List.fold elem acc body

/// Closes an expression by rewiring global variables to be
/// member accesses on a global object.
let Close (glob: Id) (expr: E) =
    let rec tE bound expr =
        match expr with
        | Var id ->
            if Set.contains id bound then expr else
                (?) (Var glob) id
        | Lambda (name, vars, body) ->
            let bound =
                getLocals body bound
                + Set.ofList (Option.toList name @ vars)
            Lambda (name, vars, tP bound body)
        | _ -> TransformExpression (tE bound) (tS bound) expr
    and tPE bound e =
        match e with
        | Function (name, vars, body) ->
            let bound = getLocals body bound + Set.ofList vars
            Function (name, vars, tP bound body)
        | Action s -> Action (tS bound s)
    and tP scope prog = List.map (tPE scope) prog
    and tS scope stmt = TransformStatement (tE scope) (tS scope) stmt
    tE Set.empty expr

let Walk t wE wS mk e =
    let r = ref []
    let tr f v =
        let (x, n) = f v
        r := x :: !r
        n
    let next = t (tr wE) (tr wS) e
    (mk (List.rev !r), next)

let WalkExpression (wE: E -> 'T * E) (wS: S -> 'T * S) mk e : 'T * E =
    Walk TransformExpression wE wS mk e

let WalkStatement (wE: E -> 'T * E) (wS: S -> 'T * S) mk s : 'T * S =
    Walk TransformStatement wE wS mk s

let Optimize (expr: E) =
    let rec removeVars free vars =
        match vars with
        | [] -> []
        | var :: rest ->
            if Set.contains var free
            then vars
            else removeVars free rest
    let rec tE (expr: E) : Set<Id> * E =
        match expr with
        | Lambda (name, vars, body) ->
            let (free, newBody) = tP body
            let fv = Option.toList name @ vars
            let newFree = Set.difference free (Set.ofList fv)
            let newVars = List.rev (removeVars free (List.rev vars))
            (newFree, Lambda (name, newVars, newBody))
        | Var x ->
            (Set.singleton x, expr)
        | _ ->
            WalkExpression tE tS Set.unionMany expr
    and tPE e : Set<Id> * ProgramElement =
        match e with
        | Function (name, vars, body) ->
            let (free, newBody) = tP body
            let newFree = Set.difference free (Set.ofList vars)
            let newVars = List.rev (removeVars free (List.rev vars))
            (newFree, Function (name, newVars, newBody))
        | Action s ->
            let (free, newS) = WalkStatement tE tS Set.unionMany s
            (free, Action newS)
    and tP prog : Set<Id> * Program =
        let (sets, prog) = List.unzip (List.map tPE prog)
        (Set.unionMany sets, prog)
    and tS stmt : Set<Id> * Statement =
        WalkStatement tE tS Set.unionMany stmt
    snd (tE expr)
