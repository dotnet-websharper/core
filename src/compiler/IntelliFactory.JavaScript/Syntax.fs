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

type SourcePos =
    {
        Assembly : string
        File : string
        Line : int
        Column : int
    }

let X =
    Microsoft.FSharp.Quotations.Expr.AddressOf

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
    private
    | Application of E * list<E>
    | Binary      of E * BinaryOperator * E
    | Conditional of E * E * E
    | Constant    of Literal
    | Lambda      of option<Id> * list<Id> * list<ProgramElement>
    | New         of E * list<E>
    | NewArray    of list<option<E>>
    | NewObject   of list<Id * E>
    | NewRegex    of Regex
    | Postfix     of E * PostfixOperator
    | This
    | Unary       of UnaryOperator * E
    | Var         of Id
    | ExprPos     of Expression * SourcePos

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
    | Block      of list<S>
    | Break      of option<Label>
    | Continue   of option<Label>
    | Debugger
    | Do         of S * E
    | Empty
    | For        of option<E> * option<E> * option<E> * S
    | ForIn      of E * E * S
    | ForVarIn   of Id * option<E> * E * S
    | ForVars    of list<Id * option<E>> * option<E> * option<E> * S
    | If         of E * S * S
    | Ignore     of E
    | Labelled   of Label * S
    | Return     of option<E>
    | Switch     of E * list<SwitchElement>
    | Throw      of E
    | TryFinally of S * S
    | TryWith    of S * Id * S * option<S>
    | Vars       of list<Id * option<E>>
    | While      of E * S
    | With       of E * S

and SwitchElement =
    | Case of E * list<S>
    | Default of list<S>

and E = Expression
and S = Statement

and ProgramElement =
    | Action of S
    | Function of Id * list<Id> * list<ProgramElement>

let inline (|IgnorePos|) e =
    match e with ExprPos(e, _) | e -> e

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
    | ExprPos (x, pos) -> ExprPos (!x, pos)

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
let GetLocals (body: list<ProgramElement>) (bound: Set<Id>) =
    let res = ref bound
    let rec visitStmt s : unit =
        match s with
        | ForVars (vars, _, _, _) | Vars vars ->
            for (v, _) in vars do
                res := Set.add v !res
        | ForVarIn (v, _, _, _) ->
            res := Set.add v !res
        | _ -> ()
        FoldStatement (fun () _ -> ()) (fun () s -> visitStmt s) () s
    and visitElem e =
        match e with
        | Action s -> visitStmt s
        | Function (v, _, _) ->
            res := Set.add v !res
    List.iter visitElem body
    !res

/// Closes an expression by rewiring global variables to be
/// member accesses on a global object.
let Close (glob: Id) (expr: E) =
    let rec tE bound expr =
        match expr with
        | Var id ->
            if Set.contains id bound then expr else
                (?) (Var glob) id
        | Lambda (name, vars, body) ->
            let bound = GetLocals body (bound + Set.ofList (Option.toList name @ vars))
            Lambda (name, vars, tP bound body)
        | _ ->
            TransformExpression (tE bound) (tS bound) expr
    and tPE bound e =
        match e with
        | Function (name, vars, body) ->
            let bound = GetLocals body (Set.add name bound + Set.ofList vars)
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

let (|Application|_|) e = match e with IgnorePos (Application(x, y)   ) -> Some (x, y)    | _ -> None 
let (|Binary     |_|) e = match e with IgnorePos (Binary(x, y, z)     ) -> Some (x, y, z) | _ -> None 
let (|Conditional|_|) e = match e with IgnorePos (Conditional(x, y, z)) -> Some (x, y, z) | _ -> None 
let (|Constant   |_|) e = match e with IgnorePos (Constant x          ) -> Some x         | _ -> None 
let (|Lambda     |_|) e = match e with IgnorePos (Lambda(x, y, z)     ) -> Some (x, y, z) | _ -> None 
let (|New        |_|) e = match e with IgnorePos (New(x, y)           ) -> Some (x, y)    | _ -> None 
let (|NewArray   |_|) e = match e with IgnorePos (NewArray x          ) -> Some x         | _ -> None 
let (|NewObject  |_|) e = match e with IgnorePos (NewObject x         ) -> Some x         | _ -> None 
let (|NewRegex   |_|) e = match e with IgnorePos (NewRegex x          ) -> Some x         | _ -> None 
let (|Postfix    |_|) e = match e with IgnorePos (Postfix(x, y)       ) -> Some (x, y)    | _ -> None 
let (|This       |_|) e = match e with IgnorePos (This                ) -> Some ()        | _ -> None 
let (|Unary      |_|) e = match e with IgnorePos (Unary(x, y)         ) -> Some (x, y)    | _ -> None 
let (|Var        |_|) e = match e with IgnorePos (Var x               ) -> Some x         | _ -> None 

let (|ExprPos|_|) e = match e with ExprPos(x, y)  -> Some (x, y) | _ -> None 

let Application(x, y)    = Application(x, y)   
let Binary(x, y, z)      = Binary(x, y, z)     
let Conditional(x, y, z) = Conditional(x, y, z)
let Constant x           = Constant x          
let Lambda(x, y, z)      = Lambda(x, y, z)     
let New(x, y)            = New(x, y)           
let NewArray x           = NewArray x          
let NewObject x          = NewObject x         
let NewRegex x           = NewRegex x          
let Postfix(x, y)        = Postfix(x, y)       
let This                 = This                
let Unary(x, y)          = Unary(x, y)         
let Var x                = Var x               
let ExprPos(x, y)        = ExprPos(x, y)
