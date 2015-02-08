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

module IntelliFactory.JavaScript.Core

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open IntelliFactory.JavaScript
module S = Syntax

type SB = Syntax.BinaryOperator
type SP = Syntax.PostfixOperator
type SU = Syntax.UnaryOperator

[<Sealed>]
type Id(id: int64, name: string option, mut: bool) =
    static let root = obj ()
    static let mutable n = 0L
    static let next() = Interlocked.Increment(&n)
    let mutable name = name

    new () = new Id(next(), None, false)
    new (nameOpt) = new Id(next(), nameOpt, false)
    new (name) = new Id(next(), Some name, false)
    new (name, mut) = new Id(next(), Some name, mut)
    new (id: Id) = new Id(next (), id.Name, id.IsMutable)

    member this.Id = id

    member this.Name
        with get () = name
        and set x = name <- x

    member this.IsMutable = mut

    override this.GetHashCode() = int id

    override this.Equals obj =
        match obj with
        | :? Id as obj -> id = obj.Id
        | _ -> false

    interface System.IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Id as obj -> compare this.Id obj.Id
            | _ -> invalidArg "obj" "Invalid type for comparison."

    override this.ToString() =
        let n =
            match name with
            | None -> "id"
            | Some n -> n
        if this.IsMutable then
            System.String.Format("{0}#{1:x}!", n, id)
        else
            System.String.Format("{0}#{1:x}", n, id)

type UnaryOperator =
    | ``~`` = 0
    | ``-`` = 1
    | ``!`` = 2
    | ``+`` = 3
    | ``typeof`` = 4
    | ``void`` = 5

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

type B = BinaryOperator
type U = UnaryOperator

type Literal =
    | Double of double
    | False
    | Integer of int64
    | Null
    | String of string
    | True
    | Undefined

    override this.ToString() =
        match this with
        | Double x -> string x
        | False -> "false"
        | Integer x -> string x
        | Null -> "null"
        | String x -> "\"" + x + "\""
        | True -> "true"
        | Undefined -> "undefined"

    static member ( !~ ) x = Constant x
        
and Expression =
    private
    | Application         of E * list<E>
    | Arguments        
    | Binary              of E * BinaryOperator * E
    | Call                of E * E * list<E>
    | Constant            of Literal
    | FieldDelete         of E * E
    | FieldGet            of E * E
    | FieldSet            of E * E * E
    | ForEachField        of Id * E * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Global              of list<string>
    | IfThenElse          of E * E * E
    | Lambda              of option<Id> * list<Id> * E
    | Let                 of Id * E * E
    | LetRecursive        of list<Id * E> * E
    | New                 of E * list<E>
    | NewArray            of list<E>
    | NewObject           of list<string * E>
    | NewRegex            of string
    | Runtime
    | Sequential          of E * E
    | Throw               of E
    | TryFinally          of E * E
    | TryWith             of E * Id * E
    | Unary               of UnaryOperator * E
    | Var                 of Id
    | VarSet              of Id * E
    | WhileLoop           of E * E
    | SourcePos           of E * S.SourcePos

    static member ( + ) (a, b) = Binary (a, B.``+``, b)
    static member ( - ) (a, b) = Binary (a, B.``-``, b)
    static member ( * ) (a, b) = Binary (a, B.``*``, b)
    static member ( / ) (a, b) = Binary (a, B.``/``, b)
    static member ( % ) (a, b) = Binary (a, B.``%``, b)

    static member ( &== )  (a, b) = Binary (a, B.``==``, b)
    static member ( &!= )  (a, b) = Binary (a, B.``!=``, b)
    static member ( &=== ) (a, b) = Binary (a, B.``===``, b)
    static member ( &!== ) (a, b) = Binary (a, B.``!==``, b)
    static member ( &< )   (a, b) = Binary (a, B.``<``, b)
    static member ( &> )   (a, b) = Binary (a, B.``>``, b)
    static member ( &<= )  (a, b) = Binary (a, B.``<=``, b)
    static member ( &>= )  (a, b) = Binary (a, B.``>=``, b)
    static member ( &<< )  (a, b) = Binary (a, B.``<<``, b)
    static member ( &>> )  (a, b) = Binary (a, B.``>>``, b)

    static member ( !! ) a = Unary (UnaryOperator.``!``, a)
    static member ( ~+ ) a = Unary (UnaryOperator.``+``, a)
    static member ( ~- ) a = Unary (UnaryOperator.``-``, a)

    member this.Void = Unary (UnaryOperator.``void``, this)
    member this.TypeOf = Unary (UnaryOperator.``typeof``, this)

    member this.In x = Binary (this, B.``in``, x)
    member this.InstanceOf x = Binary (this, B.``instanceof``, x)

    member this.Item with get (x: E) = FieldGet (this, x)
    member this.Item with get xs = Application (this, xs)

    static member ( ? ) (e: E, msg: string) =
        FieldGet (e, Constant (String msg))

    /// use only for debugging
    override this.ToString() =
        let b = System.Text.StringBuilder()
        let p (x: string) = b.Append x |> ignore
        let pc (x: char) = b.Append x |> ignore
        let po (x: obj) = b.Append x |> ignore
        let rec pr e = 
            let rec pl l =
                match l with
                | [] -> ()
                | [i] -> p i
                | i :: t -> p i; p "."; pl t
            let rec prl l =
                match l with
                | [] -> ()
                | [i] -> pr i
                | i :: t -> pr i; p ", "; prl t
            let rec pol l =
                match l with
                | [] -> ()
                | [i] -> po i
                | i :: t -> po i; p ", "; pol t
            let rec pletr l =
                match l with
                | [] -> ()
                | [x, y] -> po x; p " = ("; pr y; pc ')'
                | (x, y) :: t -> po x; p " = ("; pr y; pc ')'; p " and " ; pletr t
            let rec pobj l =
                match l with
                | [] -> ()
                | [x, y] -> p x; p " = ("; pr y; pc ')'
                | (x, y) :: t -> po x; p " = ("; pr y; pc ')'; p ", " ; pobj t
            match e with 
            | Application(x, y)               -> pc '('; pr x; p ")("; prl y; pc ')'
            | Arguments                       -> p "arguments"
            | Binary(x, y, z)                 -> pr x; pc ' '; po y; pc ' '; pr z
            | Call(x, y, z)                   -> pc '('; pr x; p ").("; pr y; p ")("; prl z; pc ')'
            | Constant x                      -> po x
            | FieldDelete(x, y)               -> p "delete ("; pr x; p ").("; pr y; pc ')' 
            | FieldGet(x, y)                  -> pc '('; pr x; p ").["; pr y; pc ']'
            | FieldSet(x, y, z)               -> pc '('; pr x; p ").["; pr y; p "] = ("; pr z; pc ')'
            | ForEachField(x, y, z)           -> p "for ("; po x; p " in "; pr y; p ") {\n"; pr z; pc '}'
            | ForIntegerRangeLoop(x, y, z, u) -> p "for ("; po x; p " = "; pr y; p " to "; pr z; p ") {\n"; pr u; pc '}'
            | Global x                        -> pl x
            | IfThenElse(x, y, z)             -> p "if ("; pr x; p ") {\n"; pr y; p "} else {\n"; pr z; pc '}'
            | Lambda(x, y, z)                 -> p "fun ("; x |> Option.iter (fun x -> po x; p " | "); pol y; p ") {\n"; pr z; pc '}'
            | Let(x, y, z)                    -> p "let "; po x; p " = ("; pr y; p ") in {\n"; pr z; pc '}'
            | LetRecursive(x, y)              -> p "let "; pletr x; p " in {\n"; pr y; pc '}'
            | New(x, y)                       -> p "new ("; pr x; p ")("; prl y; pc ')'
            | NewArray x                      -> pc '['; prl x; pc ']'
            | NewObject x                     -> pc '{'; pobj x; pc '}'
            | NewRegex x                      -> p "RegExp("; p x; pc ')'
            | Runtime                         -> p "Runtime"
            | Sequential(x, y)                -> pr x; p ";\n"; pr y
            | Throw x                         -> p "throw ("; pr x; pc ')'
            | TryFinally(x, y)                -> p "try {\n"; pr x; p "} finally {\n"; pr y; pc '}'
            | TryWith(x, y, z)                -> p "try {\n"; pr x; p "} with ("; po y; p ") {\n"; pr z; pc '}'
            | Unary(x, y)                     -> po x; pc ' '; pr y
            | Var x                           -> po x
            | VarSet(x, y)                    -> po x; p " = ("; pr y; pc ')'
            | WhileLoop(x, y)                 -> p "while ("; pr x; p ") {\n"; pr y; pc '}' 
            | SourcePos(x, y)                 -> pr x
        pr this
        string b

and E = Expression

let inline (|IgnorePos|) e =
    match e with SourcePos(e, _) | e -> e
      
let (|Application        |_|) e = match e with IgnorePos (Application(x, y)              ) -> Some (x, y)       | _ -> None 
let (|Arguments          |_|) e = match e with IgnorePos  Arguments                        -> Some ()           | _ -> None 
let (|Binary             |_|) e = match e with IgnorePos (Binary(x, y, z)                ) -> Some (x, y, z)    | _ -> None 
let (|Call               |_|) e = match e with IgnorePos (Call(x, y, z)                  ) -> Some (x, y, z)    | _ -> None 
let (|Constant           |_|) e = match e with IgnorePos (Constant x                     ) -> Some x            | _ -> None 
let (|FieldDelete        |_|) e = match e with IgnorePos (FieldDelete(x, y)              ) -> Some (x, y)       | _ -> None 
let (|FieldGet           |_|) e = match e with IgnorePos (FieldGet(x, y)                 ) -> Some (x, y)       | _ -> None 
let (|FieldSet           |_|) e = match e with IgnorePos (FieldSet(x, y, z)              ) -> Some (x, y, z)    | _ -> None 
let (|ForEachField       |_|) e = match e with IgnorePos (ForEachField(x, y, z)          ) -> Some (x, y, z)    | _ -> None 
let (|ForIntegerRangeLoop|_|) e = match e with IgnorePos (ForIntegerRangeLoop(x, y, z, u)) -> Some (x, y, z, u) | _ -> None 
let (|Global             |_|) e = match e with IgnorePos (Global x                       ) -> Some x            | _ -> None 
let (|IfThenElse         |_|) e = match e with IgnorePos (IfThenElse(x, y, z)            ) -> Some (x, y, z)    | _ -> None 
let (|Lambda             |_|) e = match e with IgnorePos (Lambda(x, y, z)                ) -> Some (x, y, z)    | _ -> None 
let (|Let                |_|) e = match e with IgnorePos (Let(x, y, z)                   ) -> Some (x, y, z)    | _ -> None 
let (|LetRecursive       |_|) e = match e with IgnorePos (LetRecursive(x, y)             ) -> Some (x, y)       | _ -> None 
let (|New                |_|) e = match e with IgnorePos (New(x, y)                      ) -> Some (x, y)       | _ -> None 
let (|NewArray           |_|) e = match e with IgnorePos (NewArray x                     ) -> Some x            | _ -> None 
let (|NewObject          |_|) e = match e with IgnorePos (NewObject x                    ) -> Some x            | _ -> None 
let (|NewRegex           |_|) e = match e with IgnorePos (NewRegex x                     ) -> Some x            | _ -> None 
let (|Runtime            |_|) e = match e with IgnorePos  Runtime                          -> Some ()           | _ -> None 
let (|Sequential         |_|) e = match e with IgnorePos (Sequential(x, y)               ) -> Some (x, y)       | _ -> None 
let (|Throw              |_|) e = match e with IgnorePos (Throw x                        ) -> Some x            | _ -> None 
let (|TryFinally         |_|) e = match e with IgnorePos (TryFinally(x, y)               ) -> Some (x, y)       | _ -> None 
let (|TryWith            |_|) e = match e with IgnorePos (TryWith(x, y, z)               ) -> Some (x, y, z)    | _ -> None 
let (|Unary              |_|) e = match e with IgnorePos (Unary(x, y)                    ) -> Some (x, y)       | _ -> None 
let (|Var                |_|) e = match e with IgnorePos (Var x                          ) -> Some x            | _ -> None 
let (|VarSet             |_|) e = match e with IgnorePos (VarSet(x, y)                   ) -> Some (x, y)       | _ -> None 
let (|WhileLoop          |_|) e = match e with IgnorePos (WhileLoop(x, y)                ) -> Some (x, y)       | _ -> None 

let Application(x, y)               = Application(x, y)              
let Arguments                       = Arguments                        
let Binary(x, y, z)                 = Binary(x, y, z)                
let Call(x, y, z)                   = Call(x, y, z)                  
let Constant x                      = Constant x                     
let FieldDelete(x, y)               = FieldDelete(x, y)              
let FieldGet(x, y)                  = FieldGet(x, y)                 
let FieldSet(x, y, z)               = FieldSet(x, y, z)              
let ForEachField(x, y, z)           = ForEachField(x, y, z)          
let ForIntegerRangeLoop(x, y, z, u) = ForIntegerRangeLoop(x, y, z, u)
let Global x                        = Global x                       
let IfThenElse(x, y, z)             = IfThenElse(x, y, z)            
let Lambda(x, y, z)                 = Lambda(x, y, z)                
let Let(x, y, z)                    = Let(x, y, z)                   
let LetRecursive(x, y)              = LetRecursive(x, y)             
let New(x, y)                       = New(x, y)                      
let NewArray x                      = NewArray x                     
let NewObject x                     = NewObject x                    
let NewRegex x                      = NewRegex x                     
let Runtime                         = Runtime                        
let Sequential(x, y)                = Sequential(x, y)               
let Throw x                         = Throw x                        
let TryFinally(x, y)                = TryFinally(x, y)               
let TryWith(x, y, z)                = TryWith(x, y, z)               
let Unary(x, y)                     = Unary(x, y)                    
let Var x                           = Var x                          
let VarSet(x, y)                    = VarSet(x, y)                   
let WhileLoop(x, y)                 = WhileLoop(x, y)                

let (==) a b =
    System.Object.ReferenceEquals(a, b)

let WithPos pos (IgnorePos e) = SourcePos(e, pos)

let WithPosOf eFrom e =
    match eFrom with
    | SourcePos(ef, pos) ->
        let e =
            match e with
            | SourcePos(e, _) -> e
            | _ -> e 
        if e == ef then eFrom else SourcePos(e, pos)
    | _ -> e

exception TransformError

// Now define recursion schemes reminiscent of the Haskell Uniplate library.
// On any tree, we need to deconstruct a node into a list of children,
// and reconstruct children at that node.

type M<'T> = list<'T> * (list<'T> -> 'T)
type U<'T> = 'T -> M<'T>

let inline UChildren (recur: U<'T>) node =
    fst (recur node)

let inline UAll u node =
    let ch node = UChildren u node
    let rec all node =
        seq {
            yield node
            for c in ch node do
                yield! all c
        }
    all node

let inline UMapChildren (recur: U<'T>) f node =
    let (ch, b) = recur node
    b (List.map f ch)

let inline UBottomUp (recur: U<'T>) tr node =
    let rec g node =
        let (ch, b) = recur node
        tr (b (List.map g ch))
    g node

// To simplify recursion that is aware of binding structure,
// we will mostly recur on `Node` tree below rather than `E`.
// For example, an expression `let x = Y in Z` will be represented
// as an `ExprNode` with two children, `ExprNode Y` and `BindNode (x, Z)`.

type Node =
    | BindNode of Id * E
    | BindNodes of list<Id> * list<E>
    | ExprNode of E

let inline unExprNode e =
    match e with
    | ExprNode e -> e
    | _ -> raise TransformError

#nowarn "25"

let ENodeMatch e =

    let inline match0 () =
        ([], fun _ -> e)

    let inline match1 x ctor =
        ([ExprNode x], function
            | [ExprNode x] -> ctor x
            | _ -> raise TransformError)

    let inline match2 x y ctor =
        ([ExprNode x; ExprNode y], function
            | [ExprNode x; ExprNode y] -> ctor (x, y)
            | _ -> raise TransformError)

    let inline match3 x y z ctor =
        ([ExprNode x; ExprNode y; ExprNode z], function
            | [ExprNode x; ExprNode y; ExprNode z] -> ctor (x, y, z)
            | _ -> raise TransformError)


    let inline matchL xs ctor =
        let nodes = List.map ExprNode xs
        let build xs = ctor (List.map unExprNode xs)
        (nodes, build)

    let inline matchXL x xs ctor =
        let nodes = ExprNode x :: List.map ExprNode xs
        let build = function
            | ExprNode x :: xs -> ctor (x, List.map unExprNode xs)
            | _ -> raise TransformError
        (nodes, build)

    // First handle every binding form..
    match e with
    | SourcePos (x, pos) ->
        match1 x (WithPosOf e)
    | Application (x, xs) ->
        matchXL x xs Application
    | Arguments ->
        match0 ()
    | Binary (x, op, y) ->
        match2 x y (fun (x, y) -> Binary (x, op, y))
    | Call (x, y, rest) ->
        let nodes = ExprNode x :: ExprNode y :: List.map ExprNode rest
        let build = function
            | ExprNode x :: ExprNode y :: rest ->
                Call (x, y, List.map unExprNode rest)
            | _ -> raise TransformError
        (nodes, build)
    | Constant _ ->
        match0 ()
    | FieldDelete (x, y) ->
        match2 x y FieldDelete
    | FieldGet (x, y) ->
        match2 x y FieldGet
    | FieldSet (x, y, z) ->
        match3 x y z FieldSet
    | ForEachField (var, obj, body) ->
        let nodes = [ExprNode obj; BindNode (var, body)]
        let build = function
            | [ExprNode obj; BindNode (var, body)] ->
                ForEachField (var, obj, body)
            | _ -> raise TransformError
        (nodes, build)
    | ForIntegerRangeLoop (id, x, y, z) ->
        let nodes = [ExprNode x; ExprNode y; BindNode (id, z)]
        let build = function
            | [ExprNode x; ExprNode y; BindNode (id, z)] ->
                ForIntegerRangeLoop (id, x, y, z)
            | _ -> raise TransformError
        (nodes, build)
    | Global _ ->
        match0 ()
    | IfThenElse (x, y, z) ->
        match3 x y z IfThenElse
    | Lambda (None, vs, body) ->
        let node = [BindNodes (vs, [body])]
        let build = function
            | [BindNodes (vs, [body])] -> Lambda (None, vs, body)
            | _ -> raise TransformError
        (node, build)
    | Lambda (Some v, vs, body) ->
        let node = [BindNodes (v :: vs, [body])]
        let build = function
            | [BindNodes (v :: vs, [body])] -> Lambda (Some v, vs, body)
            | _ -> raise TransformError
        (node, build)
    | Let (var, value, body) ->
        let nodes = [ExprNode value; BindNode (var, body)]
        let build = function
            | [ExprNode value; BindNode (var, body)] ->
                Let (var, value, body)
            | _ -> raise TransformError
        (nodes, build)
    | LetRecursive (bindings, body) ->
        let (vars, values) = List.unzip bindings
        let nodes = [BindNodes (vars, body :: values)]
        let build = function
            | [BindNodes (vars, body :: values)] ->
                LetRecursive (List.zip vars values, body)
            | _ -> raise TransformError
        (nodes, build)
    | New (x, xs) ->
        matchXL x xs New
    | NewArray xs ->
        matchL xs NewArray
    | NewObject pairs ->
        let (names, xs) = List.unzip pairs
        matchL xs (NewObject << List.zip names)
    | NewRegex _ ->
        match0 ()
    | Runtime ->
        match0 ()
    | Sequential (x, y) ->
        match2 x y Sequential
    | Throw e ->
        match1 e Throw
    | TryFinally (x, y) ->
        match2 x y TryFinally
    | TryWith (block, var, catch) ->
        let nodes = [ExprNode block; BindNode (var, catch)]
        let build = function
            | [ExprNode block; BindNode (var, catch)] ->
                TryWith (block, var, catch)
            | _ -> raise TransformError
        (nodes, build)
    | Unary (op, e) ->
        match1 e (fun e -> Unary (op, e))
    | Var _ ->
        match0 ()
    | VarSet (id, e) ->
        match1 e (fun e -> VarSet (id, e))
    | WhileLoop (x, y) ->
        match2 x y WhileLoop

let NodeEMatch node =
    match node with
    | BindNode (v, e) ->
        ([e], function [e] -> BindNode (v, e) | _ -> raise TransformError)
    | BindNodes (vs, es) ->
        (es, fun es -> BindNodes (vs, es))
    | ExprNode e ->
        ([e], function [e] -> ExprNode e | _ -> raise TransformError)

let NodeMatch node =
    match node with
    | BindNode (bound, e) ->
        let (nodes, build) = ENodeMatch e
        (nodes, fun nodes -> BindNode (bound, build nodes))
    | BindNodes (bound, es) ->
        (List.map ExprNode es, fun es -> BindNodes (bound, List.map unExprNode es))
    | ExprNode e ->
        let (nodes, build) = ENodeMatch e
        (nodes, fun nodes -> ExprNode (build nodes))

let EMatch e =
    // note: to simplify implementation we are using the fact
    // that BindNodes can only be in a list of length = 1
    // representing LetRecursive
    match ENodeMatch e with
    | (BindNodes (vars, ch) :: _, build) ->
        (ch, fun ch -> build [BindNodes (vars, ch)])
    | (nodes, build) ->
        let nodeE node =
            match node with
            | BindNode (_, e) -> e
            | ExprNode e -> e
            | _ -> raise TransformError
        let nodeUpdateE node e =
            match node with
            | BindNode (ids, _) -> BindNode (ids, e)       
            | ExprNode _ -> ExprNode e
            | _ -> raise TransformError
        let build ch = build (List.map2 nodeUpdateE nodes ch)
        (List.map nodeE nodes, build)

let All expr = UAll EMatch expr
let Transform f expr = UMapChildren EMatch f expr
let Children expr = UChildren EMatch expr
let Fold f init expr = List.fold f init (Children expr)
let Iterate f expr = Seq.iter f (All expr)
let AllNodes node = UAll NodeMatch node
let BottomUp expr = UBottomUp EMatch expr

// Examine all binders. If the same variable binds in multiple
// places, discard the expression as not alpha-normalized.
let IsAlphaNormalized e =
    let set = HashSet()
    AllNodes (ExprNode e)
    |> Seq.forall (fun node ->
        match node with
        | BindNode (var, _) ->
            set.Add(var) |> not
        | BindNodes (vars, _) ->
            vars |> List.forall (fun v -> set.Add(v) |> not)
        | _ -> true)

// Recur on binding structure, freshening the variables.
let AlphaNormalize e =
    let rec normN env node =
        match node with
        | BindNode (var, e) ->
            let varN = Id var
            let envN = Map.add var varN env
            BindNode (varN, normE envN e)
        | BindNodes (vars, exprs) ->
            let (vars, env) =
                (vars, ([], env))
                ||> List.foldBack (fun var (vars, env) ->
                    let v = Id var
                    (v :: vars, Map.add var v env))
            BindNodes (vars, List.map (normE env) exprs)
        | ExprNode e ->
            ExprNode (normE env e)
    and normE env e =
        match e with
        | Var v ->
            match env.TryFind(v) with
            | Some v -> Var v |> WithPosOf e
            | None -> e
        | VarSet (v, e) ->
            let v =
                match env.TryFind(v) with
                | Some v -> v
                | None -> v
            VarSet (v, normE env e) |> WithPosOf e
        | _ ->
            let (nodes, build) = ENodeMatch e
            build (List.map (normN env) nodes)
    if IsAlphaNormalized e then e else normE Map.empty e

// Recur on binding structure, finding free variables.
let GetFreeIdSet e =
    let out = HashSet<Id>()
    let visV bound v =
        if not (Set.contains v bound) then
            out.Add(v) |> ignore
    let rec visN bound node =
        match node with
        | BindNode (var, e) ->
            visE (Set.add var bound) e
        | BindNodes (vars, exprs) ->
            let bound = List.foldBack Set.add vars bound
            List.iter (visE bound) exprs
        | ExprNode expr ->
            visE bound expr
    and visE bound e =
        match e with
        | Var v | VarSet (v, _) -> visV bound v
        | _ -> ()
        List.iter (visN bound) (fst (ENodeMatch e))
    visE Set.empty e
    out

let IsGround e =
    let set = GetFreeIdSet e
    set.Count = 0

let Substitute f e =
    let e = AlphaNormalize e
    let free = GetFreeIdSet e
    let replace e =
        match e with
        | Var v when not v.IsMutable && free.Contains(v) ->
            match f v with
            | Some e -> AlphaNormalize e
            | None -> e
        | _ -> e
    BottomUp replace e

let GetFreeIds e =
    Set.ofSeq (GetFreeIdSet e)

// Utilities ------------------------------------------------------------------

let ClosedSet values =
    let h = HashSet()
    for x in values do
        ignore (h.Add x)
    h.Contains

let ClosedMap pairs =
    let d = Dictionary()
    for (x, y) in pairs do
        d.[x] <- y
    fun x ->
        match d.TryGetValue x with
        | true, y -> Some y
        | _ -> None

let GlobalName prefs =
    match prefs with
    | Compact -> "$$"
    | Readable -> "Global"

let RuntimeName prefs =
    match prefs with
    | Compact -> "$"
    | Readable -> "Runtime"

module Scope =

    type T =
        private {
            Children : HashSet<T>
            Count : ref<int>
            Formatter : int -> S.Id
            Formals : HashSet<Id>
            Mode : Preferences
            Parent : option<T>
            Table : Dictionary<Id,S.Expression>
            This : Id
            Used : HashSet<S.Id>
        }

    let New mode =
        let common = ["undefined"; "Infinity"; "NaN"; "IntelliFactory"]
        let used = [RuntimeName mode; GlobalName mode] @ common
        {
            Children = HashSet()
            Count = ref 0
            Formatter = Identifier.MakeFormatter()
            Formals = HashSet()
            Mode = mode
            Parent = None
            Table = Dictionary()
            This = Id()
            Used = HashSet used
        }

    let Use scope id =
        scope.Used.Add id |> ignore

    let private IsUsed id scope =
        let u x =
            x.Used.Contains id
        let rec pu x =
            match x.Parent with
            | Some p -> u p || pu p
            | None -> false
        let rec cu x =
            x.Children
            |> Seq.exists (fun x -> u x || cu x)
        u scope || pu scope || cu scope

    let private PickCompactName id scope =
        let rec pick k =
            let n = scope.Formatter k
            if IsUsed n scope then
                pick (k + 1)
            else
                (k, n)
        let (k, n) = pick !scope.Count
        incr scope.Count
        n

    let private PickReadableName (id: Id) scope =
        let fmt (x: string) (n: int) =
            if n = 0 then x else
                System.String.Format("{0}{1:x}", x, n)
        let n = defaultArg id.Name "_"
        let i = Identifier.MakeValid n
        let rec pick name k =
            let res = fmt name k
            if IsUsed res scope then
                pick name (k + 1)
            else
                res
        pick i 0

    let rec private Bind id scope =
        let name =
            match scope.Mode with
            | Compact -> PickCompactName id scope
            | Readable -> PickReadableName id scope
        let e =
            match id.Name with
            | None -> S.Var name
            | Some n -> if n = name then S.Var n else S.VarNamed(name, n)
        scope.Table.[id] <- e
        Use scope name
        e

    let Expression scope id =
        let rec lookup scope id k =
            match scope.Table.TryGetValue id with
            | true, value -> Some value
            | _ ->
                if scope.This = id then
                    if k = 0 then Some S.This else
                        Some (Bind id scope)
                else
                    match scope.Parent with
                    | Some p -> lookup p id (k + 1)
                    | None -> None
        match lookup scope id 0 with
        | None -> Bind id scope
        | Some v -> v

    let Id scope id =
        match Expression scope id with
        | S.This -> 
            match Bind id scope with
            | S.Var x -> x
        | S.Var x  -> x

    let Nest scope this formals =
        let nS =
            {
                Children = HashSet()
                Count = scope.Count
                Formatter = scope.Formatter
                Formals = HashSet(Seq.ofList formals)
                Parent = Some scope
                Mode = scope.Mode
                Table = Dictionary()
                This = defaultArg this (new Id())
                Used = HashSet()
            }
        scope.Children.Add nS |> ignore
        nS

    let Vars scope =
        [
            for KeyValue (k, S.Var v) in scope.Table do
                if scope.This = k then
                    yield (v, Some S.This)
                elif not (scope.Formals.Contains k) then
                    yield (v, None)
        ]

    let WithVars scope body =
        match Vars scope with
        | [] -> body
        | xs -> S.Action (S.Vars xs) :: body

// Optimization ---------------------------------------------------------------

// Remove 'this' bindings from lambdas when it is not used in body
let RemoveUnusedThis expr =
    let bound = HashSet()
    let rec rem expr =
        match expr with
        | Lambda (Some this, args, body) ->
            bound.Add(this) |> ignore
            let bodyTr = Transform rem body
            if bound.Remove(this) then
                 Lambda (None, args, bodyTr)
            else
                 Lambda (Some this, args, bodyTr)
            |> WithPosOf expr
        | Var v ->
            bound.Remove(v) |> ignore
            expr
        | _ ->
            Transform rem expr
    rem expr

/// Transforms local Let- or LetRecursive-bound curried lambda functions to
/// multi-argument functions when such transformations are possible - the
/// functions are strictly local, do not escape the scope, and are always
/// called with the correct number of arguments.
let Uncurry expression =
    let (|CurriedApplication|_|) expr =
        let rec loop n acc = function
            | Application (f, [x]) -> loop (n + 1) (x :: acc) f
            | f -> (n, f, acc)
        match loop 0 [] expr with
        | n, f, x when n > 0 -> Some (n, f, x)
        | _ -> None
    let (|CurriedLambda|_|) expr =
        let rec loop n acc = function
            | Lambda (None, [x], y) -> loop (n + 1) (x :: acc) y
            | b -> (n, List.rev acc, b)
        match loop 0 [] expr with
        | (n, vars, body) when n > 0 -> Some (n, vars, body)
        | _ -> None
    let arities = Dictionary()
    let rec analyze = function
        | CurriedApplication (k, Var f, _) ->
            match arities.TryGetValue f with
            | true, n ->
                if n <> k then
                    arities.[f] <- 0
            | false, _ ->
                arities.[f] <- k
        | Var x ->
            arities.[x] <- 0
        | expr ->
            List.iter analyze (Children expr)
    let rec optimize (fs: Set<_>) expr =
        match expr with
        | CurriedApplication (_, Var f, xs) when fs.Contains f ->
            Application (Var f, List.map (optimize fs) xs)
        | Let (var, value, body) ->
            match value with
            | CurriedLambda (j, vars, b) when j > 1 ->
                match arities.TryGetValue var with
                | true, k when k = j ->
                    Let (
                        var,
                        Lambda (None, vars, optimize fs b),
                        optimize (Set.add var fs) body
                    )
                | _ ->
                    Let (var, optimize fs value, optimize fs body)
            | _ ->
                Let (var, optimize fs value, optimize fs body)
        | LetRecursive (bindings, body) ->
            let fs =
                (fs, bindings)
                ||> List.fold (fun s (v, b) ->
                    match b with
                    | CurriedLambda (j, vars, body) when j > 1 ->
                        match arities.TryGetValue v with
                        | true, k when k = j -> Set.add v s
                        | _ -> s
                    | _ -> s)
            LetRecursive (
                bindings
                |> List.map (fun (v, b) ->
                    if fs.Contains v then
                        match b with
                        | CurriedLambda (_, vs, b) ->
                            (v, Lambda (None, vs, optimize fs b))
                    else
                        (v, optimize fs b)),
                optimize fs body
            )
        | _ ->
            Transform (optimize fs) expr
        |> WithPosOf expr
    analyze expression
    optimize Set.empty expression

/// Analyses an expression to find loop-like
/// LetRecursive expressions in O(N) time. A LetRecursive
/// expression can be compiled to a loop if all the variables that
/// it binds are either not used in the branches and body, or used
/// as tail call targets within the original scope.
let IsLoop expr =
    let rec analyze loops vars labels expr =
        let add x (a: HashSet<_>) =
            ignore (a.Add x)
        match expr with
        | Application (Var f, a) ->
            add f labels
            List.iter (analyze loops vars vars) a
        | IfThenElse (cond, body, alt) ->
            analyze loops vars vars cond
            analyze loops vars labels body
            analyze loops vars labels alt
        | Lambda (_, _, body) ->
            analyze loops vars vars body
        | Let (_, value, body) ->
            analyze loops vars vars value
            analyze loops vars labels body
        | LetRecursive (bindings, body) ->
            let jumps = HashSet()
            analyze loops vars jumps body
            let vs =
                bindings
                |> List.map (fun (var, branch) ->
                    match branch with
                    | Lambda (None, _, body) -> body
                    | _ -> branch
                    |> analyze loops vars jumps
                    var)
            let isLoop =
                let ok (var, body) =
                    match body with
                    | Lambda (None, _, _) -> not (vars.Contains var)
                    | _ -> false
                List.forall ok bindings
            if isLoop then
                add expr loops
                labels.UnionWith jumps
            else
                vars.UnionWith jumps
        | Sequential (x, y) ->
            analyze loops vars vars x
            analyze loops vars labels y
        | Var v ->
            add v vars
        | expr ->
            Fold (fun () e -> analyze loops vars vars e) () expr
    let loops = HashSet()
    let vars = HashSet()
    analyze loops vars vars expr
    loops.Contains

/// Compiles LetRecursive expressions to loops when possible.
let RemoveLoops expr =
    let isLoop = IsLoop expr
    let labels = Dictionary()
    let slots = Dictionary()
    let i x = Constant (Integer (int64 x))
    let ( ++ ) a b = Sequential (a, b)
    let rec t ret expr =
        match expr with
        | Application (Var f, a) when labels.ContainsKey f ->
            let (args, p) = labels.[f]
            let rec g k bs ss = function
                | [] ->
                    let s =
                        (ss, FieldSet (args, i 0, i p))
                        ||> List.foldBack (++)
                    (bs, s)
                    ||> List.foldBack (fun (k, v) x -> Let (k, v, x))
                | x :: xs ->
                    let v = Id ()
                    let bs = (v, t id x) :: bs
                    let ss = FieldSet (args, i k, Var v) :: ss
                    g (k + 1) bs ss xs
            g 1 [] [] a
        | IfThenElse (cond, body, alt) ->
            IfThenElse (t id cond, t ret body, t ret alt)
        | Lambda (this, formals, body) ->
            ret (Lambda (this, formals, t id body))
        | Let (var, value, body) ->
            Let (var, t id value, t ret body)
        | LetRecursive (bindings, body) ->
            if isLoop expr then
                loop ret bindings body
            else
                let bindings = [for (k, v) in bindings -> (k, t id v)]
                LetRecursive (bindings, t ret body)
        | Sequential (x, y) ->
            Sequential (t id x, t ret y)
        | Var v ->
            match slots.TryGetValue v with
            | true, (_, 0) -> Constant Undefined
            | true, (a, k) -> FieldGet (a, i k)
            | _ -> expr
            |> ret
        | _ ->
            ret (Transform (t id) expr)
        |> WithPosOf expr

    and loop ret bindings body =
        let argId = Id "loop"
        let args = Var argId
        bindings
        |> List.iteri (fun i (k, v) ->
            labels.[k] <- (args, i + 1)
            match v with
            | Lambda (None, formals, body) ->
                formals
                |> List.iteri (fun j v ->
                    slots.[v] <- (args, j + 1))
        )
        let exit x =
            FieldSet (args, i 0, i 0)
            ++ FieldSet (args, i 1, x)
        let next = FieldGet (args, i 0)
        let switch x cases =
            let rec f k = function
                | [] -> Unary (UnaryOperator.``void``, x)
                | [c] -> c
                | c::cs -> IfThenElse (Binary (x, B.``===``, i k),
                                       c, f (k+1) cs)
            f 1 cases
        let getBody (_, Lambda (None, _, b)) = b
        let states = List.map (getBody >> t exit) bindings
        let cycle = WhileLoop (next, switch next states)
        let res = ret (FieldGet (args, i 1))
        Let (argId, NewArray [], t exit body ++ cycle ++ res)
    t id expr

// Transforms JavaScipt object creation with additional field setters
// into a single object literal 
let CollectObjLiterals expr =
    let notOccurs (var: Id) (expr: E) =
        All expr
        |> Seq.exists (function Var v when v = var -> true | _ -> false)
        |> not

    let rec (|PropSet|_|) expr =
        match expr with
        | Unary (UnaryOperator.``void``, FieldSet (Var objVar, Constant (String field), value))
        | FieldSet (Var objVar, Constant (String field), value) 
            when notOccurs objVar value ->
            Some (objVar, (field, value))
        | Let (var, value, PropSet ((objVar, (field, Var v)))) 
            when v = var && notOccurs objVar value ->
            Some (objVar, (field, value))
        | _ -> None
    let rec coll expr =
        let tr = Transform coll
        match expr with
        | Let (objVar, NewObject objFields, Sequential (propSetters, Var v)) when v = objVar ->
            let rec getSetters acc e =
                match e with
                | Constant Null -> Some acc
                | Sequential (more, PropSet (v, fv)) when v = objVar ->
                    getSetters (fv :: acc) more
                | PropSet (v, fv) when v = objVar -> 
                    Some (fv :: acc)
                | _ -> None
            match getSetters [] propSetters with
            | Some s -> 
                objFields @ s 
                |> List.map (fun (f, vExpr) -> f, tr vExpr)
                |> NewObject
                |> WithPosOf expr
            | _ -> tr expr
        | _ -> tr expr

    coll expr

// define approximate tests for purity
// true value means evaluating the expr has no side-effects
//
// isMostlyPure : treat identity effects (new obj) as benign
// isStrictlyPure : treat identity effects as side-effects
//
// both predicates recur to an arbitrary fixed depth.
let (isMostlyPure, isStrictlyPure) =

    let isStrictlyPureNode expr =
        match expr with
        | Var i when not i.IsMutable -> true
        | Arguments | Constant _ | Global _ | Lambda _ | Runtime
        | Binary _  | FieldGet _
        | IfThenElse _ | Let _ | LetRecursive _ | Unary _ -> true
        | _ -> false

    let isMostlyPureNode expr =
        match expr with
        | NewArray _ | NewObject _ | NewRegex _ -> true
        | _ -> isStrictlyPureNode expr

    // true if expr is less than n-deep and all nodes sastisfy the predicate
    let rec sat pred d expr =
        if (d <= 0) || (not (pred expr)) then false else
            List.forall (sat pred (d - 1)) (Children expr)

    let isMostlyPure = sat isMostlyPureNode 3
    let isStrictlyPure = sat isStrictlyPureNode 3

    (isMostlyPure, isStrictlyPure)

let containsVar (var: Id) (expr: E) =
    let all = All expr
    use e = all.GetEnumerator()
    let rec loop (occurs: int) =
        if e.MoveNext() then
            match e.Current with
            | Var v when v = var -> true
            | _ -> loop occurs
        else false
    loop 0

// fast-track Substitue: assume IsAlphaNormalized on all
// expressions involved
let subst (var: Id) replace body =
    if var.IsMutable then
        invalidArg "var" "Var should not be mutable"
    let sub e =
        match e with
        | Var v when v = var -> replace
        | _ -> e
    BottomUp sub body

// Eliminate calls to Runtime.js helpers when possible.
let CleanupRuntime expr =
    
    let sliceFromArguments slice =
        Call (Global [ "Array"; "prototype"; "slice"], 
            !~ (String "call"), Arguments :: [ for a in slice -> !~ (Integer a) ])

    let (|TupledLambda|_|) expr =
        match expr with
        | Lambda (None, [tupledArg], b) ->
            // when the tuple itself is bound to a name, there will be an extra let expression
            let tupledArg, b =
                match b with
                | Let (newTA, Var t, b) when t = tupledArg -> 
                    newTA, subst tupledArg (Var newTA) b
                | _ -> tupledArg, b
            let rec loop acc = function
                | Let (v, FieldGet(Var t, Constant (Integer i)), body) when t = tupledArg ->
                    loop ((int i, v) :: acc) body
                | body -> 
                    if List.isEmpty acc then None else
                    let m = Map.ofList acc
                    Some (
                        [ for i in 0 .. (acc |> Seq.map fst |> Seq.max) -> 
                            match m |> Map.tryFind i with
                            | None -> Id()
                            | Some v -> v 
                        ], body)
            match loop [] b with
            | None -> None
            | Some (vars, body) -> 
                if containsVar tupledArg body then
                    let (|TupleGet|_|) e =
                        match e with 
                        | FieldGet(Var t, Constant (Integer i)) when t = tupledArg ->
                            Some (int i)
                        | _ -> None 
                    let maxTupleGet = ref (vars.Length - 1)
                    let rec alwaysTupleGet e =
                        match e with 
                        | TupleGet i -> 
                            if i > !maxTupleGet then maxTupleGet := i
                            true
                        | Var t when t = tupledArg -> false
                        | _ -> Children e |> List.forall alwaysTupleGet
                    if alwaysTupleGet body then
                        let vars = 
                            if List.length vars > !maxTupleGet then vars
                            else vars @ [ for k in List.length vars .. !maxTupleGet -> Id() ]
                        Some (vars, body |> BottomUp (function TupleGet i -> Var vars.[i] | e -> e))
                    else 
                        // if we would use the arguments object for anything else than getting
                        // a tuple item, convert it to an array
                        Some (vars, Let (tupledArg, sliceFromArguments [], body))
                else
                    Some (vars, body)
        | _ -> None

    let rec clean expr =
        let tr = Transform clean
        match expr with
        | Application (Call (Runtime, Constant (String "Bind"), [f; obj]), args) ->
            Call(tr f, !~(String "call"), (obj :: args) |> List.map tr)
        | Call (Call (Runtime, Constant (String "Bind"), [f; obj]), Constant (String "apply"), [args]) ->
            Call (tr f, !~(String "apply"), [tr obj; tr args])
        | Call (Runtime, Constant (String rtFunc), xs) ->
            match rtFunc, xs with
            | "CreateFuncWithArgs", [ TupledLambda (vars, body) as f ] ->
                Lambda(None, vars, tr body) |> WithPosOf f
            | "CreateFuncWithOnlyThis", [ Lambda (None, [obj], body) as f ] ->
                Lambda (Some obj, [], tr body) |> WithPosOf f   
            | "CreateFuncWithThis", [ Lambda (None, [obj], Lambda (None, args, body)) as f ] ->
                Lambda (Some obj, args, tr body) |> WithPosOf f   
            | "CreateFuncWithThisArgs", [ Lambda (None, [obj], TupledLambda (vars, body)) as f ] ->
                Lambda(Some obj, vars, tr body) |> WithPosOf f
            | "CreateFuncWithRest", [ Constant (Integer length); TupledLambda (vars, body) as f ] ->
                let rest :: fixRev = List.rev vars
                let fix = List.rev fixRev
                Lambda (None, fix, Let (rest, sliceFromArguments [ length ], body)) |> WithPosOf f
            | "SetOptional", [obj; field; optValue] ->
                match optValue with
                | NewObject ["$", Constant (Integer 0L)] ->
                    FieldDelete (tr obj, tr field) |> WithPosOf expr
                | NewObject ["$", Constant (Integer 1L); "$0", value] ->
                    FieldSet (tr obj, tr field, tr value) |> WithPosOf expr
                | _ -> tr expr     
            | "NewObject", [NewArray keyValuePairs] ->
                let withConstantKey =
                    keyValuePairs |> List.choose (function 
                        | NewArray [Constant (String k); v] -> Some (k, v) 
                        | _ -> None)
                if withConstantKey.Length = keyValuePairs.Length then
                    NewObject (withConstantKey |> List.map (fun (k, v) -> k, tr v)) |> WithPosOf expr
                else tr expr
            | _ -> tr expr     
        | Let (var, value, body) when not var.IsMutable ->
            // transform function if it is always used as JavaScript interop
            let transformIfAlwaysInterop rtFunc jsFunc =
                let (|WithInterop|_|) e =
                    match e with
                    | Call (Runtime, Constant (String f), [ Var v ]) when f = rtFunc && v = var -> Some ()
                    | _ -> None
                let rec alwaysInterop e =
                    match e with
                    | WithInterop -> true
                    | Var v when v = var -> false
                    | _ -> Children e |> List.forall alwaysInterop  
                if alwaysInterop body then
                    Let(var, jsFunc |> tr |> WithPosOf value, body |> BottomUp (function WithInterop -> Var var | e -> e) |> tr)
                else tr expr
            match value with
            | TupledLambda (vars, lBody) ->
                transformIfAlwaysInterop "CreateFuncWithArgs" (Lambda(None, vars, lBody))
            | Lambda (None, [obj], Lambda (None, args, lBody)) ->
                transformIfAlwaysInterop "CreateFuncWithThis" (Lambda (Some obj, args, lBody))
            | Lambda (None, [obj], lBody) ->
                transformIfAlwaysInterop "CreateFuncWithOnlyThis" (Lambda (Some obj, [], lBody))
            | Lambda (None, [obj], TupledLambda (vars, lBody)) ->
                transformIfAlwaysInterop "CreateFuncWithThisArgs" (Lambda (Some obj, vars, lBody))
            | _ ->
                tr expr
        | _ -> tr expr
     
    clean expr   
        
let Simplify expr =

    // counts free occurences of var in expr
    // return : 0 - no occurences; 1 - one occurence; 2 - unknown
    // assumes IsAlphaNormalized on all expressions invovled
    // because of the invariant, all occurences are free
    let occurenceCountApprox (var: Id) (expr: E) =
        if var.IsMutable then
            invalidArg "var" "Var should not be mutable"
        let all = All expr
        use e = all.GetEnumerator()
        let rec loop (steps: int) (occurs: int) =
            if steps > 64 then 2 else
                if e.MoveNext() then
                    match e.Current with
                    | Var v when v = var ->
                        match occurs + 1 with
                        | 1 -> loop (steps + 1) 1
                        | _ -> 2
                    | _ ->
                        loop (steps + 1) occurs
                else occurs
        loop 0 0

    // optimizes a given expr = Let (var, value, body)
    // assuming occurenceCount var body = 1
    // test here if the only occurence is the first thing
    // that gets evaluated. if so, inlining is safe.
    // state: 0 = search; 1 = ok; 2 = fail
    // state is used as a poor-man's cheap exceptions -
    // would use exceptions here in OCaml/SML
    let inlineLet expr var value body =
        let st = ref 0
        let stop () =
            if !st = 0 then st := 2
        let rec eval e =
            if !st = 0 then
                match e with
                | Application _ | Call _ | FieldSet _ | FieldDelete _
                | New _ | NewArray _ | NewObject _ | NewRegex _ | Throw _ | VarSet _ ->
                    List.iter eval (Children e); stop ()
                | Binary (x, _, y) | FieldGet (x, y) | Sequential (x, y) -> eval x; eval y
                | Arguments | Constant _ | Global _ | Lambda _ | Runtime -> ()
                | ForEachField (_, obj, _) -> eval obj; stop ()
                | ForIntegerRangeLoop (_, x, _, _) -> eval x; stop ()
                | IfThenElse (e, _, _) -> eval e; stop ()
                | Let (_, v, b) -> eval v; eval b
                | LetRecursive (bs, b) -> List.iter (snd >> eval) bs; eval b
                | TryFinally _ | TryWith _ -> stop ()
                | Unary (_, x) -> eval x
                | Var v -> (if v.IsMutable then stop () elif v = var then st := 1)
                | WhileLoop (_, _) -> stop ()
        eval body
        if !st = 1 then subst var value body else expr

    // specify a simple rewrite step - recusion is taken care of later
    // assume rewrites are confluent or else the order does not matter
    // assume rewrites terminate
    let step expr =
        match expr with
        | Application (Lambda (None, args, body), xs) ->
            if List.length args = List.length xs then
                let bind key value body = Let (key, value, body)
                List.foldBack2 bind args xs body |> WithPosOf expr
            else expr
        | Application (Let (var, value, body), xs) ->
            Let (var, value, Application (body, xs)) |> WithPosOf expr
        | Let (var, Let (v, vl, bd), body) ->
            Let (v, vl, Let (var, bd, body)) |> WithPosOf expr
        // this pattern is generated by Async functions
        | Let (var, value, Lambda (None, [x], Application (Var f, [Var y])))
            when not var.IsMutable && f = var && x = y ->
                value
        | Let (var, value, body) when not var.IsMutable ->
            match value with
            | Constant _ | Global _ | Runtime ->
                subst var value body
            | Var var2 when not var2.IsMutable ->
                subst var value body
            | Lambda _ ->
                let mutable count = 0
                let mutable isApplication = false
                for occ in All body do
                    match occ with
                    | Application (Var f, _) when f = var -> isApplication <- true
                    | Var x when x = var -> count <- count + 1
                    | _ -> ()
                match count with
                | 0 -> body
                | 1 ->
                    if isApplication
                    then subst var value body
                    else inlineLet expr var value body
                | _ -> 
                    expr
            | _ ->
                match occurenceCountApprox var body with
                | 0 -> if isMostlyPure value then body else Sequential (value, body)
                | 1 -> inlineLet expr var value body
                | _ -> expr
        | Sequential (a, b) when isMostlyPure a -> b
        | FieldGet (NewObject fs, Constant (String fieldName)) ->
            let mutable nonPureBefore = []
            let mutable nonPureAfter = []
            let mutable fieldValue = None
            for n, v in fs do
                if n = fieldName then
                    fieldValue <- Some v
                else 
                    if not (isMostlyPure v) then
                        match fieldValue with
                        | None -> nonPureBefore <- v :: nonPureBefore
                        | _ -> nonPureAfter <- v :: nonPureAfter
            let fieldValue = defaultArg fieldValue (Constant Undefined)
            let result =
                Seq.fold (fun e v -> Sequential (v, e)) fieldValue nonPureBefore 
            if List.isEmpty nonPureAfter then
                result 
            else 
                let resVar = Id fieldName
                Let (resVar, result, 
                    Seq.fold (fun e v -> Sequential (v, e)) (Var resVar) nonPureAfter
                )
        | FieldGet (NewArray fs, Constant (Integer index)) ->
            let mutable nonPureBefore = []
            let mutable nonPureAfter = []
            let mutable fieldValue = None
            let mutable i = 0
            for v in fs do
                if i = int index then
                    fieldValue <- Some v
                else 
                    if not (isMostlyPure v) then
                        match fieldValue with
                        | None -> nonPureBefore <- v :: nonPureBefore
                        | _ -> nonPureAfter <- v :: nonPureAfter
                i <- i + 1
            let fieldValue = defaultArg fieldValue (Constant Undefined)
            let result =
                Seq.fold (fun e v -> Sequential (v, e)) fieldValue nonPureBefore 
            if List.isEmpty nonPureAfter then
                result 
            else 
                let resVar = Id ("item" + string index)
                Let (resVar, result, 
                    Seq.fold (fun e v -> Sequential (v, e)) (Var resVar) nonPureAfter
                )
        | _ -> expr

    // keep rewriting with `step` bottom-up until reach a fixpoint
    let rec simpl expr =
        let changed = ref false
        let tr e =
            let eN = step e
            if not (eN == e) then
                changed := true
            eN
        let exprN = BottomUp tr expr
        if !changed then simpl exprN else expr

    simpl (AlphaNormalize expr)

let Optimize expr =
    expr
    |> AlphaNormalize
    |> RemoveUnusedThis
    |> Uncurry
    |> RemoveLoops
    |> CollectObjLiterals
    |> CleanupRuntime
    |> Simplify

// Elaboration ----------------------------------------------------------------

let ElaborateBinaryOperator op =
    match op with
    | B.``!==`` -> SB.``!==``
    | B.``!=`` -> SB.``!=``
    | B.``%`` -> SB.``%``
    | B.``&&`` -> SB.``&&``
    | B.``&`` -> SB.``&``
    | B.``*`` -> SB.``*``
    | B.``+`` -> SB.``+``
    | B.``-`` -> SB.``-``
    | B.``/`` -> SB.``/``
    | B.``<<`` -> SB.``<<``
    | B.``<=`` -> SB.``<=``
    | B.``<`` -> SB.``<``
    | B.``===`` -> SB.``===``
    | B.``==`` -> SB.``==``
    | B.``>=`` -> SB.``>=``
    | B.``>>>`` -> SB.``>>>``
    | B.``>>`` -> SB.``>>``
    | B.``>`` -> SB.``>``
    | B.``^`` -> SB.``^``
    | B.``in`` -> SB.``in``
    | B.``instanceof`` -> SB.``instanceof``
    | B.``|`` -> SB.``|``
    | _ -> SB.``||``

let ElaborateUnaryOperator op =
    match op with
    | U.``~`` -> SU.``~``
    | U.``-`` -> SU.``-``
    | U.``!`` -> SU.``!``
    | U.``+`` -> SU.``+``
    | U.``typeof`` -> SU.``typeof``
    | _ -> SU.``void``

let ElaborateConstant c =
    let ne x = S.Unary (S.UnaryOperator.``-``, x)
    let elaborateDouble x =
        match x with
        | x when System.Double.IsNaN x -> S.Var "NaN"
        | x when System.Double.IsPositiveInfinity x -> S.Var "Infinity"
        | x when System.Double.IsNegativeInfinity x -> ne (S.Var "Infinity")
        | x when x >= 0. -> S.Constant (S.Number (string x))
        | _ -> ne (S.Constant (S.Number (string (abs x))))
    let elaborateInteger x =
        if x >= 0L then S.Constant (S.Number (string x))
                   else ne (S.Constant (S.Number (string (abs x))))
    match c with
    | Double x -> elaborateDouble x
    | False -> S.Constant S.False
    | Integer x -> elaborateInteger x
    | Null -> S.Constant S.Null
    | String x -> S.Constant (S.String x)
    | True -> S.Constant S.True
    | Undefined -> S.Var "undefined"

/// Check if an expression is compilable to a JavaScript expression.
let rec CompilesToJavaScriptExpression (expr: E) : bool =
    let inline isE x = CompilesToJavaScriptExpression x
    match expr with
    | Application (f, a)
    | New (f, a) -> isE f && List.forall isE a
    | Binary (x, _, y)
    | FieldDelete (x, y)
    | FieldGet (x, y) -> isE x && isE y
    | Call (t, m, a) -> isE t && isE m && List.forall isE a
    | Arguments 
    | Constant _
    | Global _
    | Lambda _
    | NewRegex _
    | Runtime
    | Var _ -> true
    | FieldSet (x, y, z)
    | IfThenElse (x, y, z) -> isE x && isE y && isE z
    | ForEachField _
    | ForIntegerRangeLoop _
    | Let _
    | LetRecursive _
    | Sequential _
    | Throw _
    | TryFinally _ 
    | TryWith _
    | WhileLoop _ -> false
    | NewArray xs -> List.forall isE xs
    | NewObject xs -> List.forall (snd >> isE) xs
    | Unary (_, x) | VarSet (_, x) -> isE x

type Effects =
    | EffZ
    | EffSt of S.Statement
    | EffApp of Effects * Effects

    static member Append a b =
        match a, b with
        | EffZ, x | x, EffZ -> x
        | _ -> EffApp (a, b)

    static member Concat es =
        match es with
        | [] -> EffZ
        | xs -> List.reduce Effects.Append xs

    static member Statement s = EffSt s
    static member Zero = EffZ

    member eff.ToStatements() =
        let rec toS eff tail =
            match eff with
            | EffZ -> tail
            | EffSt s -> s :: tail
            | EffApp (a, b) -> toS a (toS b tail)
        toS eff []

    member eff.ToBlock() =
        S.Block (eff.ToStatements())

type K =
    | KIgnore
    | KReturn
    | KSet of S.Id
    | KThrow
    | KWith of (S.Expression -> Effects)

let GetMutableIds expr =
    let set = HashSet<Id>()
    let add v = set.Add(v) |> ignore
    expr
    |> Iterate (function
        | VarSet (var, _)
        | ForIntegerRangeLoop (var, _, _, _)
        | ForEachField (var, _, _) -> add var
        | _ -> ())
    Set.ofSeq set

let CloseOverMutablesIfNecessary (mut: Set<Id>) (sc: Scope.T) (expr: E) (tr: S.Expression) : S.Expression =
    let vs = Set.intersect (GetFreeIds expr) mut
    if not vs.IsEmpty then
        let vars = [for m in vs -> Scope.Id sc m]
        S.Application (S.Lambda (None, vars, [S.Action (S.Return (Some tr))]), List.map S.Var vars)
    else tr

let ToProgram prefs (expr: E) : S.Program =

    let mut = GetMutableIds expr

    let lib = RuntimeName prefs
    let glob = GlobalName prefs
    let undef = S.Var "undefined"

    let ( ++ ) a b = Effects.Append a b
    let stmt x = Effects.Statement x

    let appKU k =
        match k with
        | KIgnore -> Effects.Zero
        | KReturn -> stmt (S.Return None)
        | KSet var -> Effects.Zero
        | KThrow -> stmt (S.Throw undef)
        | KWith cont -> cont undef

    let isPureSE e =
        match e with
        | S.Constant _
        | S.Var _ -> true
        | _ -> false

    let noVoid e =
        let e =
            match e with
            | S.Unary (S.UnaryOperator.``void``, e) -> e
            | _ -> e
        if isPureSE e then Effects.Zero else stmt (S.Ignore e)

    let noVoidRet e =
        match e with
        | S.Unary (S.UnaryOperator.``void``, e) ->
            noVoid e ++ stmt (S.Return None)
        | _ -> stmt (S.Return (Some e))

    let appK k (e: S.Expression) : Effects =
        match e with
        | S.Var "undefined" -> appKU k
        | e ->
            match k with
            | KIgnore -> noVoid e
            | KReturn -> noVoidRet e
            | KSet var -> noVoid (S.Var var ^= e)
            | KThrow -> stmt (S.Throw e)
            | KWith cont -> cont e

    let block (eff: Effects) =
        eff.ToBlock()

    let voidK k s =
        s ++ appKU k

    let voidKS k s =
        voidK k (stmt s)

    let withPos pos e = 
        match e with
        | S.ExprPos (e, _)
        | e -> S.ExprPos (e, pos)

    let rec cW (sc: Scope.T) (expr: E) (k: S.Expression -> Effects) : Effects =
        c sc expr (KWith k)

    and cW2 sc e1 e2 k =
        cW sc e1 (fun e1 -> cW sc e2 (k e1))

    and cW3 sc e1 e2 e3 k =
        cW2 sc e1 e2 (fun e1 e2 -> cW sc e3 (k e1 e2))

    and cList sc list k =
        match list with
        | [] -> k []
        | e1 :: es -> cW sc e1 (fun e1 -> cList sc es (fun e2 -> k (e1 :: e2)))

    and c sc expr k =
        let inline (!^) x = Scope.Expression sc x
        match expr with
        | SourcePos (x, pos) ->
            cW sc x (fun x -> appK k (x |> withPos pos))
        | Application (f, a) ->
            match f with
            | FieldGet (t, m) ->
                cW sc t (fun t -> cW sc m (fun m -> cList sc a (fun a ->
                    appK k t.[m]?call.[!~ S.Null :: a])))
            | _ ->
                cW sc f (fun f -> cList sc a (fun a -> appK k f.[a]))
        | Arguments ->
            appK k (S.Var "arguments")
        | Binary (x, o, y) ->
            let o = ElaborateBinaryOperator o
            cW2 sc x y (fun x y -> appK k (S.Binary (x, o, y)))
        | Call (t, m, a) ->
            cW2 sc t m (fun t m -> cList sc a (fun a -> appK k t.[m].[a]))
        | Constant c ->
            appK k (ElaborateConstant c)
        | FieldDelete (t, f) ->
            cW2 sc t f (fun t f ->
                let d = t.[f].Delete
                let d =
                    match k with
                    | KIgnore -> d
                    | _ -> d.Void
                appK k d)
        | FieldGet (t, f) ->
            cW2 sc t f (fun t f -> appK k t.[f])
        | FieldSet (t, f, v) ->
            cW3 sc t f v (fun t f v ->
                let s = t.[f] ^= v
                let s =
                    match k with
                    | KIgnore -> s
                    | _ -> s.Void
                appK k s)
        | ForEachField (i, o, b) ->
            cW sc o (fun o ->
                let v = !^ i
                S.ForIn (v, o, block (c sc b KIgnore))
                |> voidKS k)
        | ForIntegerRangeLoop (i, l, h, b) ->
            cW2 sc l h (fun l h ->
                let v = !^i
                let pp = S.Postfix (v, S.PostfixOperator.``++``)
                S.For (Some (v ^= l), Some (v &<= h), Some pp, block (c sc b KIgnore))
                |> voidKS k)
        | Global name ->
            (S.Var glob, name)
            ||> Seq.fold (fun s x -> s.[!~(S.String x)])
            |> appK k
        | IfThenElse (cond, t, e) ->
            cW sc cond <| fun cond ->
                let compilesToExpr =
                    CompilesToJavaScriptExpression t
                    && CompilesToJavaScriptExpression e
                let toStmt () =
                    match k with
                    | KIgnore | KReturn | KSet _ | KThrow ->
                        S.If (cond, block (c sc t k), block (c sc e k))
                        |> stmt
                    | KWith k ->
                        let freshVar = Scope.Id sc (Id())
                        let kS = KSet freshVar
                        let st = S.If (cond, block (c sc t kS), block (c sc e kS)) |> stmt
                        st ++ k (S.Var freshVar)
                if compilesToExpr then
                    match k with
                    | KIgnore -> toStmt ()
                    | _ -> cW sc t (fun t -> cW sc e (fun e -> appK k (S.Conditional (cond, t, e))))
                else toStmt ()
        | Lambda (this, vars, body) as orig ->
            match k with
            | K.KIgnore -> Effects.Zero
            | _ -> appK k (cLambda sc orig this vars body)
        | Let (var, value, body) ->
            c sc value (KSet (Scope.Id sc var))
            ++ c sc body k
        | LetRecursive (bindings, body) ->
            let (vars, values) = List.unzip bindings
            let vars = List.map (Scope.Id sc) vars
            Effects.Concat [
                for (var, value) in List.zip vars values do
                    match value with
                    | Constant Undefined -> ()
                    | _ -> yield c sc value (KSet var)
                yield c sc body k
            ]
        | New (f, a) ->
            cW sc f (fun f -> cList sc a (fun a -> appK k (S.New (f, a))))
        | NewArray a ->
            cList sc a (fun a -> appK k (S.NewArray (List.map Some a)))
        | NewObject o ->
            let (ks, vs) = List.unzip o
            cList sc vs (fun vs -> appK k (S.NewObject (List.zip ks vs)))
        | NewRegex x ->
            appK k (S.NewRegex x)
        | Runtime ->
            appK k (S.Var lib)
        | Sequential (x, y) ->
            c sc x KIgnore ++ c sc y k
        | Throw e ->
            c sc e KThrow
        | TryFinally (tryBlock, guard) ->
            match k with
            | KIgnore | KReturn | K.KThrow | KSet _ ->
                S.TryFinally (block (c sc tryBlock k), block (c sc guard KIgnore))
                |> stmt
            | KWith k ->
                let freshVar = Scope.Id sc (Id())
                let kS = KSet freshVar
                let st =
                    S.TryFinally (block (c sc tryBlock kS), block (c sc guard KIgnore))
                    |> stmt
                st ++ k (S.Var freshVar)
        | TryWith (tryBlock, var, guard) ->
            match k with
            | KIgnore | KReturn | KSet _ ->
                S.TryWith (block (c sc tryBlock k), Scope.Id sc var, block (c sc guard k), None)
                |> stmt
            | k ->
                let freshVar = Scope.Id sc (Id())
                let kS = KSet freshVar
                let st =
                    S.TryWith (block (c sc tryBlock kS), Scope.Id sc var, block (c sc guard kS), None)
                    |> stmt
                st ++ appK k (S.Var freshVar)
        | Unary (o, x) ->
            match o with
            | UnaryOperator.``void`` ->
                cW sc x (fun x ->
                    match k with
                    | KIgnore -> x
                    | _ ->
                        match x with
                        | S.Unary (S.UnaryOperator.``void``, _) as x -> x
                        | x -> x.Void
                    |> appK k)
            | _ ->
                cW sc x (fun x -> appK k (S.Unary (ElaborateUnaryOperator o, x)))
        | Var v ->
            appK k !^v
        | VarSet (var, value) ->
            cW sc value (fun v -> appK k (!^var ^= v))
        | WhileLoop (cond, body) ->
            if CompilesToJavaScriptExpression cond then
                cW sc cond (fun cond ->
                    S.While (cond, block (c sc body KIgnore))
                    |> voidKS k)
            else
                let expr =
                    let ok = Id()
                    Let (ok, !~ Literal.True,
                        WhileLoop (Var ok,
                            Sequential (
                                VarSet (ok, cond),
                                IfThenElse (Var ok, body, !~ Literal.Undefined)
                            )))
                c sc expr k

    and cLambda sc orig this vars body : S.Expression =
        let scope = Scope.Nest sc this vars
        let formals = List.map (Scope.Id scope) vars
        let mode =
            match body with
            | Unary (UnaryOperator.``void``, body) -> KIgnore
            | body -> KReturn
        let body = List.map S.Action <| (c scope body mode).ToStatements()
        S.Lambda (None, formals, Scope.WithVars scope body)
        |> CloseOverMutablesIfNecessary mut sc orig

    let scope = Scope.New prefs
    let main = (c scope expr KIgnore).ToStatements()
    let vars =
        S.Vars ((glob, Some S.This)
        :: (lib, Some S.This?IntelliFactory?Runtime)
        :: Scope.Vars scope)
    vars :: main
    |> List.map S.Action

exception RecognitionError

let Recognize expr =
    let rec rE this (env: Map<_,_>) used expr =
        let (!) = rE this env true
        let (!!) = List.map (!)
        match expr with
        | S.Application (S.Binary (t, SB.``.``, f), xs) ->
            Call (!t, !f, !!xs)
        | S.Application (f, xs) ->
            Application (!f, !!xs)
        | S.Binary (x, o, y) ->
            let parse = function
                | SB.``!==`` -> Some B.``!==``
                | SB.``!=`` -> Some B.``!=``
                | SB.``%`` -> Some B.``%``
                | SB.``&&`` -> Some B.``&&``
                | SB.``&`` -> Some B.``&``
                | SB.``*`` -> Some B.``*``
                | SB.``+`` -> Some B.``+``
                | SB.``-`` -> Some B.``-``
                | SB.``/`` -> Some B.``/``
                | SB.``<<`` -> Some B.``<<``
                | SB.``<=`` -> Some B.``<=``
                | SB.``<`` -> Some B.``<``
                | SB.``===`` -> Some B.``===``
                | SB.``==`` -> Some B.``==``
                | SB.``>=`` -> Some B.``>=``
                | SB.``>>>`` -> Some B.``>>>``
                | SB.``>>`` -> Some B.``>>``
                | SB.``>`` -> Some B.``>``
                | SB.``^`` -> Some B.``^``
                | SB.``in`` -> Some B.``in``
                | SB.``instanceof`` -> Some B.``instanceof``
                | SB.``|`` -> Some B.``|``
                | SB.``||`` -> Some B.``||``
                | _ -> None
            match o with
            | SB.``.`` -> FieldGet (!x, !y)
            | SB.``,`` -> Sequential (rE this env false x, !y)
            | SB.``=`` ->
                match x with
                | S.Binary (a, SB.``.``, b) ->
                    if not used then
                        FieldSet (!a, !b, !y)
                    else
                        let target = Id()
                        let field = Id ()
                        let value = Id()
                        let it = Var value
                        let assign = FieldSet (Var target, Var field, it)
                        Let (target, !a,
                            Let (field, !b,
                                Let (value, !y,
                                    Sequential (assign, it))))
                | _ -> raise RecognitionError
            | _ ->
                match parse o with
                | Some o -> Binary (!x, o, !y)
                | None -> raise RecognitionError
        | S.Conditional (a, b, c) ->
            IfThenElse (!a, !b, !c)
        | S.Constant c ->
            let rN n =
                match System.Int64.FromString n with
                | Some i -> !~ (Integer i)
                | None ->
                    match System.Double.FromString n with
                    | Some i -> !~ (Double i)
                    | None -> raise RecognitionError
            match c with
            | S.False -> !~False
            | S.Null -> !~Null
            | S.Number n -> rN n
            | S.String x -> !~(String x)
            | S.True -> !~True
        | S.Lambda (name, vars, body) ->
            match name with
            | None ->
                let this = Id()
                let env =
                    (env, vars)
                    ||> List.fold (fun env v -> Map.add v (Id v) env)
                let body =
                    body
                    |> List.map (function
                        | S.Action s -> s
                        | _ -> raise RecognitionError)
                    |> S.Block
                Lambda (
                    Some this,
                    [for v in vars -> env.[v]],
                    rS (Some this) env true body
                )
            | Some _ ->
                raise RecognitionError
        | S.NewArray x ->
            let f = function
                | None -> !~Undefined
                | Some x -> !x
            NewArray (List.map f x)
        | S.NewObject o ->
            NewObject [for (k, v) in o -> (k, !v)]
        | S.New (x, xs) ->
            New (!x, !!xs)
        | S.Postfix _ ->
            raise RecognitionError
        | S.Unary (o, e) ->
            match o with
            | SU.``~`` -> Unary (U.``~``, !e)
            | SU.``-`` -> Unary (U.``-``, !e)
            | SU.``!`` -> Unary (U.``!``, !e)
            | SU.``+`` -> Unary (U.``+``, !e)
            | SU.``typeof`` -> Unary (U.``typeof``, !e)
            | SU.``void`` -> Unary (U.``void``, rE this env false e)
            | _ -> raise RecognitionError
        | S.NewRegex x ->
            NewRegex x
        | S.This ->
            match this with
            | Some id -> Var id
            | None -> raise RecognitionError
        | S.Var id ->
            match env.TryFind id with
            | Some id -> Var id
            | None when id = "arguments" -> Arguments
            | None -> Global [id]
        | S.ExprPos (x, pos) ->
            !x |> WithPos pos
               
    and rS this (env: Map<_,_>) tail (stmt: S.Statement) =
        let (!) = rE this env true
        let rS = rS this
        match stmt with
        | S.Block x ->
            let rec f acc = function
                | [] -> acc
                | [x] -> Sequential (acc, rS env tail x)
                | x::xs -> f (Sequential (acc, rS env false x)) xs
            and g = function
                | [] -> !~Undefined
                | [x] -> rS env tail x
                | x::xs -> f (rS env false x) xs
            g (Seq.toList x)
        | S.Empty ->
            !~Undefined
        | S.If (a, b, c) ->
            IfThenElse (!a, rS env tail b, rS env tail c)
        | S.Ignore x ->
            let x = rE this env false x
            if tail then x.Void else x
        | S.Return x ->
            if tail then
                match x with
                | None -> !~Undefined
                | Some x -> !x
            else
                raise RecognitionError
        | S.Throw e ->
            Throw !e
        | S.TryFinally (a, b) ->
            TryFinally (rS env tail a, rS env false b)
        | S.TryWith (a, b, c, d) ->
            match d with
            | None ->
                let id = Id b
                let envc = Map.add b id env
                TryWith (rS env tail a, id, rS envc tail c)
            | Some _ ->
                raise RecognitionError
        | S.While (e, s) ->
            WhileLoop (!e, rS env false s)
        | S.Break _
        | S.Continue _
        | S.Debugger
        | S.Do _ 
        | S.For _
        | S.ForIn _
        | S.ForVarIn _
        | S.ForVars _
        | S.Labelled _
        | S.Switch _
        | S.Vars _
        | S.With _ ->
            raise RecognitionError
    try Some (rE None Map.empty true expr) with RecognitionError -> None
