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

module WebSharper.Compiler.Recognize

open WebSharper.Core
open WebSharper.Core.AST

module S = WebSharper.Core.JavaScript.Syntax
type SB = WebSharper.Core.JavaScript.Syntax.BinaryOperator

type Environment =
    {
        Vars : list<IDictionary<string, Expression>>
        Inputs : list<Expression>
        Labels : Map<string, Id>
        This : option<Id>
        IsPure : bool
    }

    static member New(thisArg, isDirect, isPure, args) =
        // TODO : add  `arguments` to scope
        let mainScope =
            Option.toList thisArg @ args
            |> Seq.mapi (fun i (a: Id) ->                    
                let isThis = Some a = thisArg
                let v =
                    if isDirect && isThis then This else Var a
                [ 
                    yield "$" + string i, v
                    match a.Name with
                    | Some n -> yield "$" + n, v
                    | _ -> ()
                    if isThis then yield "$this", v
                ] 
            ) |> Seq.concat |> dict
        {
            Vars = [ Dictionary(); mainScope ]
            Inputs = 
                if isDirect then [] 
                else (if Option.isSome thisArg then [This] else []) @ (args |> List.map Var)
            Labels = Map.empty
            This = None
            IsPure = isPure
        }

    static member Empty =
        {
            Vars = []
            Inputs = []
            Labels = Map.empty
            This = None
            IsPure = false
        }

    member this.WithNewScope (vars) =
        { this with 
            Vars = (Dictionary(dict vars) :> _) :: this.Vars 
            IsPure = false
        }

    member this.NewVar(name) =
        let v = Id.New name
        match this.Vars with
        | [] -> failwith "no scope"
        | h :: t ->
            h.Add(name, Var v)
            v
            //{ this with Vars = (h |> Map.add name (Var v)) :: t }, v

    member this.TryFindVar(name) =
        let rec findIn scope =
            match scope with
            | [] -> None
            | (h : IDictionary<_,_>) :: t ->
                match h.TryFind name with
                | Some _ as res -> res
                | _ -> findIn t
        findIn this.Vars

    member this.IsInput(expr) =
        this.Inputs |> List.contains expr

exception RecognitionError

let rec transformExpression (env: Environment) (expr: S.Expression) =
    let inline trE e = transformExpression env e
    let checkNotMutating a f =
        if env.IsInput a then
            failwith "arguments of inlined functions should not be mutated"
        else f a
    let mbin a op c =
        checkNotMutating (trE a) (fun ta -> MutatingBinary(ta, op, trE c)) 
    let mun a op =
        checkNotMutating (trE a) (fun ta -> MutatingUnary(op, ta))
    match expr with
    | S.Application (a, b) ->
        Application (trE a, b |> List.map trE, env.IsPure, None) 
    | S.Binary (a, b, c) ->
        match b with    
        | SB.``!=``     -> Binary(trE a, BinaryOperator.``!=``, trE c)
        | SB.``!==``    -> Binary(trE a, BinaryOperator.``!==``, trE c)
        | SB.``%``      -> Binary(trE a, BinaryOperator.``%``, trE c)
        | SB.``%=``     -> mbin a MutatingBinaryOperator.``%=`` c
        | SB.``&``      -> Binary(trE a, BinaryOperator.``&``, trE c)
        | SB.``&&``     -> Binary(trE a, BinaryOperator.``&&``, trE c)
        | SB.``&=``     -> mbin a MutatingBinaryOperator.``&=`` c
        | SB.``*``      -> Binary(trE a, BinaryOperator.``*``, trE c)
        | SB.``*=``     -> mbin a MutatingBinaryOperator.``*=`` c
        | SB.``+``      -> Binary(trE a, BinaryOperator.``+``, trE c)
        | SB.``+=``     -> mbin a MutatingBinaryOperator.``+=`` c
        | SB.``,``      -> Sequential [trE a; trE c]
        | SB.``-``      -> Binary(trE a, BinaryOperator.``-``, trE c)
        | SB.``-=``     -> mbin a MutatingBinaryOperator.``-=`` c
        | SB.``.``      -> if env.IsPure then ItemGet(trE a, trE c) else ItemGetNonPure(trE a, trE c)
        | SB.``/``      -> Binary(trE a, BinaryOperator.``/``, trE c)
        | SB.``/=``     -> mbin a MutatingBinaryOperator.``/=`` c
        | SB.``<``      -> Binary(trE a, BinaryOperator.``<``, trE c)
        | SB.``<<``     -> Binary(trE a, BinaryOperator.``<<``, trE c)
        | SB.``<<=``    -> mbin a MutatingBinaryOperator.``<<=`` c
        | SB.``<=``     -> Binary(trE a, BinaryOperator.``<=``, trE c)
        | SB.``=``      ->
            match trE a with
            | ItemGet (d, e)
            | ItemGetNonPure (d, e) -> ItemSet(d, e, trE c)
            | Var d -> checkNotMutating (Var d) (fun _ -> VarSet(d, trE c))
            | a -> failwith "invalid form at '='"
        | SB.``==``     -> Binary(trE a, BinaryOperator.``==``, trE c)
        | SB.``===``    -> Binary(trE a, BinaryOperator.``===``, trE c)
        | SB.``>``      -> Binary(trE a, BinaryOperator.``>``, trE c)
        | SB.``>>``     -> Binary(trE a, BinaryOperator.``>>``, trE c)
        | SB.``>=``     -> Binary(trE a, BinaryOperator.``>=``, trE c)
        | SB.``>>=``    -> mbin a MutatingBinaryOperator.``>>=`` c
        | SB.``>>>``    -> Binary(trE a, BinaryOperator.``>>>``, trE c)
        | SB.``>>>=``   -> mbin a MutatingBinaryOperator.``>>>=`` c
        | SB.``^``      -> Binary(trE a, BinaryOperator.``^``, trE c)
        | SB.``^=``     -> mbin a MutatingBinaryOperator.``^=`` c
        | SB.``in``     -> Binary(trE a, BinaryOperator.``in``, trE c)
        | SB.instanceof -> Binary(trE a, BinaryOperator.instanceof, trE c)
        | SB.``|``      -> Binary(trE a, BinaryOperator.``|``, trE c)
        | SB.``|=``     -> mbin a MutatingBinaryOperator.``|=`` c
        | SB.``||``     -> Binary(trE a, BinaryOperator.``||``, trE c)
        | _ -> failwith "unrecognized binary operator"
    | S.Conditional (a, b, c) ->
        Conditional (trE a, trE b, trE c)
    | S.Constant a -> 
        let rN n =
            match System.Int64.FromString n with
            | Some i -> Int64 i
            | None ->
                match System.Double.FromString n with
                | Some i -> Double i
                | None -> raise RecognitionError
        match a with
        | S.False -> Bool false
        | S.Null -> Null
        | S.Number n -> rN n
        | S.String x -> String x
        | S.True -> Bool true
        |> Value
    | S.Lambda (a, b, c) ->
       match a with
       | None ->
            let vars = b |> List.map (fun v -> Id.New v)
            let env = env.WithNewScope(Seq.zip b (vars |> Seq.map Var))
            let body =
                c
                |> List.map (function
                    | S.Action s -> s
                    | _ -> raise RecognitionError)
                |> S.Block
            Function (
                vars,
                transformStatement env body
            )
       | _ -> failwith "TODO" 
    | S.New (a, b) -> New(trE a, List.map trE b)
    | S.NewArray a -> NewArray (a |> List.map (function Some i -> trE i | _ -> Undefined))
    | S.NewObject a -> Object(a |> List.map (fun (b, c) -> b, trE c))
    | S.NewRegex a -> 
        let closingSlash = a.LastIndexOf '/'
        let flags = a.[closingSlash + 1 ..] |> Seq.map (string >> String >> Value) |> List.ofSeq
        New (Global ["RegExp"], Value (String a.[1 .. closingSlash - 1]) :: flags)
    | S.Postfix (a, b) ->
        match b with
        | S.PostfixOperator.``++`` -> mun a MutatingUnaryOperator.``()++``
        | S.PostfixOperator.``--`` -> mun a MutatingUnaryOperator.``()--``
        | _ -> failwith "unrecognized postfix operator"
    | S.This -> This
    | S.Unary (a, b) ->
        match a with
        | S.UnaryOperator.``!`` -> Unary(UnaryOperator.``!``, trE b)
        | S.UnaryOperator.``+`` -> Unary(UnaryOperator.``+``, trE b)
        | S.UnaryOperator.``++`` -> mun b MutatingUnaryOperator.``++()``
        | S.UnaryOperator.``-`` -> Unary(UnaryOperator.``-``, trE b)
        | S.UnaryOperator.``--`` -> mun b MutatingUnaryOperator.``--()``
        | S.UnaryOperator.delete -> mun b MutatingUnaryOperator.delete
        | S.UnaryOperator.typeof -> Unary(UnaryOperator.typeof, trE b)
        | S.UnaryOperator.``void`` -> Unary(UnaryOperator.``void``, trE b)
        | S.UnaryOperator.``~`` -> Unary(UnaryOperator.``~``, trE b)
        | _ -> failwith "unrecognized unary operator"
    | S.Var a ->
        match a with
        | "$global" -> Global []
        | "$wsruntime" -> Global ["IntelliFactory"; "Runtime"]
        | "undefined" -> Undefined
        | _ ->
        match env.TryFindVar a with
        | Some e -> e
        | None -> if env.IsPure then ItemGet(Global [], Value (String a)) else ItemGetNonPure(Global [], Value (String a))
    | e ->     
        failwithf "Failed to recognize: %A" e
//    | S.Postfix (a, b) ->
//        match b with
//        | S.PostfixOperator.``++`` -> 
                                                                       
and transformStatement (env: Environment) (statement: S.Statement) =
    let inline trE e = transformExpression env e
    let inline trS s = transformStatement env s
    match statement with
    | S.StatementPos(a, b) -> trS a
    | S.Block a ->
        a |> List.map trS |> CombineStatements
    | S.Break a   -> Break (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Continue a -> Continue (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Debugger -> failwith "TODO"
    | S.Do (a, b) -> DoWhile (trS a, trE b)
    | S.Empty -> Empty
    | S.For (a, b, c, d) -> 
        For (Option.map trE a, Option.map trE b, Option.map trE c, trS d) // TODO: var declarations (?)
    | S.ForIn (a, b, c) -> failwith "TODO" //
    | S.ForVarIn (a, b, c, d) -> 
        match b with
        | Some b -> failwith "TODO"
        | _ ->
        let v = env.NewVar a
        Block [
            VarDeclaration (v, Undefined)
            ForIn(v, trE c, trS d)
        ]
    | S.ForVars (a, b, c, d) -> 
        let decls =
            a |> List.map (fun (i, v) ->
                let id = env.NewVar i
                VarDeclaration(id, match v with Some v -> trE v | _ -> Undefined)
            )
        Block (
            decls @
            [
                For (None, Option.map trE b, Option.map trE c, trS d)
            ]    
        )
    | S.If (a, b, c) -> If (trE a, trS b, trS c)
    | S.Ignore a -> ExprStatement (trE a)    
    | S.Labelled (a, b)  -> failwith "TODO: recognize labels"
    | S.Return a -> Return (match a with Some v -> trE v | _ -> Undefined)
    | S.Switch (a, b)    -> failwith "TODO: recognize switch"
    | S.Throw a -> Throw (trE a)
    | S.TryFinally (a, b) -> TryFinally (trS a, trS b)
    | S.TryWith (a, b, c, d)   -> failwith "TODO: recognize try-with"
    | S.Vars a ->
        match a with
        | [var, value] -> 
            VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)
        | _ -> 
            Block (a |> List.map (fun (var, value) -> VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)))
    | S.While (a, b) -> While (trE a, trS b)
    | S.With (a, b) -> failwith "TODO"

let createInline thisArg args isPure inlineString =        
    let s = 
        inlineString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    let parsed = 
        try s |> WebSharper.Core.JavaScript.Parser.ParseExpression |> Choice1Of2
        with _ -> s |> WebSharper.Core.JavaScript.Parser.ParseProgram |> Choice2Of2 
    let b =
        match parsed with
        | Choice1Of2 e ->
            e |> transformExpression (Environment.New(thisArg, false, isPure, args))
        | Choice2Of2 p ->
            p
            |> List.map (function S.Action a -> a | _ -> failwith "TODO: function declarations in Inline" )
            |> S.Block
            |> transformStatement (Environment.New(thisArg, false, isPure, args))
            |> IgnoredStatementExpr
    makeExprInline (Option.toList thisArg @ args) b

let parseDirect thisArg args jsString =
    let s = 
        jsString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    let parsed = 
        try s |> WebSharper.Core.JavaScript.Parser.ParseExpression |> Choice1Of2
        with _ -> s |> WebSharper.Core.JavaScript.Parser.ParseProgram |> Choice2Of2 
    let body =
        match parsed with
        | Choice1Of2 e ->
            e |> transformExpression (Environment.New(thisArg, true, false, args)) |> Return 
        | Choice2Of2 p ->
            p
            |> List.map (
                function 
                | S.Action a -> a 
                | S.Function (id, args, body) -> S.Vars [ id, Some (S.Lambda(None, args, body)) ]
            )
            |> S.Block
            |> transformStatement (Environment.New(thisArg, true, false, args))
    Function(args, body)

let parseGeneratedJavaScript e =
    e |> transformExpression Environment.Empty

let parseGeneratedString s =
    s |> WebSharper.Core.JavaScript.Parser.Source.FromString
    |> WebSharper.Core.JavaScript.Parser.ParseExpression
    |> parseGeneratedJavaScript

