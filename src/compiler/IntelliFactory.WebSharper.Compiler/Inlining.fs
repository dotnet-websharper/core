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

module IntelliFactory.WebSharper.Compiler.Inlining

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>
type Queue<'T> = System.Collections.Generic.Queue<'T>

module C = IntelliFactory.JavaScript.Core
module S = IntelliFactory.JavaScript.Syntax
module P = IntelliFactory.JavaScript.Parser
module Q = IntelliFactory.WebSharper.Core.Quotations

type ParameterName = S.Id

type Pattern =
    | Compiled of string
    | Inlined of string
    | Quoted of Corrector.Correction * Q.Expression

type Inliner =
    | Function of S.Expression
    | Transformer of ((Q.Expression -> C.Expression) -> list<C.Expression> ->
        C.Expression)

type Evaluation =
    | Certain
    | Uncertain

let parseDirectTransformer (logger: Logger) loc kind (args: list<_>) core =
    let names =
        let d = Dictionary()
        match kind with
        | Instance ->
            d.["$this"] <- 0
            d.["$0"] <- 0
        | Static -> ()
        let f i x =
            let k =
                match kind with
                | Instance -> i + 1
                | Static -> i
            d.["$" + x] <- k
            d.["$" + string k] <- k
        List.iteri f args
        d
    let certain = Queue()
    let uncertain = Queue()
    let ( &&& ) a b = a && b
    let rec eval (trace: Queue<_>) expr =
        let (!) x = if x then trace else uncertain
        let evals mask xs =
            let mutable t = true
            for x in xs do
                t <- t &&& eval !t x
            t &&& mask
        match expr with
        | C.Application (a, xs) ->
            evals false (a :: xs)
        | C.Call (t, f, xs) ->
            evals false (t :: f :: xs)
        | C.FieldDelete (x, y) ->
            evals false [x; y]
        | C.FieldSet (x, y, z) ->
            evals false [x; y; z]
        | C.New (x, xs) ->
            evals false (x :: xs)
        | C.Throw x ->
            ignore (eval trace x)
            false
        | C.Binary (a, C.BinaryOperator.``&&``, b)
        | C.Binary (a, C.BinaryOperator.``||``, b) ->
            eval trace a
            &&& eval uncertain b
        | C.Global [x] ->
            match names.TryGetValue x with
            | true, x -> trace.Enqueue x
            | _ -> ()
            true
        | C.IfThenElse (a, b, c) ->
            eval trace a
            &&& eval uncertain b
            &&& eval uncertain c
        | C.Let (_, a, b) ->
            eval !(eval trace a) b
        | C.LetRecursive (bs, b) ->
            eval !(evals true (List.map snd bs)) b
        | C.Lambda (_, _, b) ->
            ignore (eval uncertain b)
            true
        | C.TryFinally (a, b) ->
            eval trace a
            &&& eval uncertain b
        | C.TryWith (a, _, b) ->
            eval trace a
            &&& eval uncertain b
        | e ->
            C.Fold (fun ok e -> ok &&& eval !ok e) true e
    ignore (eval certain core)
    let len =
        match kind with
        | Instance -> args.Length + 1
        | Static -> args.Length
    let expected = Array.init len id
    if certain.ToArray() = expected && uncertain.Count = 0 then
        let f xs =
            let xs = List.toArray xs
            if xs.Length <> len then
                logger.Log {
                    Text = "Unexpected code transformation error."
                    Priority = Error
                    Location = loc
                }
                !~C.Undefined
            else
                let rec t = function
                    | C.Global [x] as e ->
                        match x with
                        | "$global" -> C.Global []
                        | _ ->
                            match names.TryGetValue x with
                            | true, i -> xs.[i]
                            | _ -> e
                    | e -> C.Transform t e
                t core
        Some (Transformer (fun _ -> f))
    else None

let parseIndirectTransformer kind args core =
    let (names, vars) =
        let d = Dictionary()
        let v = Queue()
        match kind with
        | Instance ->
            let self = C.Id "self"
            d.["$this"] <- self
            d.["$0"] <- self
            v.Enqueue self
        | Static -> ()
        let f i (x: string) =
            let k =
                match kind with
                | Instance -> i + 1
                | Static -> i
            let arg = C.Id x
            v.Enqueue arg
            d.["$" + x] <- arg
            d.["$" + string k] <- arg
        List.iteri f args
        (d, Seq.toList v)
    let body =
        let rec replace e =
            match e with
            | C.Global ["$global"] -> C.Global []
            | C.Global [x] ->
                match names.TryGetValue x with
                | true, id -> C.Var id
                | _ -> e
            | _ -> C.Transform replace e
        replace core
    let fn = C.Lambda (None, vars, body)
    Transformer (fun _ xs -> C.Application (fn, xs))

type Use =
    | Ignored = 0
    | Symbolic = 1
    | Numeric = 2
    | Both = 3

let parseProgram kind args (program: S.Program) =
    let block x = S.Block x
    let numericName i =
        let x =
            match kind with
            | Instance -> i + 1
            | Static   -> i
        "$" + string x
    let lookup (d: Dictionary<_,_>) key =
        match d.TryGetValue key with
        | true, x -> Some x
        | _ -> None
    let (symbolics, numerics) =
        let s = Dictionary()
        let n = Dictionary()
        s.["$this"] <- 0
        match kind with
        | Instance -> n.["$0"] <- 0
        | Static -> ()
        let f i x =
            s.["$" + x] <- i + 1
            n.[numericName i] <- i + 1
        List.iteri f args
        (lookup s, lookup n)
    let uses =
        let uses = Array.create (args.Length + 1) Use.Ignored
        let rec dE () = function
            | S.Var n ->
                match symbolics n with
                | Some i -> uses.[i] <- uses.[i] ||| Use.Symbolic
                | None -> ()
                match numerics n with
                | Some i -> uses.[i] <- uses.[i] ||| Use.Numeric
                | None -> ()
            | expr ->
                S.FoldExpression dE dS () expr
        and dS () = S.FoldStatement dE dS ()
        and dPE () x =
            match x with
            | S.Action s -> dS () s
            | S.Function (_, _, p) -> dP () p
        and dP () x  = List.fold dPE () x
        dP () program
        uses
    let formals =
        let f i x =
            match uses.[i + 1] with
            | Use.Numeric | Use.Both -> numericName i
            | _ -> "$" + x
        List.mapi f args
    let vars =
        S.Vars [
            if uses.[0] ||| Use.Numeric = Use.Numeric then
                yield ("$0", Some S.This)
            if uses.[0] ||| Use.Symbolic = Use.Symbolic then
                yield ("$this", Some S.This)
            for (i, x) in List.mapi (fun i x -> (i, x)) args do
                if uses.[i] = Use.Both then
                    yield ("$" + x, Some (S.Var (numericName i)))
        ]
    S.Lambda (None, formals, S.Action vars :: program)
    |> Function

let dummy = Transformer (fun _ _ -> !~C.Undefined)

let error (log: Logger) loc text =
    log.Log {
        Location = loc
        Priority = Error
        Text = text
    }
    dummy

let parseInlined log loc kind args definition =
    try
        let e = P.ParseExpression (P.Source.FromString definition)
        match C.Recognize e with
        | Some rE ->
            match parseDirectTransformer log loc kind args rE with
            | Some t -> t
            | None -> parseIndirectTransformer kind args rE
        | None ->
            "The JavaScript definition uses language constructs that \
            cannot be inlined: " + definition
            |> error log loc
    with P.ParserError (line, col, message) ->
        "Failed to parse the JavaScript expression: " + message
        |> error log loc

let parseCompiled log loc kind args definition =
    let ret e = S.Action (S.Return (Some e))
    try
        let e = P.ParseExpression (P.Source.FromString definition)
        parseProgram kind args [ret e]
    with P.ParserError (line, col, message) ->
        P.ParseProgram (P.Source.FromString definition)
        |> parseProgram kind args

exception InlineTransformError

let parseQuoted cc (q: Q.Expression) : Inliner =
    let isCtor =
        match cc with
        | Corrector.Constructor _ -> true
        | _ -> false
    let apply f xs =
        match f with
        | C.Lambda (t, vs, body) ->
            let vs = if isCtor then vs else (Option.toList t @ vs)
            try
                List.foldBack2 (fun var value body ->
                    C.Let (var, value, body)) vs xs body
            with e -> raise InlineTransformError
        | _ -> raise InlineTransformError
    Transformer (fun t xs ->
        apply (Corrector.Correct cc (t (Q.Alpha q))) xs)

type Inline =
    {
        location : Location
        isTransformer : bool
        parameters : list<ParameterName>
        pattern : Pattern
        scope : MemberScope
    }

    member this.Quotation =
        match this.pattern with
        | Quoted (_, q) -> Some q
        | _ -> None

    member this.Key = (this.scope, this.parameters, this.pattern)
    member this.MemberScope = this.scope
    member this.IsTransformer = this.isTransformer

[<Sealed>]
type Pool(logger: Logger) =
    let cache = Dictionary()

    member this.CreateInline scope loc (ps: list<ParameterName>) (pat: Pattern) =
        let key = (scope, ps, pat)
        let isT =
            match cache.TryGetValue key with
            | true, Function _ -> false
            | true, Transformer _ -> true
            | _ ->
                let inliner =
                    match pat with
                    | Compiled pat -> parseCompiled logger loc scope ps pat
                    | Inlined pat -> parseInlined logger loc scope ps pat
                    | Quoted (cc, pat) -> parseQuoted cc pat
                cache.[key] <- inliner
                match inliner with
                | Function _ -> false
                | Transformer _ -> true
        {
            isTransformer = isT
            parameters = ps
            pattern = pat
            scope = scope
            location = loc
        }

    member this.Parse(i: Inline) =
        let k = i.Key
        match cache.TryGetValue k with
        | true, r -> r
        | _ ->
            let r =
                match i.pattern with
                | Compiled pat ->
                    parseCompiled logger i.location i.scope i.parameters pat
                | Inlined pat ->
                    parseInlined logger i.location i.scope i.parameters pat
                | Quoted (cc, pat) -> parseQuoted cc pat
            cache.[k] <- r
            r

    static member Create logger = Pool logger
