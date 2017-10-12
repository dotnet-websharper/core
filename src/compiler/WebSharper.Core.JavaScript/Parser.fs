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

module WebSharper.Core.JavaScript.Parser

module L = Lexer
module S = Syntax
type B = S.BinaryOperator
type U = S.UnaryOperator
type P = S.PostfixOperator
type internal Sy = L.Symbol
type internal Kw = L.Keyword

type Source =
    {
        mutable current : L.IToken
        lexer : L.State
    }

    static member FromLexerState (lexer: L.State) =
        {
            current = L.InputElementRegExp lexer
            lexer = lexer
        }

    static member FromString s =
        Source.FromLexerState (L.FromString s)

    static member FromTextReader tR =
        Source.FromLexerState (L.FromTextReader tR)

let readRx i =
    let t = i.current
    i.current <- L.InputElementRegExp i.lexer
    t

let readDiv i =
    let t = i.current
    i.current <- L.InputElementDiv i.lexer
    t

let peek i = i.current
let skipRx i = ignore (readRx i)
let skipDiv i = ignore (readDiv i)

type Line = int
type Column = int

exception ParserError of Line * Column * string with
    override this.ToString() =
        System.String.Format("Parser error at line {0}, column {1}: {2}",
            this.Data0, this.Data1, this.Data2)

let error (t: L.IToken) m =
    raise (ParserError (t.Line, t.Column, m))

let symbolRx sy i =
    let t = readRx i
    match t.Lexeme with
    | L.Punctuator s when s = sy -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", sy))

let symbolDiv sy i =
    let t = readRx i
    match t.Lexeme with
    | L.Punctuator s when s = sy -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", sy))

let keywordRx kw i =
    let t = readRx i
    match t.Lexeme with
    | L.ReservedWord rw when rw = kw -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", kw))

let keywordDiv kw i =
    let t = readRx i
    match t.Lexeme with
    | L.ReservedWord rw when rw = kw -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", kw))

let (|PostfixOp|_|) (t: L.IToken) =
    if t.FollowsLineTerminator then None else
        match t.Lexeme with
        | L.Punctuator s ->
            match s with
            | Sy.``++`` -> Some P.``++``
            | Sy.``--`` -> Some P.``--``
            | _ -> None
        | _ ->
            None

let (|UnaryOp|_|) (t: L.IToken) =
    match t.Lexeme with
    | L.Punctuator s ->
        match s with
        | Sy.``++`` -> Some U.``++``
        | Sy.``--`` -> Some U.``--``
        | Sy.``+`` -> Some U.``+``
        | Sy.``-`` -> Some U.``-``
        | Sy.``~`` -> Some U.``~``
        | Sy.``!`` -> Some U.``!``
        | _ -> None
    | L.ReservedWord kw ->
        match kw with
        | Kw.``delete`` -> Some U.``delete``
        | Kw.``typeof`` -> Some U.``typeof``
        | Kw.``void`` -> Some U.``void``
        | _ -> None
    | _ ->
        None

let rec primExpr i =
    let t = peek i
    match t.Lexeme with
    | L.Identifier x -> skipDiv i; S.Var (S.Id.New x)
    | L.ReservedWord Kw.``this`` -> skipDiv i; S.This
    | L.ReservedWord Kw.``null`` -> skipDiv i; S.Constant S.Null
    | L.ReservedWord Kw.``false`` -> skipDiv i; S.Constant S.False
    | L.ReservedWord Kw.``true`` -> skipDiv i; S.Constant S.True
    | L.NumericLiteral x -> skipDiv i; S.Constant (S.Number x)
    | L.StringLiteral x -> skipDiv i; S.Constant (S.String x)
    | L.RegexLiteral x -> skipDiv i; S.NewRegex x
    | L.Punctuator Sy.``[`` -> skipRx i; arrayLiteral i
    | L.Punctuator Sy.``{`` -> skipRx i; objectLiteral i
    | L.Punctuator Sy.``(`` ->
        skipRx i
        let e = expr true i
        let t = readDiv i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> e
        | _ -> error t "Expecting ')'."
    | _ -> error t "Expecting a primary expression."

and arrayLiteral i =
    let rec elision acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``,`` -> skipRx i; elision (None :: acc)
        | _ -> acc
    let rec p0 acc =
        let acc = elision acc
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``]`` -> skipDiv i; S.NewArray (List.rev acc)
        | _ -> p1 (Some (assignExpr true i) :: acc)
    and p1 acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``]`` -> skipDiv i; S.NewArray (List.rev acc)
        | L.Punctuator Sy.``,`` -> skipRx i; p0 acc
        | _ -> error t "Expecting ']' or ','."
    p0 []

and objectLiteral i =
    let rec p0 acc =
        let t = readDiv i
        match t.Lexeme with
        | L.Punctuator Sy.``}`` -> S.NewObject (List.rev acc)
        | L.Identifier s
        | L.NumericLiteral s
        | L.StringLiteral s -> p1 s acc
        | L.ReservedWord kw -> p1 (string kw) acc
        | _ -> error t "Expecting '}' or a property name."
    and p1 n acc =
        let t = readRx i
        match t.Lexeme with
        | L.Punctuator Sy.``:`` -> p2 ((n, assignExpr true i) :: acc)
        | _ -> error t "Expecting ':'."
    and p2 acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``,`` -> skipRx i; p0 acc
        | L.Punctuator Sy.``}`` -> skipDiv i; S.NewObject (List.rev acc)
        | _ -> error t "Expecting ',' or '}'."
    p0 []

and lhsExpr i =
    let rec news k =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``new`` -> skipRx i; news (k + 1)
        | _ -> k
    let news = news 0
    let body =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``function`` -> skipRx i; funExpr i
        | _ -> primExpr i
    lhsExprTail news i body

and lhsExprTail news i body =
    let rec loop news e =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``[`` ->
            skipRx i
            let m = expr true i
            let t = readDiv i
            match t.Lexeme with
            | L.Punctuator Sy.``]`` -> loop news (S.Binary (e, B.``.``, m))
            | _ -> error t "Expecting ']'."
        | L.Punctuator Sy.``.`` ->
            skipRx i
            let t = readDiv i
            let m =
                match t.Lexeme with
                | L.Identifier x -> x
                | L.ReservedWord x -> string x
                | _ -> error t "Expecting an identifier name."
            loop news (S.Binary (e, B.``.``, S.Constant (S.String m)))
        | L.Punctuator Sy.``(`` ->
            skipRx i
            let args = arguments i
            match news with
            | 0 -> loop news (S.Application (e, args))
            | _ -> loop (news - 1) (S.New (e, args))
        | _ ->
            let rec loop news e =
                match news with
                | 0 -> e
                | _ -> loop (news - 1) (S.New (e, []))
            loop news e
    loop news body

and arguments i =
    let rec loop acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> skipDiv i; List.rev acc
        | L.Punctuator Sy.``,`` -> skipRx i; loop (assignExpr true i :: acc)
        | _ -> error t "Expecting ',' or ')'."
    match (peek i).Lexeme with
    | L.Punctuator Sy.``)`` -> skipDiv i; []
    | _ -> loop [assignExpr true i]

and postfixExpr i =
    postfixExprTail i (lhsExpr i)

and postfixExprTail i e =
    match peek i with
    | PostfixOp op -> skipDiv i; S.Postfix (e, op)
    | _ -> e

and unaryExpr i =
    let rec loop () =
        match peek i with
        | UnaryOp o -> skipRx i; S.Unary (o, loop ())
        | _ -> postfixExpr i
    loop ()

and logicalExpr allowIn i =
    logicalExprTail allowIn i (unaryExpr i)

and logicalExprTail allowIn i e =
    let parse (t: L.IToken) =
        match t.Lexeme with
        | L.Punctuator o ->
            match o with
            | Sy.``*`` -> Some B.``*``
            | Sy.``/`` -> Some B.``/``
            | Sy.``%`` -> Some B.``%``
            | Sy.``+`` -> Some B.``+``
            | Sy.``-`` -> Some B.``-``
            | Sy.``<<`` -> Some B.``<<``
            | Sy.``>>`` -> Some B.``>>``
            | Sy.``>>>`` -> Some B.``>>>``
            | Sy.``<`` -> Some B.``<``
            | Sy.``>`` -> Some B.``>``
            | Sy.``<=`` -> Some B.``<=``
            | Sy.``>=`` -> Some B.``>=``
            | Sy.``==`` -> Some B.``==``
            | Sy.``!=`` -> Some B.``!=``
            | Sy.``===`` -> Some B.``===``
            | Sy.``!==`` -> Some B.``!==``
            | Sy.``&`` -> Some B.``&``
            | Sy.``^`` -> Some B.``^``
            | Sy.``|`` -> Some B.``|``
            | Sy.``&&`` -> Some B.``&&``
            | Sy.``||`` -> Some B.``||``
            | _ -> None
        | L.ReservedWord Kw.``in`` ->
            if allowIn then Some B.``in`` else None
        | L.ReservedWord Kw.``instanceof`` ->
            Some B.``instanceof``
        | _ ->
            None
    let prec x =
        match x with
        | B.``.`` -> 0
        | B.``*`` | B.``/`` | B.``%`` -> 1
        | B.``+`` | B.``-`` -> 2
        | B.``<<`` | B.``>>`` | B.``>>>`` -> 3
        | B.``<=`` | B.``<``  | B.``>=`` | B.``>``
        | B.``in`` | B.``instanceof`` -> 4
        | B.``!==`` | B.``!=`` | B.``===`` | B.``==`` -> 5
        | B.``&`` -> 6
        | B.``^`` -> 7
        | B.``|`` -> 8
        | B.``&&`` -> 9
        | B.``||`` -> 10
        | B.``,`` -> 12
        | _ -> 11
    let app a o b = S.Binary (a, o, b)
    let rec add b o2 stack =
        match stack with
        | (a, o1) :: rest when prec o1 <= prec o2 -> add (app a o1 b) o2 rest
        | _ -> (b, o2) :: stack
    let rec reduce b stack =
        match stack with
        | (a, o) :: rest -> reduce (app a o b) rest
        | [] -> b
    let rec loop e stack =
        match parse (peek i) with
        | Some op -> skipRx i; loop (unaryExpr i) (add e op stack)
        | None -> reduce e stack
    loop e []

and condExpr allowIn i =
    condExprTail allowIn i (logicalExpr allowIn i)

and condExprTail allowIn i e =
    let t = peek i
    match t.Lexeme with
    | L.Punctuator Sy.``?`` ->
        skipRx i
        let e2 = assignExpr allowIn i
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``:`` ->
            skipRx i
            let e3 = assignExpr allowIn i
            S.Conditional (e, e2, e3)
        | _ ->
            error t "Expecting ':'."
    | _ ->
        e

and assignExpr allowIn i =
    let t = peek i
    match t with
    | UnaryOp _ -> condExpr allowIn i
    | _ -> assignExprTail allowIn i (lhsExpr i)

and assignExprTail allowIn i e =
    let parse lexeme =
        match lexeme with
        | L.Punctuator s ->
            match s with
            | Sy.``=`` -> Some B.``=``
            | Sy.``*=`` -> Some B.``*=``
            | Sy.``/=`` -> Some B.``/=``
            | Sy.``%=`` -> Some B.``%=``
            | Sy.``+=`` -> Some B.``+=``
            | Sy.``-=`` -> Some B.``-=``
            | Sy.``<<=`` -> Some B.``<<=``
            | Sy.``>>=`` -> Some B.``>>=``
            | Sy.``>>>=`` -> Some B.``>>>=``
            | Sy.``&=`` -> Some B.``&=``
            | Sy.``^=`` -> Some B.``^=``
            | Sy.``|=`` -> Some B.``|=``
            | _ -> None
        | _ ->
            None
    match parse (peek i).Lexeme with
    | Some op -> skipRx i; S.Binary (e, op, assignExpr allowIn i)
    | None -> e
                 |> postfixExprTail i
                 |> logicalExprTail allowIn i
                 |> condExprTail allowIn i

and expr allowIn i =
    exprTail allowIn i (assignExpr allowIn i)

and exprTail allowIn i e =
    let ( ++ ) a b = S.Binary (a, B.``,``, b)
    let rec loop e =
        match (peek i).Lexeme with
        | L.Punctuator Sy.``,`` -> skipRx i; loop (e ++ assignExpr allowIn i)
        | _ -> e
    loop e

and stmt i =
    let fallback () =
        let e = expr true i
        ``;`` i
        S.Ignore e
    let t = peek i
    match t.Lexeme with
    | L.ReservedWord kw ->
        match kw with
        | Kw.``var`` -> skipRx i; varStmt i
        | Kw.``if`` -> skipRx i; ifStmt i
        | Kw.``do`` -> skipRx i; doStmt i
        | Kw.``while`` -> skipRx i; whileStmt i
        | Kw.``for`` -> skipRx i; forStmt i
        | Kw.``continue`` -> skipRx i; contStmt i
        | Kw.``break`` -> skipRx i; breakStmt i
        | Kw.``return`` -> skipRx i; returnStmt i
        | Kw.``with`` -> skipRx i; withStmt i
        | Kw.``switch`` -> skipRx i; switchStmt i
        | Kw.``throw`` -> skipRx i; throwStmt i
        | Kw.``try`` -> skipRx i; tryStmt i
        | Kw.``debugger`` -> skipRx i; ``;`` i; S.Debugger
        | Kw.``function`` -> error t "Functions are not allowed in statements."
        | _ -> fallback ()
    | L.Punctuator s ->
        match s with
        | Sy.``{`` -> skipRx i; block i
        | Sy.``;`` -> skipRx i; S.Empty
        | _ -> fallback ()
    | L.Identifier id ->
        skipDiv i
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``:`` ->
            skipRx i
            S.Labelled (id, stmt i)
        | _ ->
            let e =
                S.Var (S.Id.New id)
                |> lhsExprTail 0 i
                |> assignExprTail true i
                |> exprTail true i
            ``;`` i
            S.Ignore e
    | _ ->
        fallback ()

and block i =
    let rec loop acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``}`` -> skipRx i; S.Block (List.rev acc)
        | _ -> loop (stmt i :: acc)
    loop []

and varStmt i =
    let vs = vars true i
    ``;`` i
    S.Vars vs

and vars allowIn i =
    varsTail allowIn [varDecl allowIn i] i

and varsTail allowIn acc i =
    match (peek i).Lexeme with
    | L.Punctuator Sy.``,`` ->
        skipRx i
        varsTail allowIn (varDecl allowIn i :: acc) i
    | _ ->
        List.rev acc

and varDecl allowIn i =
    let t = readRx i
    match t.Lexeme with
    | L.Identifier id ->
        match (peek i).Lexeme with
        | L.Punctuator Sy.``=`` -> skipRx i; (S.Id.New id, Some (assignExpr allowIn i))
        | _ -> (S.Id.New id, None)
    | _ ->
        error t "Expected an identifier."

and ifStmt i =
    symbolRx Sy.``(`` i
    let e = expr true i
    symbolRx Sy.``)`` i
    let s = stmt i
    match (peek i).Lexeme with
    | L.ReservedWord Kw.``else`` -> skipRx i; S.If (e, s, stmt i)
    | _ -> S.If (e, s, S.Empty)

and doStmt i =
    let s = stmt i
    keywordRx Kw.``while`` i
    symbolRx Sy.``(`` i
    let e = expr true i
    symbolRx Sy.``)`` i
    ``;`` i
    S.Do (s, e)

and whileStmt i =
    symbolRx Sy.``(`` i
    let e = expr true i
    symbolRx Sy.``)`` i
    let s = stmt i
    S.While (e, s)

and forStmt i =
    let exprOpt sy i =
        match (peek i).Lexeme with
        | L.Punctuator s when s = sy -> skipRx i; None
        | _ ->
            let e = expr true i
            symbolRx sy i; Some e
    symbolRx Sy.``(`` i
    match (peek i).Lexeme with
    | L.ReservedWord Kw.``var`` ->
        skipRx i
        let (var, value) = varDecl false i
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``in`` ->
            skipRx i
            let e = expr true i
            symbolRx Sy.``)`` i
            let s = stmt i
            S.ForVarIn (var, value, e, s)
        | _->
            let vars = varsTail false [(var, value)] i
            symbolRx Sy.``;`` i
            let e2 = exprOpt Sy.``;`` i
            let e3 = exprOpt Sy.``)`` i
            S.ForVars (vars, e2, e3, stmt i)
    | _ ->
        match (peek i).Lexeme with
        | L.Punctuator Sy.``;`` ->
            skipRx i
            let e2 = exprOpt Sy.``;`` i
            let e3 = exprOpt Sy.``)`` i
            S.For (None, e2, e3, stmt i)
        | _ ->
            let e = lhsExpr i
            match (peek i).Lexeme with
            | L.ReservedWord Kw.``in`` ->
                skipRx i
                let e2 = expr true i
                symbolRx Sy.``)`` i
                S.ForIn (e, e2, stmt i)
            | _ ->
                let e =
                    e
                    |> assignExprTail false i
                    |> exprTail false i
                symbolRx Sy.``;`` i
                let e2 = exprOpt Sy.``;`` i
                let e3 = exprOpt Sy.``)`` i
                S.For (Some e, e2, e3, stmt i)

and contStmt i =
    let t = peek i
    if t.FollowsLineTerminator then S.Continue None else
        match t.Lexeme with
        | L.Identifier id -> skipRx i; ``;`` i; S.Continue (Some id)
        | _ -> ``;`` i; S.Continue None

and breakStmt i =
    let t = peek i
    if t.FollowsLineTerminator then S.Break None else
        match t.Lexeme with
        | L.Identifier id -> skipRx i; ``;`` i; S.Break (Some id)
        | _ -> ``;`` i; S.Break None

and returnStmt i =
    let t = peek i
    if t.FollowsLineTerminator then S.Return None else
        match t.Lexeme with
        | L.EndOfInput -> S.Return None
        | L.Punctuator Sy.``;`` -> skipRx i; S.Return None
        | L.Punctuator Sy.``}`` -> S.Return None
        | _ ->
            let e = expr true i
            ``;`` i
            S.Return (Some e)

and withStmt i =
    symbolRx Sy.``(`` i
    let e = expr true i
    symbolRx Sy.``)`` i
    S.With (e, stmt i)

and switchStmt i =
    symbolRx Sy.``(`` i
    let e = expr true i
    symbolRx Sy.``)`` i
    symbolRx Sy.``{`` i
    let rec stmts acc =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``case``
        | L.ReservedWord Kw.``default``
        | L.Punctuator Sy.``}`` -> List.rev acc
        | _ -> stmts (stmt i :: acc)
    let rec loop acc =
        let t = readRx i
        match t.Lexeme with
        | L.ReservedWord Kw.``case`` ->
            let e = expr true i
            symbolRx Sy.``:`` i
            loop (S.Case (e, stmts []) :: acc)
        | L.ReservedWord Kw.``default`` ->
            symbolRx Sy.``:`` i
            loop (S.Default (stmts []) :: acc)
        | L.Punctuator Sy.``}`` ->
            S.Switch (e, List.rev acc)
        | _ ->
            error t "Expecting 'case', 'default', or '}'."
    loop []

and throwStmt i =
    let t = peek i
    if t.FollowsLineTerminator then
        error t "Illegal newline after 'throw'."
    else
        let e = expr true i
        ``;`` i
        S.Throw e

and tryStmt i =
    symbolRx Sy.``{`` i
    let b1 = block i
    let t = peek i
    let finallyOpt () =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``finally`` ->
            skipRx i
            symbolRx Sy.``{`` i
            Some (block i)
        | _ ->
            None
    let catchOpt () =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``catch`` ->
            skipRx i
            symbolRx Sy.``(`` i
            let id =
                let t = readRx i
                match t.Lexeme with
                | L.Identifier id -> id
                | _ -> error t "Expecting an identifier."
            symbolRx Sy.``)`` i
            symbolRx Sy.``{`` i
            Some (id, block i)
        | _ ->
            None
    let c = catchOpt ()
    let f = finallyOpt ()
    match c, f with
    | Some (id, b2), fin -> S.TryWith (b1, (S.Id.New id), b2, fin)
    | None, Some b2 -> S.TryFinally (b1, b2)
    | _ -> error t "Expecting `catch` or `finally`."

and ``;`` i =
    let t = peek i
    match t.Lexeme with
    | L.Punctuator Sy.``;`` -> skipRx i
    | L.EndOfInput -> ()
    | L.Punctuator Sy.``}`` -> ()
    | _ when t.FollowsLineTerminator -> ()
    | _ -> error t "Expecting ';'."

and funExpr i =
    let t = readRx i
    match t.Lexeme with
    | L.Identifier id ->
        symbolRx Sy.``(`` i
        let f = formals i
        S.Lambda (Some (S.Id.New id), f, funBody i, false)
    | L.Punctuator Sy.``(`` ->
        let f = formals i
        S.Lambda (None, f, funBody i, false)
    | _ ->
        error t "Expecting '(' or an identifier."

and funDecl i =
    let t = readRx i
    match t.Lexeme with
    | L.Identifier id ->
        symbolRx Sy.``(`` i
        let f = formals i
        S.Function (S.Id.New id, f, funBody i)
    | _ ->
        error t "Expecting an identifier."

and formals i =
    let rec s0 acc =
        let t = readRx i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> List.rev acc
        | L.Identifier id -> s1 ((S.Id.New id) :: acc)
        | _ -> error t "Expecting ')' or an identifier."
    and s1 acc =
        let t = readRx i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> List.rev acc
        | L.Punctuator Sy.``,`` -> s2 acc
        | _ -> error t "Expecting ')' or ','."
    and s2 acc =
        let t = readRx i
        match t.Lexeme with
        | L.Identifier id -> s1 ((S.Id.New id) :: acc)
        | _ -> error t "Expecting an identifier."
    s0 []

and funBody i =
    symbolRx Sy.``{`` i
    let rec loop acc =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``function`` -> skipRx i; loop (funDecl i :: acc)
        | L.Punctuator Sy.``}`` -> skipRx i; List.rev acc
        | _ -> loop ((stmt i) :: acc)
    loop []

and program i =
    let rec loop acc =
        match (peek i).Lexeme with
        | L.EndOfInput -> List.rev acc
        | L.ReservedWord Kw.``function`` -> skipRx i; loop (funDecl i :: acc)
        | _ -> loop ((stmt i) :: acc)
    loop []

let ParseExpression i =
    try
        let e = expr true i
        let tok = peek i
        match tok.Lexeme with
        | L.EndOfInput -> e
        | _ -> error tok "Expecting end of input."
    with Lexer.LexerError (line, column, message) ->
        raise (ParserError (line, column, message))

let ParseProgram i =
    try program i with
        Lexer.LexerError (line, column, message) ->
            raise (ParserError (line, column, message))
