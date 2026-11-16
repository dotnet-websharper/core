// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// Recursive-descent parser for the JavaScript used in inline code:
/// ES2022 expressions and statements, without classes, modules and destructuring.
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
        /// Inside a generator function, `yield` is an operator
        mutable inGenerator : bool
        /// Inside an async function, `await` is an operator
        mutable inAsync : bool
    }

    static member FromLexerState (lexer: L.State) =
        {
            current = L.InputElementRegExp lexer
            lexer = lexer
            inGenerator = false
            inAsync = false
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
    let t = readDiv i
    match t.Lexeme with
    | L.Punctuator s when s = sy -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", sy))

let keywordRx kw i =
    let t = readRx i
    match t.Lexeme with
    | L.ReservedWord rw when rw = kw -> ()
    | _ -> error t (System.String.Format("Expecting: {0}.", kw))

let isSymbol sy i =
    match (peek i).Lexeme with
    | L.Punctuator s -> s = sy
    | _ -> false

let isIdentifier name i =
    match (peek i).Lexeme with
    | L.Identifier n -> n = name
    | _ -> false

/// Runs a parser for a function body with the given generator/async context
let inFunction (kind: S.FuncKind) i f =
    let g, a = i.inGenerator, i.inAsync
    i.inGenerator <- kind.HasGenerator
    i.inAsync <- kind.HasAsync
    try f ()
    finally
        i.inGenerator <- g
        i.inAsync <- a

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

/// Converts the items of a parenthesized expression to arrow function parameters:
/// `x`, `x = default` and a final `...rest`
let private toParams (t: L.IToken) (items: list<S.Expression>) =
    let n = List.length items
    items |> List.mapi (fun j e ->
        match S.IgnoreExprPos e with
        | S.Var id -> id
        | S.Binary (S.IgnoreEPos (S.Var id), B.``=``, d) -> id.WithDefault d
        | S.Spread (S.IgnoreEPos (S.Var id)) when j = n - 1 -> { id with Rest = true }
        | _ -> error t "Invalid arrow function parameter, only identifiers with optional default value and a rest parameter are supported."
    )

let rec primExpr i =
    let t = peek i
    match t.Lexeme with
    | L.Identifier "async" ->
        skipDiv i
        let n = peek i
        if n.FollowsLineTerminator then S.Var (S.Id.New "async") else
        match n.Lexeme with
        | L.ReservedWord Kw.``function`` ->
            skipRx i
            funExpr S.Async i
        | L.Identifier x ->
            // async x => ...
            skipDiv i
            if isSymbol Sy.``=>`` i then
                skipRx i
                arrowBody S.Async [ S.Id.New x ] i
            else error (peek i) "Expecting '=>'."
        | L.Punctuator Sy.``(`` ->
            // async (...) => ... or a call of a function named async
            skipRx i
            let args = argumentsList i
            if isSymbol Sy.``=>`` i && not (peek i).FollowsLineTerminator then
                skipRx i
                arrowBody S.Async (toParams n args) i
            else
                S.Application (S.Var (S.Id.New "async"), [], args)
        | _ -> S.Var (S.Id.New "async")
    | L.Identifier x ->
        skipDiv i
        if isSymbol Sy.``=>`` i then
            skipRx i
            arrowBody S.Normal [ S.Id.New x ] i
        else S.Var (S.Id.New x)
    | L.ReservedWord Kw.``this`` -> skipDiv i; S.This
    | L.ReservedWord Kw.``super`` -> skipDiv i; S.Super
    | L.ReservedWord Kw.``null`` -> skipDiv i; S.Constant S.Null
    | L.ReservedWord Kw.``false`` -> skipDiv i; S.Constant S.False
    | L.ReservedWord Kw.``true`` -> skipDiv i; S.Constant S.True
    | L.ReservedWord Kw.``import`` -> skipDiv i; S.ImportFunc
    | L.ReservedWord Kw.``class`` -> error t "Currently unsupported: class expressions in inline JavaScript."
    | L.NumericLiteral x -> skipDiv i; S.Constant (S.Number x)
    | L.StringLiteral x -> skipDiv i; S.Constant (S.String x)
    | L.RegexLiteral x -> skipDiv i; S.NewRegex x
    | L.TemplateString _ -> templateLiteral None i
    | L.Punctuator Sy.``[`` -> skipRx i; arrayLiteral i
    | L.Punctuator Sy.``{`` -> skipRx i; objectLiteral i
    | L.Punctuator Sy.``(`` ->
        skipRx i
        // parenthesized expression or arrow function parameters
        let items = argumentsList i
        if isSymbol Sy.``=>`` i then
            skipRx i
            arrowBody S.Normal (toParams t items) i
        else
            match items with
            | [] -> error t "Expecting a primary expression."
            | _ ->
                if items |> List.exists (function S.Spread _ -> true | _ -> false) then
                    error t "Unexpected spread in parenthesized expression."
                items |> List.reduce (fun a b -> S.Binary (a, B.``,``, b))
    | _ -> error t "Expecting a primary expression."

/// Parses the body of an arrow function after `=>`
and arrowBody kind pars i =
    inFunction kind i <| fun () ->
        match (peek i).Lexeme with
        | L.Punctuator Sy.``{`` -> S.Lambda (None, pars, funBody i, true, kind)
        | _ ->
            let e = assignExpr true i
            S.Lambda (None, pars, [S.Return (Some e)], true, kind)

/// Parses a template literal starting with the current template token
and templateLiteral tag i =
    let rec loop parts holes =
        let t = peek i
        match t.Lexeme with
        | L.TemplateString (s, true) ->
            skipDiv i
            List.rev (s :: parts), List.rev holes
        | L.TemplateString (s, false) ->
            skipRx i
            let e = expr true i
            match (peek i).Lexeme with
            | L.Punctuator Sy.``}`` ->
                // continue reading the template from after the closing brace
                i.current <- L.TemplateContinuation i.lexer
                loop (s :: parts) (e :: holes)
            | _ -> error (peek i) "Expecting '}' in template literal."
        | _ -> error t "Expecting template literal."
    let parts, holes = loop [] []
    S.TemplateLiteral (tag, parts, holes)

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
        | _ -> p1 (Some (spreadOrAssignExpr i) :: acc)
    and p1 acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``]`` -> skipDiv i; S.NewArray (List.rev acc)
        | L.Punctuator Sy.``,`` -> skipRx i; p0 acc
        | _ -> error t "Expecting ']' or ','."
    p0 []

and spreadOrAssignExpr i =
    match (peek i).Lexeme with
    | L.Punctuator Sy.``...`` ->
        skipRx i
        S.Spread (assignExpr true i)
    | _ -> assignExpr true i

/// Reads a property name, returns the name or a computed key expression
and propertyName i =
    let t = readDiv i
    match t.Lexeme with
    | L.Identifier s
    | L.StringLiteral s -> Choice1Of2 s
    | L.NumericLiteral s -> Choice1Of2 s
    | L.ReservedWord kw -> Choice1Of2 (string kw)
    | L.Punctuator Sy.``[`` ->
        let e = assignExpr true i
        symbolDiv Sy.``]`` i
        Choice2Of2 e
    | _ -> error t "Expecting a property name."

and objectLiteral i =
    let field name acc value =
        match name with
        | Choice1Of2 n -> (n, acc, value)
        | Choice2Of2 k -> ("", acc, S.ComputedMember (k, value))
    let methodValue kind i =
        symbolRx Sy.``(`` i
        let f = formals i
        S.Lambda (None, f, inFunction kind i (fun () -> funBody i), false, kind)
    let isNameStart i =
        match (peek i).Lexeme with
        | L.Identifier _ | L.StringLiteral _ | L.NumericLiteral _ | L.ReservedWord _
        | L.Punctuator Sy.``[`` -> true
        | _ -> false
    let rec p0 acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``}`` -> skipDiv i; S.NewObject (List.rev acc)
        | L.Punctuator Sy.``...`` ->
            skipRx i
            next (("", S.Simple, S.Spread (assignExpr true i)) :: acc)
        | L.Punctuator Sy.``*`` ->
            skipDiv i
            let n = propertyName i
            next (field n S.Simple (methodValue S.Generator i) :: acc)
        | L.Identifier (("get" | "set" | "async") as w) ->
            skipDiv i
            if isNameStart i || (w = "async" && isSymbol Sy.``*`` i && not (peek i).FollowsLineTerminator) then
                match w with
                | "get" ->
                    let n = propertyName i
                    next (field n S.Get (methodValue S.Normal i) :: acc)
                | "set" ->
                    let n = propertyName i
                    next (field n S.Set (methodValue S.Normal i) :: acc)
                | _ ->
                    let kind =
                        if isSymbol Sy.``*`` i then skipDiv i; S.AsyncGenerator
                        else S.Async
                    let n = propertyName i
                    next (field n S.Simple (methodValue kind i) :: acc)
            else
                p1 (Choice1Of2 w) true acc
        | _ ->
            let isId = match t.Lexeme with L.Identifier _ -> true | _ -> false
            p1 (propertyName i) isId acc
    and p1 n isId acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``:`` ->
            skipRx i
            next (field n S.Simple (assignExpr true i) :: acc)
        | L.Punctuator Sy.``(`` ->
            next (field n S.Simple (methodValue S.Normal i) :: acc)
        | L.Punctuator (Sy.``,`` | Sy.``}``) when isId ->
            // shorthand property { x }
            match n with
            | Choice1Of2 name -> next ((name, S.Simple, S.Var (S.Id.New name)) :: acc)
            | _ -> error t "Expecting ':'."
        | _ -> error t "Expecting ':'."
    and next acc =
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
        | L.ReservedWord Kw.``function`` -> skipRx i; funExpr S.Normal i
        | _ -> primExpr i
    lhsExprTail news i body

and memberName i =
    let t = readDiv i
    match t.Lexeme with
    | L.Identifier x -> x
    | L.ReservedWord x -> string x
    | _ -> error t "Expecting an identifier name."

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
            let m = memberName i
            loop news (S.Binary (e, B.``.``, S.Constant (S.String m)))
        | L.Punctuator Sy.``?.`` ->
            if news > 0 then error t "Optional chaining is not allowed in a new expression."
            skipRx i
            match (peek i).Lexeme with
            | L.Punctuator Sy.``(`` ->
                skipRx i
                let args = arguments i
                loop news (S.OptionalApplication (e, args))
            | L.Punctuator Sy.``[`` ->
                skipRx i
                let m = expr true i
                symbolDiv Sy.``]`` i
                loop news (S.Binary (e, B.``?.``, m))
            | _ ->
                let m = memberName i
                loop news (S.Binary (e, B.``?.``, S.Constant (S.String m)))
        | L.TemplateString _ ->
            loop news (templateLiteral (Some e) i)
        | L.Punctuator Sy.``(`` ->
            skipRx i
            let args = arguments i
            match news with
            | 0 -> loop news (S.Application (e, [], args))
            | _ -> loop (news - 1) (S.New (e, [], args))
        | _ ->
            let rec loop news e =
                match news with
                | 0 -> e
                | _ -> loop (news - 1) (S.New (e, [], []))
            loop news e
    loop news body

/// Arguments after `(`, up to and including `)`, with spreads and a trailing comma
and argumentsList i =
    let rec loop acc =
        let t = peek i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> skipDiv i; List.rev acc
        | _ ->
            let e = spreadOrAssignExpr i
            let t = peek i
            match t.Lexeme with
            | L.Punctuator Sy.``)`` -> skipDiv i; List.rev (e :: acc)
            | L.Punctuator Sy.``,`` -> skipRx i; loop (e :: acc)
            | _ -> error t "Expecting ',' or ')'."
    loop []

and arguments i = argumentsList i

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
        | t when i.inAsync && (match t.Lexeme with L.Identifier "await" -> true | _ -> false) ->
            skipRx i
            S.Await (loop ())
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
            | Sy.``**`` -> Some B.``**``
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
            | Sy.``??`` -> Some B.``??``
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
        | B.``**`` -> 1
        | B.``*`` | B.``/`` | B.``%`` -> 2
        | B.``+`` | B.``-`` -> 3
        | B.``<<`` | B.``>>`` | B.``>>>`` -> 4
        | B.``<=`` | B.``<``  | B.``>=`` | B.``>``
        | B.``in`` | B.``instanceof`` -> 5
        | B.``!==`` | B.``!=`` | B.``===`` | B.``==`` -> 6
        | B.``&`` -> 7
        | B.``^`` -> 8
        | B.``|`` -> 9
        | B.``&&`` -> 10
        | B.``||`` | B.``??`` -> 11
        | B.``,`` -> 13
        | _ -> 12
    let app a o b = S.Binary (a, o, b)
    let rec add b o2 stack =
        match stack with
        | _ when o2 = B.``**`` -> (b, o2) :: stack
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
        let e2 = assignExpr true i
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

and yieldExpr allowIn i =
    skipRx i
    let t = peek i
    let isDelegating =
        match t.Lexeme with
        | L.Punctuator Sy.``*`` when not t.FollowsLineTerminator -> skipRx i; true
        | _ -> false
    let t = peek i
    let hasValue =
        isDelegating ||
        not t.FollowsLineTerminator &&
            match t.Lexeme with
            | L.Punctuator (Sy.``)`` | Sy.``]`` | Sy.``}`` | Sy.``,`` | Sy.``;`` | Sy.``:``)
            | L.EndOfInput -> false
            | L.ReservedWord Kw.``in`` -> false
            | _ -> true
    S.Yield ((if hasValue then Some (assignExpr allowIn i) else None), isDelegating)

and assignExpr allowIn i =
    let t = peek i
    match t with
    | UnaryOp _ -> condExpr allowIn i
    | _ when i.inGenerator && (match t.Lexeme with L.Identifier "yield" -> true | _ -> false) ->
        yieldExpr allowIn i
    | _ when i.inAsync && (match t.Lexeme with L.Identifier "await" -> true | _ -> false) ->
        condExpr allowIn i
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
            | Sy.``**=`` -> Some B.``**=``
            | Sy.``??=`` -> Some B.``??=``
            | Sy.``&&=`` -> Some B.``&&=``
            | Sy.``||=`` -> Some B.``||=``
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

/// Checks if `let` starts a declaration
and isLetDeclaration i =
    match (peek i).Lexeme with
    | L.Identifier _ | L.Punctuator (Sy.``[`` | Sy.``{``) -> true
    | _ -> false

and stmt i =
    let fallback () =
        let e = expr true i
        ``;`` i
        S.Ignore e
    let t = peek i
    match t.Lexeme with
    | L.ReservedWord kw ->
        match kw with
        | Kw.``var`` -> skipRx i; varStmt S.VarDecl i
        | Kw.``const`` -> skipRx i; varStmt S.ConstDecl i
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
        | Kw.``function`` -> skipRx i; funDecl S.Normal i
        | Kw.``class`` -> error t "Currently unsupported: class declarations in inline JavaScript."
        | _ -> fallback ()
    | L.Punctuator s ->
        match s with
        | Sy.``{`` -> skipRx i; block i
        | Sy.``;`` -> skipRx i; S.Empty
        | _ -> fallback ()
    | L.Identifier id ->
        skipDiv i
        let t = peek i
        match id, t.Lexeme with
        | _, L.Punctuator Sy.``:`` ->
            skipRx i
            S.Labelled (id, stmt i)
        | "let", _ when isLetDeclaration i ->
            varStmt S.LetDecl i
        | "async", L.ReservedWord Kw.``function`` when not t.FollowsLineTerminator ->
            skipRx i
            funDecl S.Async i
        | "async", _ ->
            // async arrow function or a call of a function named async
            let e =
                match t.Lexeme with
                | L.Identifier x when not t.FollowsLineTerminator ->
                    skipDiv i
                    if isSymbol Sy.``=>`` i then
                        skipRx i
                        arrowBody S.Async [ S.Id.New x ] i
                    else error (peek i) "Expecting '=>'."
                | L.Punctuator Sy.``(`` when not t.FollowsLineTerminator ->
                    skipRx i
                    let args = argumentsList i
                    if isSymbol Sy.``=>`` i then
                        skipRx i
                        arrowBody S.Async (toParams t args) i
                    else
                        S.Application (S.Var (S.Id.New "async"), [], args)
                | _ -> S.Var (S.Id.New "async")
            let e =
                e
                |> lhsExprTail 0 i
                |> assignExprTail true i
                |> exprTail true i
            ``;`` i
            S.Ignore e
        | ("yield" | "await"), _ when (id = "yield" && i.inGenerator) || (id = "await" && i.inAsync) ->
            // give back the keyword as the current token is already the next one
            let e =
                if id = "yield" then
                    let t = peek i
                    let isDelegating =
                        match t.Lexeme with
                        | L.Punctuator Sy.``*`` when not t.FollowsLineTerminator -> skipRx i; true
                        | _ -> false
                    let t = peek i
                    let hasValue =
                        isDelegating ||
                        not t.FollowsLineTerminator &&
                            match t.Lexeme with
                            | L.Punctuator (Sy.``)`` | Sy.``]`` | Sy.``}`` | Sy.``,`` | Sy.``;``)
                            | L.EndOfInput -> false
                            | _ -> true
                    S.Yield ((if hasValue then Some (assignExpr true i) else None), isDelegating)
                else
                    S.Await (unaryExpr i)
                    |> logicalExprTail true i
                    |> condExprTail true i
            let e = e |> exprTail true i
            ``;`` i
            S.Ignore e
        | _ ->
            let e =
                if isSymbol Sy.``=>`` i then
                    skipRx i
                    arrowBody S.Normal [ S.Id.New id ] i
                else S.Var (S.Id.New id)
            let e =
                e
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

and varStmt kind i =
    let vs = vars true i
    ``;`` i
    S.Vars (vs, kind)

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
    | L.Punctuator (Sy.``[`` | Sy.``{``) ->
        error t "Currently unsupported: destructuring in inline JavaScript."
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
    let declKind =
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``var`` -> skipRx i; Some S.VarDecl
        | L.ReservedWord Kw.``const`` -> skipRx i; Some S.ConstDecl
        | L.Identifier "let" ->
            let t = peek i
            skipDiv i
            if isLetDeclaration i then Some S.LetDecl
            else error t "Unsupported: `let` as a variable name in for statement."
        | _ -> None
    match declKind with
    | Some kind ->
        let (var, value) = varDecl false i
        match (peek i).Lexeme with
        | L.ReservedWord Kw.``in`` ->
            skipRx i
            let e = expr true i
            symbolRx Sy.``)`` i
            let s = stmt i
            match kind, value with
            | S.VarDecl, _ -> S.ForVarIn (var, value, e, s)
            | _, None -> S.ForDeclIn (kind, var, e, s)
            | _ -> error (peek i) "Initializer not allowed in for...in."
        | L.Identifier "of" when Option.isNone value ->
            skipRx i
            let e = assignExpr true i
            symbolRx Sy.``)`` i
            S.ForDeclOf (kind, var, e, stmt i)
        | _->
            let vars = varsTail false [(var, value)] i
            symbolRx Sy.``;`` i
            let e2 = exprOpt Sy.``;`` i
            let e3 = exprOpt Sy.``)`` i
            S.ForVars (vars, e2, e3, stmt i)
    | None ->
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
            | L.Identifier "of" ->
                skipRx i
                let e2 = assignExpr true i
                symbolRx Sy.``)`` i
                S.ForOf (e, e2, stmt i)
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
            let id =
                match (peek i).Lexeme with
                | L.Punctuator Sy.``(`` ->
                    skipRx i
                    let t = readRx i
                    let id =
                        match t.Lexeme with
                        | L.Identifier id -> id
                        | _ -> error t "Expecting an identifier."
                    symbolRx Sy.``)`` i
                    id
                | _ ->
                    // optional catch binding
                    "_"
            symbolRx Sy.``{`` i
            Some (id, block i)
        | _ ->
            None
    let c = catchOpt ()
    let f = finallyOpt ()
    match c, f with
    | Some (id, b2), fin -> S.TryWith (b1, S.Id.New id, b2, fin)
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

/// Function expression after `function`
and funExpr kind i =
    let kind =
        if isSymbol Sy.``*`` i then
            skipRx i
            if kind = S.Async then S.AsyncGenerator else S.Generator
        else kind
    let t = readRx i
    match t.Lexeme with
    | L.Identifier id ->
        symbolRx Sy.``(`` i
        let f = formals i
        S.Lambda (Some (S.Id.New id), f, inFunction kind i (fun () -> funBody i), false, kind)
    | L.Punctuator Sy.``(`` ->
        let f = formals i
        S.Lambda (None, f, inFunction kind i (fun () -> funBody i), false, kind)
    | _ ->
        error t "Expecting '(' or an identifier."

/// Function declaration after `function`
and funDecl kind i =
    let kind =
        if isSymbol Sy.``*`` i then
            skipRx i
            if kind = S.Async then S.AsyncGenerator else S.Generator
        else kind
    let t = readRx i
    match t.Lexeme with
    | L.Identifier id ->
        symbolRx Sy.``(`` i
        let f = formals i
        S.Function (S.Id.New id, f, Some (inFunction kind i (fun () -> funBody i)), kind)
    | _ ->
        error t "Expecting an identifier."

/// Formal parameters after `(`, up to and including `)`
and formals i =
    let rec s0 acc =
        let t = readRx i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> List.rev acc
        | L.Identifier id -> s1 (S.Id.New id) acc
        | L.Punctuator Sy.``...`` ->
            let t = readRx i
            match t.Lexeme with
            | L.Identifier id ->
                symbolRx Sy.``)`` i
                List.rev (S.Id.New(id, rest = true) :: acc)
            | _ -> error t "Expecting an identifier."
        | L.Punctuator (Sy.``[`` | Sy.``{``) ->
            error t "Currently unsupported: destructuring in inline JavaScript."
        | _ -> error t "Expecting ')' or an identifier."
    and s1 id acc =
        let id =
            match (peek i).Lexeme with
            | L.Punctuator Sy.``=`` ->
                skipRx i
                id.WithDefault (assignExpr true i)
            | _ -> id
        let t = readRx i
        match t.Lexeme with
        | L.Punctuator Sy.``)`` -> List.rev (id :: acc)
        | L.Punctuator Sy.``,`` -> s0 (id :: acc)
        | _ -> error t "Expecting ')' or ','."
    s0 []

and funBody i =
    symbolRx Sy.``{`` i
    let rec loop acc =
        match (peek i).Lexeme with
        | L.Punctuator Sy.``}`` -> skipRx i; List.rev acc
        | _ -> loop ((stmt i) :: acc)
    loop []

and program i =
    let rec loop acc =
        match (peek i).Lexeme with
        | L.EndOfInput -> List.rev acc
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
