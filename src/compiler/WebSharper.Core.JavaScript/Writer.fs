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

module WebSharper.Core.JavaScript.Writer

module S = Syntax
type StringBuilder = System.Text.StringBuilder
type StringWriter = System.IO.StringWriter
type TextWriter = System.IO.TextWriter

type Layout =
    | Empty
    | Token of string
    | Word of string
    | Horizontal of Layout * Layout
    | Vertical of Layout * Layout
    | Indent of Layout
    | SourceMapping of S.SourcePos
    | SourceMappingEnd of S.SourcePos
    | SourceName of string

let inline ( ++ ) a b = Horizontal (a, b)
let inline ( -- ) a b = Vertical (a, b)

type Associativity =
    | LeftAssociative
    | NonAssociative
    | RightAssociative

type B = S.BinaryOperator

let BinaryOperatorPrecedence operator =
    match operator with
    | B.``.`` -> 1
    | B.``*`` | B.``/`` | B.``%`` -> 5
    | B.``+`` | B.``-`` -> 6
    | B.``<<`` | B.``>>`` | B.``>>>`` -> 7
    | B.``<`` | B.``<=`` | B.``>`` | B.``>=``
    | B.``in`` | B.``instanceof`` -> 8
    | B.``==`` | B.``!=`` | B.``===`` | B.``!==`` -> 9
    | B.``&`` -> 10
    | B.``^`` -> 11
    | B.``|`` -> 12
    | B.``&&`` -> 13
    | B.``||`` -> 14
    | B.``,`` -> 17
    | _ -> 16

let BinaryOperatorAssociativity operator =
    match operator with
    | B.``=`` | B.``+=`` | B.``-=`` | B.``*=`` | B.``/=`` | B.``%=``
    | B.``<<=`` | B.``>>=`` | B.``>>>=`` | B.``&=`` | B.``^=`` | B.``|=`` ->
        RightAssociative
    | _ ->
        LeftAssociative

[<Literal>]
let PrefixOperatorPrecedence =
    4

let PrefixOperatorAssociativity operator =
    match operator with
    | S.UnaryOperator.``++``
    | S.UnaryOperator.``--`` -> NonAssociative
    | _ -> RightAssociative

[<Literal>]
let PostfixOperatorPrecedence =
    4

let PostfixOperatorAssociativity =
    NonAssociative

let Precedence expression =
    match S.IgnoreExprPos expression with
    | S.Cast _
    | S.Lambda(_, _, _, true)
    | S.Application _ -> 2
    | S.Binary (_, op, _) -> BinaryOperatorPrecedence op
    | S.Conditional _ -> 15
    | S.New _ -> 1
    | S.Postfix (_, op) -> PostfixOperatorPrecedence
    | S.Unary (op, _) -> PrefixOperatorPrecedence
    | _ -> 0

let Associativity expression =
    match S.IgnoreExprPos expression with
    | S.Application _ -> LeftAssociative
    | S.Binary (_, op, _) -> BinaryOperatorAssociativity op
    | S.Conditional _ -> RightAssociative
    | S.New _ -> RightAssociative
    | S.Postfix (_, _) -> PostfixOperatorAssociativity
    | S.Unary (op, _) -> PrefixOperatorAssociativity op
    | _ -> NonAssociative

let Keywords =
    System.Collections.Generic.HashSet [|
        "break"
        "case"
        "catch"
        "class"
        "const"
        "continue"
        "debugger"
        "default"
        "delete"
        "do"
        "else"
        "enum"
        "export"
        "extends"
        "finally"
        "for"
        "function"
        "if"
        "implements"
        "import"
        "in"
        "instanceof"
        "interface"
        "let"
        "new"
        "package"
        "private"
        "protected"
        "public"
        "return"
        "static"
        "super"
        "switch"
        "this"
        "throw"
        "try"
        "typeof"
        "var"
        "void"
        "while"
        "with"
        "yield"
    |]
let IsKeyword word = Keywords.Contains word

let WriteUnicodeEscapeCodeForId (buf: StringBuilder) (c: char) =
    buf.AppendFormat(@"u{0:x4}", int c)
    |> ignore

let EscapeId (id: string) =
    let buf = StringBuilder()
    if System.String.IsNullOrEmpty id then
        "$$EMPTY_ID_ERROR$$"
        //invalidArg "id" "Cannot escape null and empty identifiers."
    else
    let isFirst = function
        | '_' | '$' -> true
        | c when System.Char.IsLetter c -> true
        | _ -> false
    let isFollow c =
        isFirst c || System.Char.IsDigit c
    let writeChar i =
        let c = id.[i]
        if isFollow c then
            buf.Append c |> ignore
        else
            WriteUnicodeEscapeCodeForId buf c
    match id.[0] with
    | k when not (isFirst k) || IsKeyword id ->
        WriteUnicodeEscapeCodeForId buf id.[0]
        for i in 1 .. id.Length - 1 do
            writeChar i
    | _ ->
        for i in 0 .. id.Length - 1 do
            writeChar i
    string buf

let WriteUnicodeEscape (buf: StringBuilder) (c: char) =
    buf.AppendFormat(@"\u{0:x4}", int c)
    |> ignore

let QuoteString (s: string) =
    let buf = StringBuilder()
    buf.Append '"' |> ignore
    for c in s do
        match c with
        | '\r' ->
            buf.Append "\\r" |> ignore
        | '\n' ->
            buf.Append "\\n" |> ignore
        | '\\' | '"' ->
            buf.Append '\\' |> ignore
            buf.Append c |> ignore
        | c ->
            if int c < 256 && not (System.Char.IsControl c) then
                buf.Append c |> ignore
            else
                WriteUnicodeEscape buf c
    buf.Append '"' |> ignore
    string buf

let BracketString (s: string) =
    "[" + QuoteString s + "]"

let ListLayout separator brush items =
    match items with
    | [] -> Empty
    | _ -> Seq.map brush items
            |> Seq.reduce (fun a b -> separator a b)

let CommaSeparated brush items =
    ListLayout (fun a b -> a ++ Token ", " ++ b) brush items

let Parens layout =
    Token "(" ++ layout ++ Token ")"

let Optional f item =
    match item with
    | None -> Empty
    | Some x -> f x

let OptionalList f items =
    match items with
    | [] -> Empty
    | _ -> f items

let Conditional layout pred =
    if pred then layout else Empty

let BlockLayout items =
    if List.isEmpty items then 
        Word "{ }" 
    else
        Word "{"
        -- ListLayout (fun a b -> a -- b) Indent items
        -- Word "}"

let inline Label (l: S.Label) =
    Word l

let rec startsWithObjectExpression = function
    | S.IgnoreEPos (S.NewObject _) -> true
    | S.IgnoreEPos (S.Binary(e, _, _) | S.Application(e, _, _) | S.Conditional(e, _, _)) -> startsWithObjectExpression e
    | _ -> false

let rec Id (id: S.Id) =
    Conditional (Token "...") id.Rest
    ++ Word (if id.IsTypeName then id.Name else (if id.IsPrivate then "#" else "") + EscapeId id.Name)
    ++ Generics id.Generics
    ++ Conditional (Token "?") id.Optional
    ++ TypeAnnotation id.Type

and IdOrString (id: S.Id) =
    let priv = if id.IsPrivate then "#" else ""
    Conditional (Token "...") id.Rest
    ++ if Identifier.IsValid id.Name then Word (priv + id.Name) else Token (BracketString (priv + id.Name))
    ++ Generics id.Generics
    ++ Conditional (Token "?") id.Optional
    ++ TypeAnnotation id.Type

and IdAndModifiers (id: S.Id, modifiers: S.Modifiers) =
    (if modifiers.HasFlag S.Modifiers.Private then Word "private" else Empty)
    ++ (if modifiers.HasFlag S.Modifiers.Public then Word "public" else Empty)
    ++ (if modifiers.HasFlag S.Modifiers.ReadOnly then Word "readonly" else Empty)
    ++ Id id

and Generics items =
    OptionalList (fun g -> Token "<" ++ CommaSeparated Id g ++ Token ">") items

and TypeAnnotation a =
    Optional (fun t -> Token ":" ++ Expression t) a

and Expression (expression) =
    match expression with
    | S.ExprPos (x, pos) -> 
        SourceMapping pos ++ Expression x ++ SourceMappingEnd pos
    | S.ExprComment (x, c) ->
        Expression x ++ Word ("/*" + c +  "*/")
    | S.Application (f, ts, xs) ->
        let call = MemberExpression f
        let args = Parens (CommaSeparated AssignmentExpression xs)
        call ++ Generics ts ++ args
    | S.NewArray xs ->
        let element = function
            | None -> Token ", "
            | Some x -> AssignmentExpression x
        Token "[" ++ CommaSeparated element xs ++ Token "]"
    | S.Binary (x, op, y) ->
        match op, y with
        | B.``.``, S.IgnoreEPos (S.Constant (S.String y)) when Identifier.IsValid y ->
            let e = Expression x
            let eL =
                match S.IgnoreExprPos x with
                | S.Constant (S.Number _) -> Parens e
                | S.Application _
                | S.Binary (_, B.``.``, _)
                | S.This
                | S.Super
                | S.Var _
                | S.Constant _
                | S.NewArray _
                | S.NewObject _ -> e
                | _ -> Parens e
            eL ++ Token "." ++ Word y
        | B.``.``, _ ->
            MemberExpression x ++
            Token "[" ++ Expression y ++ Token "]"
        | _ ->
            let p = BinaryOperatorPrecedence op
            let (pL, pR) =
                match BinaryOperatorAssociativity op with
                | LeftAssociative -> (p + 1, p)
                | RightAssociative -> (p, p + 1)
                | NonAssociative -> (p, p)
            ParensExpression pL x
            ++ match op with
               | B.``in`` | B.``instanceof`` -> Word (string op)
               | _ -> Token (string op)
            ++ ParensExpression pR y
    | S.Constant x ->
        match x with
        | S.Null -> Word "null"
        | S.True -> Word "true"
        | S.False -> Word "false"
        | S.Number x -> Word x
        | S.String x -> Token (QuoteString x)
    | S.Conditional (a, b, c) ->
        LogicalOrExpression a
        ++ Token "?"
        ++ AssignmentExpression b
        ++ Token ":"
        ++ AssignmentExpression c
    | S.VarNamed (x, n) ->
        SourceName n ++ Id x
    | S.Var x ->
        Id x
    | S.Lambda (name, formals, body, isArrow) ->
        let args = Parens (CommaSeparated Id formals) 
        if isArrow then
            match body with
            | [ S.IgnoreSPos (S.Return None) ] -> args ++ Word " =>" ++ Word " { }"
            | [ S.IgnoreSPos (S.Return (Some e)) ] -> 
                args ++ Word " =>" 
                ++ (if startsWithObjectExpression e then Parens (Expression e) else Expression e)
            | _ -> args ++ Word " =>" ++ BlockLayout (List.map (Statement true) body)
        else
            Word "function"
            ++ Optional Id name
            ++ args
            ++ BlockLayout (List.map (Statement true) body)
    | S.New (x, ts, xs) ->
        let call = Word "new" ++ MemberExpression x
        let args = Parens (CommaSeparated AssignmentExpression xs)
        call ++ Generics ts ++ args
    | S.NewObject [] ->
        Token "{}"
    | S.NewObject fields ->
        let mem (k, a, v) =
            match v with 
            | S.IgnoreEPos (S.Lambda (_, args, body, false)) -> 
                Accessor a
                ++ Word (if Identifier.IsValid k then k else BracketString k)
                ++ Parens (CommaSeparated Id args) 
                ++ BlockLayout (List.map (Statement true) body)
            | _ ->
                if a <> S.Simple then 
                    failwithf "Unexpected: non-function used as getter or setter: %A" v
                Token (if Identifier.IsValid k then k else QuoteString k)
                ++ Token ":"
                ++ AssignmentExpression v
        if fields.Length <= 2 then
            Token "{"
            ++ ListLayout (fun a b -> a ++ Token ", " ++ b) mem fields
            ++ Token "}"
        else
            Token "{"
            -- Indent (ListLayout (fun a b -> a ++ Token ", " -- b) mem fields)
            -- Token "}"
    | S.Postfix (x, op) ->
        ParensExpression PostfixOperatorPrecedence x
        ++ Token (string op)
    | S.Unary (op, x) ->
        match op with
        | S.UnaryOperator.``delete``
        | S.UnaryOperator.``typeof``
        | S.UnaryOperator.``void`` -> Word (string op)
        | _ -> Token (string op)
        ++ ParensExpression PrefixOperatorPrecedence x
    | S.NewRegex x ->
        Token (string x)
    | S.This ->
        Word "this"
    | S.ImportFunc ->
        Word "import"
    | S.Super ->
        Word "super"    
    | S.Cast (t, e) ->
        Token "<" ++ Expression t ++ Token ">"
        ++ ParensExpression 1 e 
    | S.ClassExpr (n, b, i, ms) ->
        Word "class" 
        ++ Optional Id n 
        ++ Optional (fun b -> Word "extends" ++ Expression b) b
        ++ OptionalList (fun i -> Word "implements" ++ CommaSeparated Expression i) i
        ++ BlockLayout (List.map (Member true) ms)

and Statement canBeEmpty statement =
    match statement with
    | S.StatementPos (x, pos) -> 
        SourceMapping pos ++ Statement canBeEmpty x ++ SourceMappingEnd pos
    | S.StatementComment (x, c) ->
        match x with
        | S.Empty when c.StartsWith "<" && c.EndsWith "/>" ->
            Word ("/// " + c)   
        | _ ->
            Statement canBeEmpty x ++ Word ("/*" + c +  "*/")
    | S.Block ss ->
        BlockLayout (List.map (Statement true) ss)
    | S.Break label ->
        Word "break" ++ Optional Label label ++ Token ";"
    | S.Continue label ->
        Word "continue" ++ Optional Label label ++ Token ";"
    | S.Debugger ->
        Word "debugger" ++ Token ";"
    | S.Do (s, e) ->
        Word "do"
        -- Indent (Statement false s)
        -- Word "while"
        ++ Parens (Expression e)
        ++ Token ";"
    | S.Empty ->
        if canBeEmpty then Empty else Word "{ }"
    | S.Ignore e ->
        let rec dangerous e =
            match e with
            | S.Lambda _ | S.NewObject _ -> true
            | S.ExprPos (x, _)
            | S.Application (x, _, _)
            | S.Binary (x, _, _)
            | S.Conditional (x, _, _)
            | S.Postfix (x, _) -> dangerous x
            | _ -> false
        if dangerous e then
            Parens (Expression e) ++ Token ";"
        else
            Expression e ++ Token ";"
    | S.For (e1, e2, e3, body) ->
        Word "for"
        ++ Parens (Optional ExpressionNoIn e1
                   ++ Token ";"
                   ++ Optional Expression e2
                   ++ Token ";"
                   ++ Optional Expression e3)
        ++ Statement false body
    | S.ForVars (vs, e1, e2, body) ->
        Word "for"
        ++ Parens (Word "let"
                   ++ VarsNoIn vs
                   ++ Token ";"
                   ++ Optional Expression e1
                   ++ Token ";"
                   ++ Optional Expression e2)
        ++ Statement false body
    | S.ForIn (e1, e2, body) ->
        Word "for"
        ++ Parens (LeftHandSideExpression e1
                   ++ Word "in"
                   ++ Expression e2)
        ++ Statement false body
    | S.ForVarIn (id, e1, e2, body) ->
        Word "for"
        ++ Parens (Word "var"
                   ++ Id id
                   ++ (e1 |> Optional (fun x ->
                       Token "=" ++ AssignmentExpressionNoIn x))
                   ++ Word "in"
                   ++ Expression e2)
        ++ Statement false body
    | S.If (e, s, S.IgnoreSPos S.Empty) ->
        Word "if"
        ++ Parens (Expression e)
        ++ (Statement false s)
    | S.If (e, s1, s2) ->
        let s1L =
            match S.IgnoreStatementPos s1 with
            | S.If (_,_,_) -> BlockLayout [ Statement true s1 ]
            | _ -> Statement false s1
        let rec isEmpty s =
            match S.IgnoreStatementPos s with
            | S.Empty -> true
            | S.Block ss -> List.forall isEmpty ss
            | _ -> false
        let s2L =
            if isEmpty s2 then Empty else
                Word "else"
                ++ (Statement false s2)
        Word "if"
        ++ Parens (Expression e)
        ++ s1L
        -- s2L
    | S.Labelled (label, s) ->
        Label label ++ Token ": " ++ Statement canBeEmpty s
    | S.Return e ->
        Word "return"
        ++ Optional Expression e
        ++ Token ";"
    | S.Switch (e, cases) ->
        Word "switch"
        ++ Parens (Expression e)
        ++ BlockLayout [
            for c in cases do
                match c with
                | S.Default ss ->
                    yield Word "default" ++ Token ":"
                    for s in ss do
                        yield Indent (Statement true s)
                | S.Case (e, ss)->
                    yield Word "case" ++ Expression e ++ Token ":"
                    for s in ss do
                        yield Indent (Statement true s)
           ]
    | S.Throw e ->
        Word "throw" ++ Expression e ++ Token ";"
    | S.TryWith (s1, id, s2, f) ->
        Word "try"
        ++ Block s1
        -- Word "catch" ++ Parens (Id id)
        ++ Block s2
        -- Optional (fun x -> Word "finally" ++ Block x) f
    | S.TryFinally (s1, s2) ->
        Word "try"
        ++ Block s1
        -- Word "finally"
        ++ Block s2
    | S.Vars ([], _) ->
        Empty
    | S.Vars (vs, k) ->
        let keyword =
            match k with
            | S.VarDecl -> "var"
            | S.ConstDecl -> "const"
            | S.LetDecl -> "let"
        Word keyword ++ Vars vs ++ Token ";"
    | S.While (e, s) ->
        Word "while" ++ Parens (Expression e)
        -- Indent (Statement false s)
    | S.With (e, s) ->
        Word "with" ++ Parens (Expression e) ++ Statement false s
    | S.Function (id, formals, body) ->
        Word "function"
        ++ Id (id.ToNonTyped())
        ++ Parens (CommaSeparated Id formals)
        ++ TypeAnnotation id.Type 
        ++ BlockLayout (List.map (Statement true) body)
    | S.Export (d, s) ->
        Word "export" ++ Conditional (Word "default") d  ++ Statement false s
    | S.ExportAlias (a, b) ->
        Word "export" ++ Word "{" ++ Id a ++ Word "as" ++ Id b ++ Word "}"
    | S.ImportAll (None, m) ->
        Word "import " ++ Token (QuoteString m) 
    | S.ImportAll (Some i, m) ->
        Word "import * as" ++ Id i ++ Word "from " ++ Token (QuoteString m) 
    | S.ImportAlias (i, e) ->
        Word "import" ++ Id i ++ Token "=" ++ Expression e
    | S.TypeAlias (i, e) ->
        Word "type" ++ Id i ++ Token "=" ++ Expression e
    | S.Declare s ->
        Word "declare" ++ Statement false s
    | S.DeclareGlobal s ->
        Word "declare global"
        ++ BlockLayout (List.map (Statement true) s)
    | S.Namespace (n, s) ->
        Word "namespace" ++ Id n
        ++ BlockLayout (List.map (Statement true) s)
    | S.Class (n, a, b, i, ms) ->
        Conditional (Word "abstract") a
        ++ Word "class" 
        ++ Id n 
        ++ Optional (fun b -> Word "extends" ++ Expression b) b
        ++ OptionalList (fun i -> Word "implements" ++ CommaSeparated Expression i) i
        ++ BlockLayout (List.map (Member true) ms)
    | S.Interface (n, i, ms) ->
        Word "interface" 
        ++ Id n 
        ++ OptionalList (fun i -> Word "extends" ++ CommaSeparated Expression i) i
        ++ BlockLayout (List.map (Member false) ms)
    | S.Import (d, f, n, m) ->
        let defImport =
            d |> Option.map Id |> Option.toList   
        let fullImport =
            f |> Option.map (fun f -> Word "* as" ++ Id f) |> Option.toList
        let namedImports =
            n |> List.map (fun (n, i) -> 
                let ei = EscapeId i.Name
                if n <> ei then
                    Word n
                    ++ Word "as"
                    ++ Word ei
                else
                    Word n
            ) 
        let namedImportBlock =
            match namedImports with
            | [] ->  []
            | _ -> [ Word "{" ++ CommaSeparated id namedImports ++ Word "}" ]
        Word "import"
        ++ CommaSeparated id (defImport @ fullImport @ namedImportBlock)
        ++ Word "from"
        ++ Word (QuoteString m)

and Accessor a =
    match a with
    | S.Get -> Word "get"
    | S.Set -> Word "set"
    | S.Simple -> Empty

and Member isClass mem =
    match mem with
    | S.Method (s, a, n, args, body) ->
        Conditional (Word "abstract") (isClass && Option.isNone body)
        ++ Conditional (Word "static") s
        ++ Accessor a
        ++ IdOrString (n.ToNonTyped())
        ++ Parens (CommaSeparated Id args)
        ++ TypeAnnotation n.Type 
        ++ Optional (List.map (Statement true) >> BlockLayout) body
    | S.Constructor (args, body) ->
        Word "constructor"
        ++ Parens (CommaSeparated IdAndModifiers args)
        ++ Optional (List.map (Statement true) >> BlockLayout) body
    | S.Property (s, n, v) ->
        Conditional (Word "static") s
        ++ IdOrString n
        ++ Optional (fun v -> Token "=" ++ Expression v) v
        ++ Token ";"
    | S.Static body ->
        Word "static"
        ++ BlockLayout (List.map (Statement true) body)

and Block statement =
    match S.IgnoreStatementPos statement with
    | S.Block ss -> List.map (Statement true) (Seq.toList ss)
    | _ -> [Statement true statement]
    |> BlockLayout

and AssignmentExpression expression =
    let prec = BinaryOperatorPrecedence B.``,``
    ParensExpression prec expression

and LeftHandSideExpression expression =
    match S.IgnoreExprPos expression with
    | S.Application _
    | S.New _
    | _ when Precedence expression <= BinaryOperatorPrecedence B.``.`` ->
        Expression expression
    | _ ->
        Parens (Expression expression)

and LogicalOrExpression expression =
    let prec = BinaryOperatorPrecedence B.``||``
    ParensExpression (prec + 1) expression

and MemberExpression expression =
    let prec = BinaryOperatorPrecedence B.``.``
    ParensExpression (prec + 1) expression

and ParensExpression level expression =
    if Precedence expression >= level then
        Parens (Expression expression)
    else
        Expression expression

and VarsGeneric expr vars =
    let init x = Token "=" ++ expr x
    let var (x, y) = Id x ++ Optional init y
    CommaSeparated var vars

and AssignmentExpressionNoIn expression =
    if HasIn expression then
        Parens (Expression expression)
    else
        AssignmentExpression expression

and ExpressionNoIn expression =
    if HasIn expression then
        Parens (Expression expression)
    else
        Expression expression

and HasIn expression =
    match S.IgnoreExprPos expression with
    | S.Binary (a, op, b) ->
        match op with
        | B.``in`` -> true
        | _ -> HasIn a || HasIn b
    | S.Conditional (a, b, c) ->
        HasIn a || HasIn b || HasIn c
    | _ ->
        false

and Vars = VarsGeneric AssignmentExpression
and VarsNoIn = VarsGeneric AssignmentExpressionNoIn

type Atom =
    | T of string
    | W of string
    | P of S.SourcePos
    | E of S.SourcePos
    | N of string

type Line =
    {
        Indent : int
        Atoms : list<Atom>
    }

let Simplify layout =
    let pair f a b =
        match a, b with
        | Empty, x | x, Empty -> x
        | _ -> f (a, b)
    let rec simp = function
        | Horizontal (a, b) -> pair Horizontal (simp a) (simp b)
        | Vertical (a, b) -> pair Vertical (simp a) (simp b)
        | Indent Empty -> Empty
        | Indent x -> 
            match simp x with
            | Empty -> Empty
            | sx -> Indent sx 
        | x -> x
    simp layout

let ToLines mode layout =
    let empty = { Indent = 0; Atoms = [] }
    let append level atom lines =
        match lines with
        | [] ->
            [{Indent = level; Atoms = [atom]}]
        | l :: ls ->
            let indent = max l.Indent level
            let l =
                {
                    Atoms = atom :: l.Atoms
                    Indent = indent
                }
            l :: ls
    let rec lines level tail = function
        | Empty ->
            tail
        | Horizontal (a, b) ->
            lines level (lines level tail b) a
        | Vertical (a, b) ->
            match mode with
            | Readable -> lines level (empty :: lines level tail b) a
            | Compact -> lines level (lines level tail b) a
        | Indent layout ->
            lines (level + 1) tail layout
        | Word x ->
            append level (W x) tail
        | Token x ->
            append level (T x) tail
        | SourceMapping p ->
            append level (P p) tail
        | SourceMappingEnd p ->
            append level (E p) tail
        | SourceName n ->
            append level (N n) tail
    lines 0 [] (Simplify layout)

let base64Digits =
    lazy [|
        yield! { 'A' .. 'Z' }
        yield! { 'a' .. 'z' }
        yield! { '0' .. '9' }
        yield '+'
        yield '/'
    |]
    
let encodeBase64VLQ value (builder: StringBuilder) =
    let base64Digits = base64Digits.Value
    let mutable v = abs value

    let mutable digit = (v &&& 0b00001111) <<< 1
    if value < 0 then digit <- digit + 1 
    v <- v >>> 4
    if v > 0 then digit <- digit ||| 0b00100000
    builder.Append base64Digits.[digit] |> ignore

    while v > 0 do
        digit <- v &&& 0b00011111    
        v <- v >>> 5
        if v > 0 then
            digit <- digit ||| 0b00100000
        builder.Append base64Digits.[digit] |> ignore

let testEncode value =
    let b = StringBuilder()
    encodeBase64VLQ value b
    string b

type CodeWriter(?sources: (string * string)[], ?offset) =
    let code = StringBuilder()
    let mappings = StringBuilder()
    let sourceMap = Option.isSome sources
    let sources = defaultArg sources [||]  
    let sourcesDict = sources |> Seq.mapi (fun i (n, _) -> n, i) |> dict 
    let mutable insertComma = false
    let mutable colFromLastMapping = 0
    let mutable mappingOn = false
    let names = ResizeArray()
    let namesDict = System.Collections.Generic.Dictionary()

    do
        match offset with
        | Some o ->
            for i = 1 to o do
                mappings.Append ';' |> ignore    
        | None -> ()

    let mutable lastFileName = ""
    let mutable lastFileIndex = 0
    let mutable lastSourceLine = 0
    let mutable lastSourceColumn = 0
    let mutable lastNameIndex = 0

    member this.Write(s: string) =
        code.Append s |> ignore
        if sourceMap then
            colFromLastMapping <- colFromLastMapping + s.Length

    member this.Write(s: char) =
        code.Append s |> ignore
        if sourceMap then
            colFromLastMapping <- colFromLastMapping + 1

    member this.WriteLine() =
        code.AppendLine() |> ignore
        if sourceMap then
            if insertComma then
                this.WriteMappingSkip()
            mappings.Append ';' |> ignore
            insertComma <- false
            colFromLastMapping <- 0

    member this.WriteMappingComma() =
        if insertComma then
            mappings.Append ',' |> ignore
        else
            insertComma <- true

    member this.WriteMappingSkip() =
        if not mappingOn then
            if colFromLastMapping > 0 then
                this.WriteMappingComma()
                mappings |> encodeBase64VLQ colFromLastMapping
                colFromLastMapping <- 0

    member this.AddCodeMapping(pos : S.SourcePos, start: bool, ?name : string) =
        if sourceMap then
            this.WriteMappingSkip()
            this.WriteMappingComma()
            mappingOn <- start

            mappings |> encodeBase64VLQ colFromLastMapping
            colFromLastMapping <- 0

            let fileName = pos.File
            if lastFileName = fileName then
                mappings.Append 'A' |> ignore
            else
                let fileIndex = sourcesDict.[fileName]
                mappings |> encodeBase64VLQ (fileIndex - lastFileIndex)   
                lastFileIndex <- fileIndex   
                lastFileName <- fileName
        
            let sourceLine = if start then pos.Line - 1 else pos.EndLine - 1
            mappings |> encodeBase64VLQ (sourceLine - lastSourceLine)
            lastSourceLine <- sourceLine
        
            let sourceColumn = if start then pos.Column - 1 else pos.EndColumn - 1
            mappings |> encodeBase64VLQ (sourceColumn - lastSourceColumn)
            lastSourceColumn <- sourceColumn

            match name with
            | Some name ->
                let nameIndex =
                    match namesDict.TryGetValue name with
                    | true, i -> i
                    | _ ->
                        let i = names.Count
                        names.Add name
                        namesDict.Add(name, i)
                        i
                mappings |> encodeBase64VLQ (nameIndex - lastNameIndex)
                lastNameIndex <- nameIndex
            | _ -> ()

    member this.GetCodeFile() = string code

    override this.ToString() = string code

    member this.GetMapFile() =
        if sources.Length = 0 then None else
        let mapFile = StringBuilder()
        let inline mapC (c: char) = mapFile.Append c |> ignore 
        let inline mapS (s: string) = mapFile.Append s |> ignore 
        let inline mapN (s: string) = mapFile.AppendLine s |> ignore 

        mapN "{"
        mapN "\"version\": 3,"
        mapN "\"sourceRoot\": \"Source\","
        mapS "\"sources\": [\""
        let im = sources.Length - 1
        for i = 0 to im do
            let key, _ = sources.[i]
            mapS key
            if i < im then
                mapS "\", \""
        mapN "\"],"
        mapS "\"sourcesContent\": ["
        for i = 0 to im do
            let _, content = sources.[i]
            mapC '"'
            for c in content do
                match c with
                | '\\' -> mapS "\\\\"
                | '"' ->  mapS "\\\""
                | '\n' -> mapS "\\n"
                | '\r' -> ()
                | _ -> mapC c
            mapC '"'
            if i < im then
                mapS ", "
        mapN "],"
        mapS "\"names\": [" 
        let im = names.Count - 1
        for i = 0 to im do
            mapC '"'
            mapS names.[i]
            mapC '"'
            if i < im then
                mapS ", "
        mapN "],"
        mapS "\"mappings\": \""
        mapFile.Append mappings |> ignore
        mapN "\""  
        mapN "}"
        Some (string mapFile)        

let Render mode (out: CodeWriter) layout =
    let rec (|O|_|) xs =
        match xs with
        | [] -> None
        | P _ :: ys 
        | E _ :: ys
        | N _ :: ys -> (|O|_|) ys
        | y :: _ -> Some y
//    let (|FirstP|_|) xs =
//        let rec skipP xs =
//            match xs with
//            | N n :: ys -> 
//                let ys, _ = skipP ys
//                ys, Some n 
//            | P _ :: ys -> skipP ys
//            | _ -> xs, None
//        match xs with
//        | P p :: ys -> Some (p, skipP ys)
//        | _ -> None
//    let rec (|LastE|_|) xs =
//        match xs with
//        | E _ :: LastE pys -> Some pys
//        | E p :: ys -> Some (p, ys)
//        | _ -> None
//    let rec renderAtoms xs =
    let (|FirstE|_|) xs =
        let rec skipE xs =
            match xs with
            | E _ :: ys -> skipE ys
            | _ -> xs
        match xs with
        | E p :: ys -> Some (p, skipE ys)
        | _ -> None
    let rec (|LastP|_|) xs =
        match xs with
        | P _ :: LastP pys -> Some pys
        | P p :: ys -> Some (p, None, ys)
        | N n :: LastP (p , _, ys) -> Some (p, Some n, ys)
        | _ -> None
    let rec renderAtoms xs =
        match xs with
        | [] -> ()
        | (W s as x) :: ys | (T s as x) :: ys ->
            out.Write s   
            match x, ys with
            | _, [] -> ()
            | W _, O(W _) | T "+", O(T "+") | T "-", O(T "-") ->
                out.Write ' '   
            | _ -> () 
            renderAtoms ys
        | LastP (p, n, ys) ->
            out.AddCodeMapping(p, true, ?name = n)
            renderAtoms ys   
        | FirstE (p, ys) ->
            out.AddCodeMapping(p, false)
            renderAtoms ys   
        | N _ :: ys ->
            renderAtoms ys   
        | _ -> failwith "wrong source position tokens"            
    let renderLine line =
        match line.Atoms with
        | [] -> ()
        | xs ->
            match mode with
            | Compact -> ()
            | Readable -> for k in 1 .. line.Indent do
                              out.Write "  "
            renderAtoms xs
        out.WriteLine()
    ToLines mode layout
    |> Seq.iter renderLine

let WriteExpression options writer expression =
    Expression expression
    |> Render options writer

let WriteProgram options writer (program: S.Program) =
    for elem in program do
        Statement true elem
        |> Render options writer

let ExpressionToString options expression =
    let w = CodeWriter()
    WriteExpression options w expression
    w.ToString()

let ProgramToString options program =
    let w = CodeWriter()
    WriteProgram options w program
    w.ToString()
