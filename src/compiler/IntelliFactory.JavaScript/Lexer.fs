// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

module IntelliFactory.JavaScript.Lexer

#nowarn "86"

type Keyword =
    | ``break`` = 0
    | ``case`` = 1
    | ``catch`` = 2
    | ``continue`` = 3
    | ``debugger`` = 4
    | ``default`` = 5
    | ``delete`` = 6
    | ``do`` = 7
    | ``else`` = 8
    | ``false`` = 9
    | ``finally`` = 10
    | ``for`` = 11
    | ``function`` = 12
    | ``if`` = 13
    | ``in`` = 14
    | ``instanceof`` = 15
    | ``new`` = 16
    | ``null`` = 17
    | ``return`` = 18
    | ``switch`` = 19
    | ``this`` = 20
    | ``throw`` = 21
    | ``true`` = 22
    | ``try`` = 23
    | ``typeof`` = 24
    | ``var`` = 25
    | ``void`` = 26
    | ``while`` = 27
    | ``with`` = 28

type Symbol =
    | ``{`` = 0
    | ``}`` = 1
    | ``(`` = 2
    | ``)`` = 3
    | ``[`` = 4
    | ``]`` = 5
    | ``.`` = 6
    | ``;`` = 7
    | ``,`` = 8
    | ``<`` = 9
    | ``>`` = 10
    | ``<=`` = 11
    | ``>=`` = 12
    | ``==`` = 13
    | ``!=`` = 14
    | ``===`` = 15
    | ``!==`` = 16
    | ``+`` = 17
    | ``-`` = 18
    | ``*`` = 19
    | ``%`` = 20
    | ``/`` = 21
    | ``++`` = 22
    | ``--`` = 23
    | ``<<`` = 24
    | ``>>`` = 25
    | ``>>>`` = 26
    | ``&`` = 27
    | ``|`` = 28
    | ``^`` = 29
    | ``!`` = 30
    | ``~`` = 31
    | ``&&`` = 32
    | ``||`` = 33
    | ``?`` = 34
    | ``:`` = 35
    | ``=`` = 36
    | ``+=`` = 37
    | ``-=`` = 38
    | ``*=`` = 39
    | ``%=`` = 40
    | ``/=`` = 41
    | ``<<=`` = 42
    | ``>>=`` = 43
    | ``>>>=`` = 44
    | ``&=`` = 45
    | ``|=`` = 46
    | ``^=`` = 47

type Lexeme =
    | EndOfInput
    | Identifier of string
    | NumericLiteral of string
    | Punctuator of Symbol
    | RegexLiteral of string
    | ReservedWord of Keyword
    | StringLiteral of string

    override this.ToString() =
        match this with
        | EndOfInput -> "#eof"
        | Identifier id -> id
        | NumericLiteral x -> x
        | Punctuator x -> string x
        | RegexLiteral x -> x
        | ReservedWord x -> string x
        | StringLiteral x -> System.String.Format("\"{0}\"", x)

type Column = int
type Line = int

type IToken =
    abstract member Lexeme : Lexeme
    abstract member Line : Line
    abstract member Column : Column
    abstract member FollowsLineTerminator : bool

[<Struct>]
type Token(lexeme: Lexeme, line: Line, column: Column, parsedLT: bool) =
    interface IToken with
        member this.Lexeme = lexeme
        member this.Line = line
        member this.Column = column
        member this.FollowsLineTerminator = parsedLT

type State =
    {
        buffer : System.Text.StringBuilder
        reader : System.IO.TextReader
        mutable allowRE : bool
        mutable column : Column
        mutable line : Line
        mutable parsedLT : bool
    }

let peek s = s.reader.Peek()
let push s (c: char) = s.buffer.Append c |> ignore
let isRegexAllowed s = s.allowRE
let parseLine s = s.parsedLT <- true

let skip s =
    match s.reader.Read() with
    | -1 ->
        -1
    | 10 ->
        s.line <- s.line + 1
        s.column <- 1
        10
    | c ->
        s.column <- s.column + 1
        c

let read s =
    match skip s with
    | -1 -> -1
    | c -> push s (char c); c

let pop s =
    let b = s.buffer
    let v = b.ToString()
    b.Remove(0, b.Length) |> ignore
    v

exception LexerError of Line * Column * string

let error s m =
    raise (LexerError (s.line, s.column, m))

let isLineTerminator c =
    match c with
    | '\r' | '\n' | '\u2028' | '\u2029' -> true
    | _ -> false

type UC = System.Globalization.UnicodeCategory

let isWhiteSpace c =
    match c with
    | '\t' | '\v' | '\f' | ' ' | '\u00A0' | '\uFEFF' -> true
    | c when c > '\xFF' ->
        match System.Char.GetUnicodeCategory (char c) with
        | UC.SpaceSeparator -> true
        | _ -> false
    | _ ->
        false

let isIdStart c =
    match c with
    | '$' | '_' ->
        true
    | c when c >= 'a' && c <= 'z'
             || c >= 'A' && c <= 'Z' ->
        true
    | c when c > '\xFF' ->
        match System.Char.GetUnicodeCategory c with
        | UC.UppercaseLetter
        | UC.LowercaseLetter
        | UC.TitlecaseLetter
        | UC.ModifierLetter
        | UC.OtherLetter
        | UC.LetterNumber ->
            true
        | _ ->
            false
    | _ ->
        false

let isIdPart c =
    match c with
    | c when c >= '0' && c <= '9' -> true
    | c when isIdStart c -> true
    | '\u200D' | '\u000c' -> true
    | c when c >= '\xFF' ->
        match System.Char.GetUnicodeCategory c with
        | UC.NonSpacingMark
        | UC.SpacingCombiningMark
        | UC.DecimalDigitNumber
        | UC.ConnectorPunctuation ->
            true
        | _ ->
            false
    | _ ->
        false

type Kind =
    | Digit = 0
    | EOF = 1
    | IdStart = 2
    | Line = 3
    | Space = 4
    | Unacceptable = 5
    | ``!`` = 6
    | ``"`` = 7
    | ``%`` = 8
    | ``&`` = 9
    | ``'`` = 10
    | ``(`` = 11
    | ``)`` = 12
    | ``*`` = 13
    | ``+`` = 14
    | ``,`` = 15
    | ``-`` = 16
    | ``.`` = 17
    | ``/`` = 18
    | ``:`` = 19
    | ``;`` = 20
    | ``<`` = 21
    | ``=`` = 22
    | ``>`` = 23
    | ``?`` = 24
    | ``[`` = 25
    | ``\`` = 26
    | ``]`` = 27
    | ``^`` = 28
    | ``{`` = 29
    | ``|`` = 30
    | ``}`` = 31
    | ``~`` = 32

let parseKind c =
    if c = -1 then Kind.EOF else
        match char c with
        | '!' -> Kind.``!``
        | '"' -> Kind.``"``
        | '%' -> Kind.``%``
        | '&' -> Kind.``&``
        | '\'' -> Kind.``'``
        | '(' -> Kind.``(``
        | ')' -> Kind.``)``
        | '*' -> Kind.``*``
        | '+' -> Kind.``+``
        | ',' -> Kind.``,``
        | '-' -> Kind.``-``
        | '.' -> Kind.``.``
        | '/' -> Kind.``/``
        | ':' -> Kind.``:``
        | ';' -> Kind.``;``
        | '<' -> Kind.``<``
        | '=' -> Kind.``=``
        | '>' -> Kind.``>``
        | '?' -> Kind.``?``
        | '[' -> Kind.``[``
        | '\\' -> Kind.``\``
        | ']' -> Kind.``]``
        | '^' -> Kind.``^``
        | '{' -> Kind.``{``
        | '|' -> Kind.``|``
        | '}' -> Kind.``}``
        | '~' -> Kind.``~``
        | '\t' | '\v' | '\f' | ' ' ->
            Kind.Space
        | '\r' | '\n' ->
            Kind.Line
        | c ->
            match c with
            | c when c >= '0' && c <= '9' -> Kind.Digit
            | c when isIdStart c -> Kind.IdStart
            | c when isWhiteSpace c -> Kind.Space
            | c when isLineTerminator c -> Kind.Line
            | _ -> Kind.Unacceptable

let toChar s i =
    match i with
    | -1 -> error s "Unexpected end of input."
    | _ -> char i

let peekChar s = toChar s (peek s)
let skipChar s = toChar s (skip s)
let readChar s = toChar s (read s)

let skipMultiLineComment s =
    let rec loop () =
        match skipChar s with
        | '*' ->
            match skipChar s with
            | '/' -> ()
            | _ -> loop ()
        | _ ->
            loop ()
    loop ()

let skipSingleLineComment s =
    let rec loop () =
        match peek s with
        | -1 -> ()
        | i ->
            if not (isLineTerminator (char i)) then
                skip s |> ignore
                loop ()
    loop ()

let isHexDigit c =
    c >= '0' && c <= '9'
    || c >= 'a' && c <= 'f'
    || c >= 'A' && c <= 'F'

let skipHexDigit s =
    match skipChar s with
    | c when c >= '0' && c <= '9' -> int c - int '0'
    | c when c >= 'a' && c <= 'f' -> 10 + int c - int 'a'
    | c when c >= 'A' && c <= 'F' -> 10 + int c - int 'A'
    | _ -> error s "Invalid hexadecimal digit."

let readHexEscapeSequence s =
    let (++) a b = (a <<< 4) + b
    let x = skipHexDigit s
    let y = skipHexDigit s
    push s (char (x ++ y))

let readUnicodeEscapeSequence s =
    let (++) a b = (a <<< 4) + b
    let a = skipHexDigit s
    let b = skipHexDigit s
    let c = skipHexDigit s
    let d = skipHexDigit s
    push s (char (a ++ b ++ c ++ d))

let readEscapeSequence s =
    match skipChar s with
    | 'b' -> push s '\b'
    | 'f' -> push s '\f'
    | 'n' -> push s '\n'
    | 'r' -> push s '\r'
    | 't' -> push s '\t'
    | 'u' -> readUnicodeEscapeSequence s
    | 'v' -> push s '\v'
    | 'x' -> readHexEscapeSequence s
    | '0' ->
        match peek s with
        | i when i >= int '0' && i <= int '9' ->
            error s "Invalid escape sequence."
        | _ ->
            push s '\000'
    | '\r' ->
        match peek s with
        | 10 -> skip s |> ignore
        | _ -> ()
    | c when c >= '1' && c <= '9' ->
        error s "Invalid escape sequence."
    | c ->
        if not (isLineTerminator c) then
            push s (char c)

let lexDoubleQuotedString s =
    let rec loop () =
        match peekChar s with
        | '"' ->
            skip s |> ignore
            StringLiteral (pop s)
        | '\\' ->
            skip s |> ignore
            readEscapeSequence s
            loop ()
        | c when isLineTerminator c ->
            error s "Unexpected LineTerminator in string literal."
        | i ->
            read s |> ignore
            loop ()
    loop ()

let lexSingleQuotedString s =
    let rec loop () =
        match peekChar s with
        | '\'' ->
            skip s |> ignore
            StringLiteral (pop s)
        | '\\' ->
            skip s |> ignore
            readEscapeSequence s
            loop ()
        | c when isLineTerminator c ->
            error s "Unexpected line terminator in string literal."
        | _ ->
            read s |> ignore
            loop ()
    loop ()

let readManyDecimalDigits s =
    let rec loop () =
        match peek s with
        | i when i >= int '0' && i <= int '9' ->
            read s |> ignore
            loop ()
        | _ ->
            ()
    loop ()

let readExponentPart s =
    match peek s with
    | -1 -> ()
    | i ->
        match char i with
        | 'e' | 'E' ->
            read s |> ignore
            match readChar s with
            | '+' | '-' ->
                match read s with
                | i when i >= int '0' && i <= int '9' ->
                    readManyDecimalDigits s
                | _ ->
                    error s "Invalid numeric literal."
            | c when c >= '0' && c <= '9' ->
                readManyDecimalDigits s
            | _ ->
                error s "Invalid numeric literal."
        | _ ->
            ()

let readDecimalLiteralTail s =
    match peek s with
    | i when i = int '.' ->
        read s |> ignore
        readManyDecimalDigits s
    | _ ->
        ()
    readExponentPart s

let lexAfterZero s =
    match peek s with
    | -1 -> NumericLiteral (pop s)
    | i ->
        match char i with
        | 'x' | 'X' ->
            read s |> ignore
            if not (isHexDigit (readChar s)) then
                error s "Invalid hexadecimal integer literal."
            let rec loop () =
                match peek s with
                | -1 -> ()
                | i ->
                    if isHexDigit (char i) then
                        read s |> ignore
                        loop ()
            loop ()
            NumericLiteral (pop s)
        | c when c >= '0' && c <= '9' ->
            error s "Invalid integer literal."
        | '.' | 'E' | 'e' ->
            readDecimalLiteralTail s
            NumericLiteral (pop s)
        | _ ->
            NumericLiteral (pop s)

let lexAfterNonZeroDigit s =
    readManyDecimalDigits s
    readDecimalLiteralTail s
    NumericLiteral (pop s)

let lexAfterDot s =
    match peek s with
    | i when i >= int '0' && i <= int '9' ->
        push s '.'
        read s |> ignore
        readManyDecimalDigits s
        NumericLiteral (pop s)
    | _ ->
        Punctuator Symbol.``.``

let readRxEscape s =
    match readChar s with
    | c when isLineTerminator c ->
        error s "Unexpected line terminator in a \
            regular expression literal."
    | _ ->
        ()

let readRxClass s =
    let rec loop () =
        match readChar s with
        | ']' -> ()
        | '\\' -> readRxEscape s; loop ()
        | c when isLineTerminator c ->
            error s "Unexpected line terminator in a \
                regular expression literal."
        | _ ->
            loop ()
    loop ()

let readRxFirstChar s =
    match readChar s with
    | '[' -> readRxClass s
    | '\\' -> readRxEscape s
    | c when isLineTerminator c ->
        error s "Unexpected line terminator in a \
            regular expression literal."
    | _ -> ()

let readRxChars s =
    let rec loop () =
        match readChar s with
        | '\\' -> readRxEscape s; loop ()
        | '[' -> readRxClass s; loop ()
        | '/' -> ()
        | c when isLineTerminator c ->
            error s "Unexpected line terminator in a \
                regular expression literal."
        | _ ->
            loop ()
    loop ()

let readRxFlags s =
    let rec loop () =
        match peek s with
        | -1 ->
            ()
        | i when isIdPart (char i) ->
            read s |> ignore
            loop ()
        | _ ->
            ()
    loop ()

let lexRegexLiteral s =
    push s '/'
    readRxFirstChar s
    readRxChars s
    readRxFlags s
    RegexLiteral (pop s)

let parseIdentifierName id =
    match id with
    | "break" -> ReservedWord Keyword.``break``
    | "case" -> ReservedWord Keyword.``case``
    | "catch" -> ReservedWord Keyword.``catch``
    | "continue" -> ReservedWord Keyword.``continue``
    | "debugger" -> ReservedWord Keyword.``debugger``
    | "default" -> ReservedWord Keyword.``default``
    | "delete" -> ReservedWord Keyword.``delete``
    | "do" -> ReservedWord Keyword.``do``
    | "else" -> ReservedWord Keyword.``else``
    | "false" -> ReservedWord Keyword.``false``
    | "finally" -> ReservedWord Keyword.``finally``
    | "for" -> ReservedWord Keyword.``for``
    | "function" -> ReservedWord Keyword.``function``
    | "if" -> ReservedWord Keyword.``if``
    | "in" -> ReservedWord Keyword.``in``
    | "instanceof" -> ReservedWord Keyword.``instanceof``
    | "new" -> ReservedWord Keyword.``new``
    | "null" -> ReservedWord Keyword.``null``
    | "return" -> ReservedWord Keyword.``return``
    | "switch" -> ReservedWord Keyword.``switch``
    | "this" -> ReservedWord Keyword.``this``
    | "throw" -> ReservedWord Keyword.``throw``
    | "true" -> ReservedWord Keyword.``true``
    | "try" -> ReservedWord Keyword.``try``
    | "typeof" -> ReservedWord Keyword.``typeof``
    | "var" -> ReservedWord Keyword.``var``
    | "void" -> ReservedWord Keyword.``void``
    | "while" -> ReservedWord Keyword.``while``
    | "with" -> ReservedWord Keyword.``with``
    | _ -> Identifier id

let lexIdentifierName allowKeywords s =
    let rec loop () =
        match peek s with
        | -1 ->
            ()
        | i ->
            match char i with
            | '\\' ->
                skip s |> ignore
                match skipChar s with
                | 'u' ->
                    readUnicodeEscapeSequence s
                    loop ()
                | _ ->
                    error s "Invalid escape sequence."
            | c when isIdPart c ->
                read s |> ignore
                loop ()
            | _ ->
                ()
    loop ()
    let id = pop s
    if allowKeywords then
        parseIdentifierName id
    else
        Identifier id

let lexToken s =
    let rec loop () =
        let c = peek s
        match parseKind c with
        | Kind.Digit ->
            read s |> ignore
            if c = int '0' then
                lexAfterZero s
            else
                lexAfterNonZeroDigit s
        | Kind.EOF ->
            EndOfInput
        | Kind.IdStart ->
            read s |> ignore
            lexIdentifierName true s
        | Kind.Line ->
            match char c with
            | '\r' ->
                skip s |> ignore
                match peek s with
                | 10 -> skip s |> ignore
                | _ -> ()
            | _ ->
                skip s |> ignore
            parseLine s
            loop ()
        | Kind.Space ->
            skip s |> ignore
            loop ()
        | Kind.``!`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                match peekChar s with
                | '=' ->
                    skip s |> ignore
                    Punctuator Symbol.``!==``
                | _ ->
                    Punctuator Symbol.``!=``
            | _ ->
                Punctuator Symbol.``!``
        | Kind.``"`` ->
            skip s |> ignore
            lexDoubleQuotedString s
        | Kind.``%`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``%=``
            | _ ->
                Punctuator Symbol.``%``
        | Kind.``&`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``&=``
            | '&' ->
                skip s |> ignore
                Punctuator Symbol.``&&``
            | _ ->
                Punctuator Symbol.``&``
        | Kind.``'`` ->
            skip s |> ignore
            lexSingleQuotedString s
        | Kind.``(`` ->
            skip s |> ignore
            Punctuator Symbol.``(``
        | Kind.``)`` ->
            skip s |> ignore
            Punctuator Symbol.``)``
        | Kind.``*`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``*=``
            | _ ->
                Punctuator Symbol.``*``
        | Kind.``+`` ->
            skip s |> ignore
            match parseKind (peek s) with
            | Kind.``+`` ->
                skip s |> ignore
                Punctuator Symbol.``++``
            | Kind.``=`` ->
                skip s |> ignore
                Punctuator Symbol.``+=``
            | _ ->
                Punctuator Symbol.``+``
        | Kind.``,`` ->
            skip s |> ignore
            Punctuator Symbol.``,``
        | Kind.``-`` ->
            skip s |> ignore
            match parseKind (peek s) with
            | Kind.``-`` ->
                skip s |> ignore
                Punctuator Symbol.``--``
            | Kind.``=`` ->
                skip s |> ignore
                Punctuator Symbol.``-=``
            | _ ->
                Punctuator Symbol.``-``
        | Kind.``.`` ->
            skip s |> ignore
            lexAfterDot s
        | Kind.``/`` ->
            skip s |> ignore
            match parseKind (peek s) with
            | Kind.``*`` ->
                skip s |> ignore
                skipMultiLineComment s
                loop ()
            | Kind.``/`` ->
                skip s |> ignore
                skipSingleLineComment s
                loop ()
            | Kind.``=`` ->
                skip s |> ignore
                Punctuator Symbol.``/=``
            | _ ->
                if isRegexAllowed s then
                    lexRegexLiteral s
                else
                    Punctuator Symbol.``/``
        | Kind.``:`` ->
            skip s |> ignore
            Punctuator Symbol.``:``
        | Kind.``;`` ->
            skip s |> ignore
            Punctuator Symbol.``;``
        | Kind.``<`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``<=``
            | '<' ->
                skip s |> ignore
                match peekChar s with
                | '=' ->
                    skip s |> ignore
                    Punctuator Symbol.``<<=``
                | _ ->
                    Punctuator Symbol.``<<``
            | _ ->
                Punctuator Symbol.``<``
        | Kind.``=`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                match peekChar s with
                | '=' ->
                    skip s |> ignore
                    Punctuator Symbol.``===``
                | _ ->
                    Punctuator Symbol.``==``
            | _ ->
                Punctuator Symbol.``=``
        | Kind.``>`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``>=``
            | '>' ->
                skip s |> ignore
                match peekChar s with
                | '=' ->
                    skip s |> ignore
                    Punctuator Symbol.``>>=``
                | '>' ->
                    skip s |> ignore
                    match peekChar s with
                    | '=' ->
                        skip s |> ignore
                        Punctuator Symbol.``>>>=``
                    | _ ->
                        Punctuator Symbol.``>>>``
                | _ ->
                    Punctuator Symbol.``>>``
            | _ ->
                Punctuator Symbol.``>``
        | Kind.``?`` ->
            skip s |> ignore
            Punctuator Symbol.``?``
        | Kind.``[`` ->
            skip s |> ignore
            Punctuator Symbol.``[``
        | Kind.``\`` ->
            skip s |> ignore
            match skipChar s with
            | 'u' ->
                readUnicodeEscapeSequence s
                lexIdentifierName false s
            | _ ->
                error s "Invalid escape sequence."
        | Kind.``]`` ->
            skip s |> ignore
            Punctuator Symbol.``]``
        | Kind.``^`` ->
            skip s |> ignore
            match peekChar s with
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``^=``
            | _ ->
                Punctuator Symbol.``^``
        | Kind.``|`` ->
            skip s |> ignore
            match peekChar s with
            | '|' ->
                skip s |> ignore
                Punctuator Symbol.``||``
            | '=' ->
                skip s |> ignore
                Punctuator Symbol.``|=``
            | _ ->
                Punctuator Symbol.``|``
        | Kind.``{`` ->
            skip s |> ignore
            Punctuator Symbol.``{``
        | Kind.``}`` ->
            skip s |> ignore
            Punctuator Symbol.``}``
        | Kind.``~`` ->
            skip s |> ignore
            Punctuator Symbol.``~``
        | _ ->
            System.String.Format("Invalid character: \u{0:x4}", c)
            |> error s
    loop ()

let FromTextReader (r: System.IO.TextReader) =
    {
        buffer = System.Text.StringBuilder(512)
        reader = r
        line = 1
        column = 1
        allowRE = false
        parsedLT = false
    }

let FromString (s: string) =
    let r = new System.IO.StringReader(s)
    FromTextReader r

let InputElementDiv s =
    s.parsedLT <- false
    s.allowRE  <- false
    let lexeme = lexToken s
    Token(lexeme, s.line, s.column, s.parsedLT) :> IToken

let InputElementRegExp s =
    s.parsedLT <- false
    s.allowRE  <- true
    let lexeme = lexToken s
    Token(lexeme, s.line, s.column, s.parsedLT) :> IToken
