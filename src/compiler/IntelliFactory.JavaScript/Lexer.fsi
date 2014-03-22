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

/// Implements lexical analysis for JavaScript.
module internal IntelliFactory.JavaScript.Lexer

/// Represents keywords.
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

/// Represents symbolic keywords.
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

/// Represents lexemes: identifiers, literals, keywords and so on.
type Lexeme =
    | EndOfInput
    | Identifier of string
    | NumericLiteral of string
    | Punctuator of Symbol
    | RegexLiteral of string
    | ReservedWord of Keyword
    | StringLiteral of string

/// Represents the column number, starting from 1.
type Column = int

/// Represents the line number, starting from 1.
type Line = int

/// Represents lexical analysis errors.
exception LexerError of Line * Column * string

/// Represents recognized tokens.
type IToken =

    /// The recognized lexeme.
    abstract member Lexeme : Lexeme

    /// The line part of the token position.
    abstract member Line : Line

    /// The column part of the token position.
    abstract member Column : Column

    /// True if the token was preceded by a significant line terminator.
    abstract member FollowsLineTerminator : bool

/// Represents an opaque analyzer state.
[<Sealed>]
type State

/// Opens a string for analysis.
val FromString : string -> State

/// Opens a TextReader for analysis.
val FromTextReader : System.IO.TextReader -> State

/// Retrieves the next token using the Div lexical grammar.
val InputElementDiv : State -> IToken

/// Retrieves the next token using the RegExp lexical grammar.
val InputElementRegExp : State -> IToken
