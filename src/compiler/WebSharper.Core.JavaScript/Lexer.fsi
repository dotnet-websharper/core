// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

/// Implements lexical analysis for JavaScript.
module internal WebSharper.Core.JavaScript.Lexer

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
    | ``import`` = 29

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
