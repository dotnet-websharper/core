// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Implements a recursive-descent parser for JavaScript, as
/// defined in ECMA-262 3rd edition.
module IntelliFactory.JavaScript.Parser

/// Line number, starting from 1.
type Line = int

/// Column number, starting from 1.
type Column = int

/// Thrown on parse failures.
exception ParserError of Line * Column * string

/// Represents parser sources.
[<Sealed>]
type Source =

    /// Uses string as a parser source.
    static member FromString : string -> Source

    /// Uses TextReader as a parser source.
    static member FromTextReader : System.IO.TextReader -> Source

/// Parses an expression.
val ParseExpression : Source -> Syntax.Expression

/// Parses a complete program.
val ParseProgram : Source -> Syntax.Program
