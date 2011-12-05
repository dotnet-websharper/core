// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

/// Provides a writer for JavaScript syntax.
module IntelliFactory.JavaScript.Writer

type private TextWriter =
    System.IO.TextWriter

/// Writes a JavaScript expression to a writer.
val WriteExpression : Preferences -> TextWriter -> Syntax.Expression -> unit

/// Writes a JavaScript program to a writer.
val WriteProgram : Preferences -> TextWriter -> Syntax.Program -> unit

/// Writes a JavaScript expression to a string.
val ExpressionToString : Preferences -> Syntax.Expression -> string

/// Writes a JavaScript program to a string.
val ProgramToString : Preferences -> Syntax.Program -> string
