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

/// Defines the translation of F# quotations to JavaScript core expressions.
module internal IntelliFactory.WebSharper.Compiler.Translator

module Q = IntelliFactory.WebSharper.Core.Quotations
module C = IntelliFactory.JavaScript.Core
module I = IntelliFactory.WebSharper.Compiler.Inlining
module R = IntelliFactory.WebSharper.Compiler.Reflector

/// Thrown when quotation invariants are broken.
exception InvalidQuotation

/// Translates a quotation to JavaScript.
val Translate :
    Logger -> I.Pool -> R.Pool ->
    Metadata.T -> Location ->
    Q.Expression -> C.Expression
