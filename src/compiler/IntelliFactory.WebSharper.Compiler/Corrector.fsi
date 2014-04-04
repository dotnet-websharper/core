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

/// Implements ad-hoc corrections for nonsensical quotations
/// produced by the F# compiler, in particular implementing uncurrying and
/// constructor normalization.
module internal IntelliFactory.WebSharper.Compiler.Corrector

module C = IntelliFactory.JavaScript.Core

type Currying = list<int>

type Correction =
    | Constructor of Currying
    | Field
    | Method of Currying * MemberScope

val Correct : Correction -> C.Expression -> C.Expression
