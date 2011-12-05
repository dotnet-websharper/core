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

/// Inline definitions are JavaScript expressions with placeholders.
/// This module parses and analyses such definitions, either compiling
/// them to JavaScript functions or intermediate language transformers.
module internal IntelliFactory.WebSharper.Compiler.Inlining

module C = IntelliFactory.JavaScript.Core
module Q = IntelliFactory.WebSharper.Core.Quotations
module S = IntelliFactory.JavaScript.Syntax

/// Represents JavaScript code patterns.
type Pattern =
    | Compiled of string
    | Inlined of string
    | Quoted of Corrector.Correction * Q.Expression

/// Represents method parameter names.
type ParameterName = S.Id

/// Represents an inline specification. Can be serialized.
[<Sealed>]
type Inline =

    /// The member scope.
    member MemberScope : MemberScope

    /// Tests if the Inline compiles to a Core expression transformer
    /// (otherwise it compiles to a syntactic JavaScript function).
    member IsTransformer : bool

    /// The quotation, if any.
    member Quotation : option<Q.Expression>

/// Represents a parsed inline.
type Inliner =
    | Function of S.Expression
    | Transformer of ((Q.Expression -> C.Expression) -> list<C.Expression> ->
        C.Expression)

/// Manages Inline objects, in particular parser inline definitions.
/// Caches the parse results.
[<Sealed>]
type Pool =

    /// Creates a new Inline object.
    member CreateInline : MemberScope -> Location -> list<ParameterName> ->
        Pattern -> Inline

    /// Parses an inline. The results are cached.
    member Parse : Inline -> Inliner

    /// Creates a new Pool object.
    static member Create : Logger -> Pool

