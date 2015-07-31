// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Inline definitions are JavaScript expressions with placeholders.
/// This module parses and analyses such definitions, either compiling
/// them to JavaScript functions or intermediate language transformers.
module internal WebSharper.Compiler.Inlining

module C = WebSharper.Core.JavaScript.Core
module Q = WebSharper.Core.Quotations
module S = WebSharper.Core.JavaScript.Syntax

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

