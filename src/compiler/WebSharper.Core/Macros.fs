// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Provides supporting types for implementing custom compilation
/// rules using MacroAttribute.
module WebSharper.Core.Macros

module C = WebSharper.Core.JavaScript.Core
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection
module S = WebSharper.Core.JavaScript.Syntax

/// Represents a translator function.
type Translator = Q.Expression -> C.Expression

/// Represents method bodies, at either JavaScript core or syntax level.
type Body =
    | QuotationBody of Microsoft.FSharp.Quotations.Expr
    | CoreBody of C.Expression
    | SyntaxBody of S.Expression

///// Represents a custom compilation rule for a method. Expand
///// accepts Call or CallModule nodes associated with the given method.
//type Macro =
//    {
//        Body            : option<Body>
//        Expand          : Translator -> Translator
//        Requirements    : list<Metadata.Node>
//    }

/// An interface for macro definitions used with MacroAttribute.
type IMacro =
    abstract member Expand: Translator -> Translator

type IGenerator =
    abstract member Body : Body
