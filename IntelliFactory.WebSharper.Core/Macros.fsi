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

/// Provides supporting types for implementing custom compilation
/// rules using MacroAttribute.
module IntelliFactory.WebSharper.Core.Macros

module R  = IntelliFactory.WebSharper.Core.Reflection
module Q  = IntelliFactory.WebSharper.Core.Quotations
module JS = IntelliFactory.JavaScript.Syntax
module JC = IntelliFactory.JavaScript.Core

/// Represents a translator function.
type Translator = Q.Expression -> JC.Expression

/// Represents method bodies, at either JavaScript core or syntax level.
type Body =
    | CoreBody of JC.Expression
    | SyntaxBody of JS.Expression

/// Represents a custom compilation rule for a method. Expand
/// accepts Call or CallModule nodes associated with the given method.
type Macro =
    {
        Body            : option<Body>
        Expand          : Translator -> Translator
        Requirements    : list<Metadata.Node>
    }

/// An interface for macro definitions used with MacroAttribute.
type IMacroDefinition =

    /// Loads a definition for a given method.
    abstract member Macro : Macro
