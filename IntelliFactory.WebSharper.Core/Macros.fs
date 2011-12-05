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

module IntelliFactory.WebSharper.Core.Macros

module C = IntelliFactory.JavaScript.Core
module Q = IntelliFactory.WebSharper.Core.Quotations
module R = IntelliFactory.WebSharper.Core.Reflection
module S = IntelliFactory.JavaScript.Syntax

type Translator = Q.Expression -> C.Expression

type Body =
    | CoreBody of C.Expression
    | SyntaxBody of S.Expression

type Macro =
    {
        Body            : option<Body>
        Expand          : Translator -> Translator
        Requirements    : list<Metadata.Node>
    }

type IMacroDefinition =
    abstract member Macro : Macro
