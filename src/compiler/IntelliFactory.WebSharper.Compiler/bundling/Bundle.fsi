// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Compiler

/// Experimental API for bundling WebSharper file sets into application packages.
[<Sealed>]
type Bundle =
    member CSS : Content
    member HtmlHeaders : Content
    member JavaScript : Content
    member JavaScriptHeaders : Content
    member MinifiedJavaScript : Content
    member TypeScript : Content
    member WithAssembly : assemblyFile: string -> Bundle
    member WithDefaultReferences : unit -> Bundle
    member WithTransitiveReferences : unit -> Bundle
    static member Empty : Bundle
    static member Create : unit -> Bundle
