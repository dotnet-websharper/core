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

[<AutoOpen>]
module internal Constants =

    [<Literal>]
    let EMBEDDED_METADATA = "WebSharper.dep"

    [<Literal>]
    let EMBEDDED_JS = "WebSharper.js"

    [<Literal>]
    let EMBEDDED_MINJS = "WebSharper.min.js"

    [<Literal>]
    let EMBEDDED_DTS = "WebSharper.d.ts"
