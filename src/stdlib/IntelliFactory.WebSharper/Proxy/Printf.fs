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

namespace IntelliFactory.WebSharper

module M = Macro

[<Proxy(typeof<PrintfFormat<_,_,_,_,_>>)>]
type PrintfFormat = 
    [<Macro(typeof<M.PrintF>)>]
    new (value: string) = {}

[<Name "Printf">]
[<Proxy
    "Microsoft.FSharp.Core.PrintfModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private PrintfProxy =
    [<Inline "$f($k)">]
    let PrintFormatToStringThen (k: string -> _) (f: Printf.StringFormat<_, _>) = X

    [<JavaScript; Inline>]
    let PrintFormatLine (f: Printf.StringFormat<_>) = PrintFormatToStringThen JavaScript.Log f  

    [<JavaScript; Inline>]
    let PrintFormatToStringThenFail (f: Printf.StringFormat<_>) = PrintFormatToStringThen failwith f