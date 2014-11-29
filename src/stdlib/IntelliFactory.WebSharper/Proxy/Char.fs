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

[<Name "Char">]
[<Proxy(typeof<System.Char>)>]
type private CharProxy =

    [<JavaScript>]
    static member GetNumericValue(c: char) : float =
        if c >= '0' && c <= '9' then float c - float '0' else -1.

    [<JavaScript>]
    static member IsControl(c: char) : bool =
        c >= '\u0000' && c <= '\u001f'
        || c >= '\u0080' && c <= '\u009f'

    [<JavaScript>]
    static member IsDigit(c: char) : bool =
        c >= '0' && c <= '9'

    [<JavaScript>]
    static member IsLetter(c: char) : bool =
        c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

    [<JavaScript>]
    static member IsLetterOrDigit(c: char) : bool =
        System.Char.IsLetter c || System.Char.IsDigit c

    [<JavaScript>]
    static member IsLower(c: char) : bool =
        c >= 'a' && c <= 'z'

    [<JavaScript>]
    static member IsUpper(c: char) : bool =
        c >= 'A' && c <= 'Z'

    [<Direct @"String.fromCharCode($c).match(/\s/) !== null">]
    static member IsWhiteSpace(c: char) = X<bool>

    [<Inline "$s.charCodeAt(0)">]
    static member CharCodeAt0(s: string) = X<char>

    [<JavaScript>]
    static member Parse(s: string) =
        if s.Length = 1 then CharProxy.CharCodeAt0(s) else
            failwith "String must be exactly one character long."

