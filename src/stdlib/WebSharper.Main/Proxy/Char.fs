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

namespace WebSharper

open WebSharper.JavaScript

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

