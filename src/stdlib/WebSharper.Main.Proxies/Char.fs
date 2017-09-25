// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

    [<Inline "'\u0000'">]
    new () = {}

    [<Inline>]
    member this.CompareTo(s: char) =
        Unchecked.compare (this :> obj) (s :> obj)

    [<Inline>]
    member this.CompareTo(s: obj) =
        Unchecked.compare (this :> obj) s

    [<Inline "$this == $s">]
    member this.Equals(s: char) = X<bool>

    [<Inline "$this === $s">]
    member this.Equals(s: obj) = X<bool>

    [<Inline>]
    member this.GetHashCode() = hash this

    static member GetNumericValue(c: char) : float =
        if c >= '0' && c <= '9' then float c - float '0' else -1.

    [<Inline>]
    static member GetNumericValue(s: string, i: int) = CharProxy.GetNumericValue(s.[i])

    static member IsControl(c: char) : bool =
        c >= '\u0000' && c <= '\u001f'
        || c >= '\u0080' && c <= '\u009f'

    [<Inline>]
    static member IsControl(s: string, i: int) = CharProxy.IsControl(s.[i])

    static member IsDigit(c: char) : bool =
        c >= '0' && c <= '9'

    [<Inline>]
    static member IsDigit(s: string, i: int) = CharProxy.IsDigit(s.[i])

    static member IsLetter(c: char) : bool =
        c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

    [<Inline>]
    static member IsLetter(s: string, i: int) = CharProxy.IsLetter(s.[i])

    static member IsLetterOrDigit(c: char) : bool =
        System.Char.IsLetter c || System.Char.IsDigit c

    [<Inline>]
    static member IsLetterOrDigit(s: string, i: int) = CharProxy.IsLetterOrDigit(s.[i])

    static member IsLower(c: char) : bool =
        c >= 'a' && c <= 'z'

    [<Inline>]
    static member IsLower(s: string, i: int) = CharProxy.IsLower(s.[i])

    static member IsUpper(c: char) : bool =
        c >= 'A' && c <= 'Z'

    [<Inline>]
    static member IsUpper(s: string, i: int) = CharProxy.IsUpper(s.[i])

    [<Direct @"$c.match(/\s/) !== null">]
    static member IsWhiteSpace(c: char) = X<bool>

    [<Inline>]
    static member IsWhiteSpace(s: string, i: int) = CharProxy.IsWhiteSpace(s.[i])

    static member Parse(s: string) =
        if s.Length = 1 then As<char> s else
            failwith "String must be exactly one character long."

    [<Inline "String.fromCharCode($x.charCodeAt(0) + $y.charCodeAt(0))">]
    static member (+) (x: char, y: char) : char = x + y

    [<Inline "$this">]
    member this.ToString() = X<string>
