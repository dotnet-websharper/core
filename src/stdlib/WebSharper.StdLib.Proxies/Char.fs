// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
[<Type "string">]
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
    override this.Equals(s: obj) = X<bool>

    [<Inline>]
    override this.GetHashCode() = hash this

    static member GetNumericValue(c: char) : float =
        if c >= '0' && c <= '9' then float c - float '0' else -1.

    [<Inline>]
    static member GetNumericValue(s: string, i: int) = CharProxy.GetNumericValue(s.[i])

    static member IsControl(c: char) : bool =
        c >= '\u0000' && c <= '\u001f'
        || c >= '\u0080' && c <= '\u009f'

    [<Inline>]
    static member IsControl(s: string, i: int) = CharProxy.IsControl(s.[i])

    [<Inline "/\\p{Nd}/u.test($c)">]
    static member IsDigit(c: char) = X<bool>

    [<Inline>]
    static member IsDigit(s: string, i: int) = CharProxy.IsDigit(s.[i])

    [<Inline "/\\p{L}/u.test($c)">]
    static member IsLetter(c: char) = X<bool>

    [<Inline>]
    static member IsLetter(s: string, i: int) = CharProxy.IsLetter(s.[i])

    static member IsLetterOrDigit(c: char) : bool =
        System.Char.IsLetter c || System.Char.IsDigit c

    [<Inline>]
    static member IsLetterOrDigit(s: string, i: int) = CharProxy.IsLetterOrDigit(s.[i])

    [<Inline "/\\p{Ll}/u.test($c)">]
    static member IsLower(c: char) = X<bool>

    [<Inline>]
    static member IsLower(s: string, i: int) = CharProxy.IsLower(s.[i])

    [<Inline "/\\p{Lu}/u.test($c)">]
    static member IsUpper(c: char) = X<bool>

    [<Inline>]
    static member IsUpper(s: string, i: int) = CharProxy.IsUpper(s.[i])

    [<Direct @"$c.match(/\s/) !== null">]
    static member IsWhiteSpace(c: char) = X<bool>

    [<Inline>]
    static member IsWhiteSpace(s: string, i: int) = CharProxy.IsWhiteSpace(s.[i])

    /// A character whose upper case is more than one character (such as 'ß') stays unchanged, as in .NET.
    static member ToUpper(c: char) : char =
        let u : string = JS.Inline("$0.toUpperCase()", c)
        if u.Length = 1 then As<char> u else c

    static member ToLower(c: char) : char =
        let l : string = JS.Inline("$0.toLowerCase()", c)
        if l.Length = 1 then As<char> l else c

    [<Inline>]
    static member ToUpperInvariant(c: char) = CharProxy.ToUpper c

    [<Inline>]
    static member ToLowerInvariant(c: char) = CharProxy.ToLower c

    static member Parse(s: string) =
        if s.Length = 1 then As<char> s else
            failwith "String must be exactly one character long."

    [<Inline "String.fromCharCode($x.charCodeAt() + $y.charCodeAt())">]
    static member (+) (x: char, y: char) : char = x + y

    [<Inline "$this">]
    override this.ToString() = X<string>
