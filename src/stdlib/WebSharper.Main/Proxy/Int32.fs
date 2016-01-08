// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<Proxy(typeof<int>)>]
type private Int32Proxy =

    [<Inline "parseInt($s)">]
    static member Parse(s: string) = X<int>

    [<Inline "$a + $b">]
    static member (+) (a: int, b: int) = X<int>

    [<Inline "$a - $b">]
    static member (-) (a: int, b: int) = X<int>

    [<Inline "$a * $b">]
    static member (*) (a: int, b: int) = X<int>

    [<Inline "$a / $b >> 0">]
    static member (/) (a: int, b: int) = X<int>

    [<Inline "$a % $b">]
    static member (%) (a: int, b: int) = X<int>

    [<Inline "$a | $b">]
    static member (|||) (a: int, b: int) = X<int>

    [<Inline "$a & $b">]
    static member (&&&) (a: int, b: int) = X<int>

    [<Inline "$a ^ $b">]
    static member (^^^) (a: int, b: int) = X<int>

    [<Inline "$a >> $b">]
    static member (>>>) (a: int, b: int) = X<int>

    [<Inline "$a << $b">]
    static member (<<<) (a: int, b: int) = X<int>

    [<Inline "-$a">]
    static member (~-) (a: int) = X<int>

    [<Inline "+$a">]
    static member (~+) (a: int) = X<int>

    [<Inline "$a == $b">]
    static member op_Equality (a: int, b: int) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality (a: int, b: int) = X<bool>

    [<Inline "$a > $b">]
    static member op_GreaterThan (a: int, b: int) = X<bool>

    [<Inline "$a >= $b">]
    static member op_GreaterThanOrEqual (a: int, b: int) = X<bool>

    [<Inline "$a < $b">]
    static member op_LessThan (a: int, b: int) = X<bool>

    [<Inline "$a <= $b">]
    static member op_LessThanOrEqual (a: int, b: int) = X<bool>
    
    static member MaxValue
        with [<Inline "-2147483648">] get () = X<int>

    static member MinValue
        with [<Inline "2147483647">] get () = X<int>

