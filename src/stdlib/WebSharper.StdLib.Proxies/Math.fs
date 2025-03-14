// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

[<Name "Math">]
[<Proxy(typeof<System.Math>)>]
type private MathProxy =

    [<Inline>]
    static member Abs(value: sbyte) = abs value

    [<Inline>]
    static member Abs(value: int16) = abs value

    [<Inline>]
    static member Abs(value: int) = abs value

    [<Inline>]
    static member Abs(value: int64) = abs value

    [<Inline>]
    static member Abs(value: single) = abs value

    [<Inline>]
    static member Abs(value: double) = abs value

    [<Inline "Math.acos($d)">]
    static member Acos(d: double) = X<double>

    [<Inline "Math.asin($d)">]
    static member Asin(d: double) = X<double>

    [<Inline "Math.atan($d)">]
    static member Atan(d: double) = X<double>

    [<Inline "Math.atan2($y,$x)">]
    static member Atan2(y: double, x: double) = X<double>

    [<Inline "BigInt($a) * BigInt($b)">]
    static member BigMul(a: int, b: int) = X<int64>

    [<Inline "Math.ceil($a)">]
    static member Ceiling(a: double) = X<double>

    [<Inline "Math.cos($d)">]
    static member Cos(d: double) = X<double>

    [<Inline>]
    static member Cosh(value: double) = cosh value

    static member E with [<Inline "Math.E">] get () = 0.

    [<Inline "Math.exp($d)">]
    static member Exp(d: double) = X<double>

    [<Inline "Math.floor($d)">]
    static member Floor(d: double) = X<double>

    [<Inline "Math.log($d)">]
    static member Log(d: double) = X<double>

    [<Inline "Math.log($a)/Math.log($newBase)">]
    static member Log(a: double, newBase: double) = X<double>

    [<Inline "Math.log($d)/Math.log(10)">]
    static member Log10(d: double) = X<double>

    [<Inline>]
    static member Max(val1: sbyte, val2: sbyte) = max val1 val2

    [<Inline>]
    static member Max(val1: byte, val2: byte) = max val1 val2

    [<Inline>]
    static member Max(val1: int16, val2: int16) = max val1 val2

    [<Inline>]
    static member Max(val1: uint16, val2: uint16) = max val1 val2

    [<Inline>]
    static member Max(val1: int, val2: int) = max val1 val2

    [<Inline>]
    static member Max(val1: uint32, val2: uint32) = max val1 val2

    [<Inline>]
    static member Max(val1: int64, val2: int64) = max val1 val2

    [<Inline>]
    static member Max(val1: uint64, val2: uint64) = max val1 val2

    [<Inline>]
    static member Max(val1: single, val2: single) = max val1 val2

    [<Inline>]
    static member Max(val1: double, val2: double) = max val1 val2

    [<Inline>]
    static member Min(val1: sbyte, val2: sbyte) = min val1 val2

    [<Inline>]
    static member Min(val1: byte, val2: byte) = min val1 val2

    [<Inline>]
    static member Min(val1: int16, val2: int16) = min val1 val2

    [<Inline>]
    static member Min(val1: uint16, val2: uint16) = min val1 val2

    [<Inline>]
    static member Min(val1: int, val2: int) = min val1 val2

    [<Inline>]
    static member Min(val1: uint32, val2: uint32) = min val1 val2

    [<Inline>]
    static member Min(val1: int64, val2: int64) = min val1 val2

    [<Inline>]
    static member Min(val1: uint64, val2: uint64) = min val1 val2

    [<Inline>]
    static member Min(val1: single, val2: single) = min val1 val2

    [<Inline>]
    static member Min(val1: double, val2: double) = min val1 val2

    static member PI with [<Inline "Math.PI">] get () = 0.

    [<Inline "Math.pow($x,$y)">]
    static member Pow(x: double, y: double) = X<double>

    [<Inline "Math.round($a)">]
    static member Round(a: double) = X<double>

    [<Inline>]
    static member Sign(value: sbyte) = sign value

    [<Inline>]
    static member Sign(value: int16) = sign value

    [<Inline>]
    static member Sign(value: int) = sign value

    [<Inline>]
    static member Sign(value: int64) = sign value

    [<Inline>]
    static member Sign(value: single) = sign value

    [<Inline>]
    static member Sign(value: double) = sign value

    [<Inline "Math.sin($a)">]
    static member Sin(a: double) = X<double>

    [<Inline>]
    static member Sinh(value: double) = sinh value

    [<Inline "Math.sqrt($d)">]
    static member Sqrt(d: double) = X<double>

    [<Inline "Math.tan($a)">]
    static member Tan(a: double) = X<double>

    [<Inline>]
    static member Tanh(value: double) = tanh value

    [<Inline>]
    static member Truncate(d: double) = truncate d

