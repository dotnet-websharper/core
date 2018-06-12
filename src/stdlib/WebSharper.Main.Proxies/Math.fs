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

module M = WebSharper.Core.Macros

[<Name "Math">]
[<Proxy(typeof<System.Math>)>]
type private MathProxy =

    [<Inline "Math.abs($value)">]
    static member Abs(value: sbyte) = X<sbyte>

    [<Inline "Math.abs($value)">]
    static member Abs(value: int16) = X<int16>

    [<Inline "Math.abs($value)">]
    static member Abs(value: int) = X<int>

    [<Inline "Math.abs($value)">]
    static member Abs(value: int64) = X<int64>

    [<Inline "Math.abs($value)">]
    static member Abs(value: single) = X<single>

    [<Inline "Math.abs($value)">]
    static member Abs(value: double) = X<double>

    [<Inline "Math.acos($d)">]
    static member Acos(d: double) = X<double>

    [<Inline "Math.asin($d)">]
    static member Asin(d: double) = X<double>

    [<Inline "Math.atan($d)">]
    static member Atan(d: double) = X<double>

    [<Inline "Math.atan2($y,$x)">]
    static member Atan2(y: double, x: double) = X<double>

    [<Inline "$a * $b">]
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

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: sbyte, val2: sbyte) = X<sbyte>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: byte, val2: byte) = X<byte>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: int16, val2: int16) = X<int16>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: uint16, val2: uint16) = X<uint16>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: int, val2: int) = X<int>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: uint32, val2: uint32) = X<uint32>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: int64, val2: int64) = X<int64>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: uint64, val2: uint64) = X<uint64>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: single, val2: single) = X<single>

    [<Inline "Math.max($val1,$val2)">]
    static member Max(val1: double, val2: double) = X<double>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: sbyte, val2: sbyte) = X<sbyte>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: byte, val2: byte) = X<byte>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: int16, val2: int16) = X<int16>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: uint16, val2: uint16) = X<uint16>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: int, val2: int) = X<int>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: uint32, val2: uint32) = X<uint32>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: int64, val2: int64) = X<int64>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: uint64, val2: uint64) = X<uint64>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: single, val2: single) = X<single>

    [<Inline "Math.min($val1,$val2)">]
    static member Min(val1: double, val2: double) = X<double>

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

